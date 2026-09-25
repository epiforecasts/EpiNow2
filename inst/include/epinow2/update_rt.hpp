#ifndef EPINOW2_UPDATE_RT_HPP
#define EPINOW2_UPDATE_RT_HPP

/**
 * Reproduction number trajectory with a reverse-mode adjoint.
 *
 * Backs the Stan function
 * `update_Rt(t, R0, noise, bps, bp_effects, stationary, n_centre)`
 * declared in inst/stan/functions/rt.stan.
 *
 * Notation (1-based, as in the Stan code): t is the length of the output,
 * c = n_centre, n = num_elements(noise), m = num_elements(bp_effects),
 * e = bp_effects and 1[.] is the indicator function.
 *
 * Forward:
 *   log R_i = log(R0) + b_i + x_i,  R_i = exp(log R_i).
 * Breakpoints (m > 0): with p^0_1 = 0 and p^0_{k + 1} = sum_{j <= k} e_j,
 * p_i = p^0_{bps_i} and b_i = p_i - (1 / c) sum_{k <= c} p_k; otherwise
 * b_i = 0.
 * Gaussian process (n > 0), stationary: x_i = noise_i for i <= n and
 * x_i = noise_n for i > n. Non-stationary: q_i = sum_{j < i, j <= n}
 * noise_j (so q_1 = 0 and q is held at its last value after n + 1) and
 * x_i = q_i - (1 / c) sum_{k <= c} q_k. Otherwise x_i = 0.
 *
 * Reverse. Write ybar for the gradient of the target with respect to y,
 * g_i = Rbar_i R_i (the gradient with respect to log R_i) and
 * G = sum_i g_i. Then
 *   R0bar = G / R0.
 * Centring subtracts the mean over the first c positions, so the gradient
 * reaching the uncentred path is gc_i = g_i - 1[i <= c] G / c.
 * Non-stationary GP: q_i depends on noise_j for every i > j, so
 *   noisebar_j = sum_{i > j} gc_i
 *              = sum_{i > j} g_i - G max(0, c - j) / c,
 * a reverse cumulative sum of gc.
 * Stationary GP: noisebar_j = g_j for j < n and
 *   noisebar_n = sum_{i >= n} g_i,
 * the last noise term also receiving the held forecast.
 * Breakpoints: the path at position i is p^0_{bps_i}, so
 *   p^0bar_k = sum_{i : bps_i = k} gc_i,
 * and p^0_k depends on e_j for every k > j, so
 *   ebar_j = sum_{k > j} p^0bar_k,
 * a reverse cumulative sum over the breakpoint levels.
 *
 * Both passes run on doubles in one reverse_pass_callback, so the whole
 * function is one autodiff node rather than one per element and
 * operation. Any of R0, noise and bp_effects may be var or double; only
 * the gradients of var inputs are computed. As with the other functions
 * in inst/include/epinow2, it lives in namespace epinow2 and is made
 * visible in stan::math.
 */

#include <stan/math.hpp>
#include <cmath>
#include <ostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace epinow2 {
namespace internal {

// The same conditions under which the Stan implementation fails to index.
inline void check_update_rt(int t, int gp_n, int bp_n,
                            const std::vector<int>& bps, int stationary,
                            int n_centre) {
  const std::string fn = "update_Rt: ";
  const bool centred = bp_n > 0 || (gp_n > 0 && !stationary);
  if (centred && (n_centre < 1 || n_centre > t)) {
    throw std::out_of_range(fn + "n_centre must be between 1 and t");
  }
  if (gp_n > 0 && gp_n + (stationary ? 0 : 1) > t) {
    throw std::out_of_range(fn + "noise is too long for t");
  }
  if (bp_n > 0) {
    if (static_cast<int>(bps.size()) != t) {
      throw std::invalid_argument(fn + "bps must have length t");
    }
    for (int k : bps) {
      if (k < 1 || k > bp_n + 1) {
        throw std::out_of_range(
            fn + "bps must be between 1 and num_elements(bp_effects) + 1");
      }
    }
  }
}

// R on doubles, in the order of operations of the Stan code.
inline Eigen::VectorXd update_rt_forward(int t, double R0,
                                         const Eigen::VectorXd& noise,
                                         const std::vector<int>& bps,
                                         const Eigen::VectorXd& bp_effects,
                                         int stationary, int n_centre) {
  const int gp_n = noise.size();
  const int bp_n = bp_effects.size();
  Eigen::VectorXd logR = Eigen::VectorXd::Constant(t, std::log(R0));
  if (bp_n > 0) {
    Eigen::VectorXd bp0(bp_n + 1);
    bp0(0) = 0.0;
    double level = 0.0;
    for (int j = 0; j < bp_n; ++j) {
      level += bp_effects(j);
      bp0(j + 1) = level;
    }
    Eigen::VectorXd bp(t);
    for (int i = 0; i < t; ++i) {
      bp(i) = bp0(bps[i] - 1);
    }
    const double bp_mean = bp.head(n_centre).mean();
    if (gp_n == 0) {
      // R takes one value per breakpoint level, so exponentiate the levels
      // and expand rather than exponentiating every position.
      const Eigen::VectorXd level_R
          = (logR(0) + (bp0.array() - bp_mean)).exp().matrix();
      for (int i = 0; i < t; ++i) {
        logR(i) = level_R(bps[i] - 1);
      }
      return logR;
    }
    bp.array() -= bp_mean;
    logR += bp;
  }
  if (gp_n > 0) {
    Eigen::VectorXd gp(t);
    if (stationary) {
      gp.head(gp_n) = noise;
      gp.tail(t - gp_n).setConstant(noise(gp_n - 1));
    } else {
      double level = 0.0;
      gp(0) = 0.0;
      for (int i = 1; i < t; ++i) {
        if (i <= gp_n) {
          level += noise(i - 1);
        }
        gp(i) = level;
      }
      gp.array() -= gp.head(n_centre).mean();
    }
    logR += gp;
  }
  return logR.array().exp().matrix();
}

// Adds the gradients to each non-null output, given g = Rbar .* R.
inline void update_rt_reverse(const Eigen::VectorXd& g, double R0, int gp_n,
                              const int* bps, int bp_n, int stationary,
                              int n_centre, double* R0bar,
                              Eigen::VectorXd* noisebar,
                              Eigen::VectorXd* bpbar) {
  const int t = g.size();
  const double G = g.sum();
  const double shift = G / n_centre;
  if (R0bar) {
    *R0bar += G / R0;
  }
  if (noisebar && gp_n > 0) {
    if (stationary) {
      noisebar->head(gp_n - 1) += g.head(gp_n - 1);
      (*noisebar)(gp_n - 1) += g.tail(t - gp_n + 1).sum();
    } else {
      double acc = 0.0;
      for (int i = t - 1; i >= 1; --i) {
        acc += i < n_centre ? g(i) - shift : g(i);
        if (i <= gp_n) {
          (*noisebar)(i - 1) += acc;
        }
      }
    }
  }
  if (bpbar && bp_n > 0) {
    Eigen::VectorXd bp0bar = Eigen::VectorXd::Zero(bp_n + 1);
    for (int i = 0; i < t; ++i) {
      bp0bar(bps[i] - 1) += i < n_centre ? g(i) - shift : g(i);
    }
    double acc = 0.0;
    for (int k = bp_n; k >= 1; --k) {
      acc += bp0bar(k);
      (*bpbar)(k - 1) += acc;
    }
  }
}

}  // namespace internal

/**
 * Reproduction numbers from an intercept, breakpoints and a Gaussian
 * process.
 *
 * The last argument is the output stream of the Stan calling convention,
 * which is not used.
 *
 * @param t Length of the output.
 * @param R0 Reproduction number intercept (var or double).
 * @param noise Gaussian process noise (var or double).
 * @param bps Breakpoint index of each position, used when bp_effects is
 *   not empty.
 * @param bp_effects Breakpoint effects (var or double).
 * @param stationary Whether the Gaussian process is stationary.
 * @param n_centre Number of leading positions to centre over.
 * @return Reproduction numbers, length t.
 * @throws std::out_of_range or std::invalid_argument when an input would
 *   be indexed out of range.
 */
template <typename T0, typename T1, typename T2,
          stan::require_stan_scalar_t<T0>* = nullptr,
          stan::require_all_eigen_col_vector_t<T1, T2>* = nullptr>
inline Eigen::Matrix<stan::return_type_t<T0, T1, T2>, Eigen::Dynamic, 1>
update_Rt(const int& t, const T0& R0, const T1& noise,
          const std::vector<int>& bps, const T2& bp_effects,
          const int& stationary, const int& n_centre,
          std::ostream* /* pstream__ */) {
  using stan::arena_t;
  using stan::math::value_of;
  using stan::math::var;
  constexpr bool R0_var = stan::is_var<T0>::value;
  constexpr bool noise_var = stan::is_var<stan::value_type_t<T1>>::value;
  constexpr bool bp_var = stan::is_var<stan::value_type_t<T2>>::value;
  const int gp_n = noise.size();
  const int bp_n = bp_effects.size();
  internal::check_update_rt(t, gp_n, bp_n, bps, stationary, n_centre);
  const double R0_d = value_of(R0);
  if constexpr (!R0_var && !noise_var && !bp_var) {
    return internal::update_rt_forward(t, R0_d, value_of(noise), bps,
                                       value_of(bp_effects), stationary,
                                       n_centre);
  } else {
    arena_t<Eigen::Matrix<stan::value_type_t<T1>, -1, 1>> noise_a = noise;
    arena_t<Eigen::Matrix<stan::value_type_t<T2>, -1, 1>> bp_a = bp_effects;
    const Eigen::VectorXd R
        = internal::update_rt_forward(t, R0_d, value_of(noise_a), bps,
                                      value_of(bp_a), stationary, n_centre);
    arena_t<Eigen::Matrix<var, -1, 1>> res(t);
    for (int i = 0; i < t; ++i) {
      res.coeffRef(i) = var(R(i));
    }
    int* bps_a = nullptr;
    if (bp_var && bp_n > 0) {
      bps_a = stan::math::ChainableStack::instance_->memalloc_
                  .alloc_array<int>(t);
      std::copy(bps.begin(), bps.end(), bps_a);
    }
    T0 R0_v = R0;
    stan::math::reverse_pass_callback([=]() mutable {
      const Eigen::VectorXd g = res.adj().cwiseProduct(res.val());
      double R0bar = 0.0;
      Eigen::VectorXd noisebar;
      Eigen::VectorXd bpbar;
      if constexpr (noise_var) {
        noisebar = Eigen::VectorXd::Zero(gp_n);
      }
      if constexpr (bp_var) {
        bpbar = Eigen::VectorXd::Zero(bp_n);
      }
      internal::update_rt_reverse(g, R0_d, gp_n, bps_a, bp_n, stationary,
                                  n_centre, R0_var ? &R0bar : nullptr,
                                  noise_var ? &noisebar : nullptr,
                                  bp_var ? &bpbar : nullptr);
      if constexpr (R0_var) {
        R0_v.adj() += R0bar;
      }
      if constexpr (noise_var) {
        noise_a.adj() += noisebar;
      }
      if constexpr (bp_var) {
        bp_a.adj() += bpbar;
      }
    });
    return Eigen::Matrix<var, -1, 1>(res);
  }
}

}  // namespace epinow2

namespace stan {
namespace math {
using ::epinow2::update_Rt;
}  // namespace math
}  // namespace stan

#endif
