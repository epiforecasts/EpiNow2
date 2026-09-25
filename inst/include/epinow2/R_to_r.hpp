#ifndef EPINOW2_R_TO_R_HPP
#define EPINOW2_R_TO_R_HPP

/**
 * Growth rate from the reproduction number with a reverse-mode adjoint.
 *
 * Backs the Stan function `R_to_r(R, gt_rev_pmf, abs_tol)` declared in
 * inst/stan/functions/rt.stan.
 *
 * Notation: p is the generation time PMF, the reverse of gt_rev_pmf, with
 * p_k the weight of a gap of k = 0, ..., G - 1 days. The growth rate r is
 * the root of
 *   f(r; R, p) = R sum_k p_k exp(-r k) - 1.
 *
 * Forward. Newton's method on doubles, as in the Stan version it replaces:
 * start at r = fmax((R - 1) / (R m), -1), with m = sum_k k p_k the mean
 * generation time, and take steps f / f_r until a step is no larger than
 * abs_tol in absolute value. The value returned is r after the last step.
 * This code is based on Julia code from
 * https://github.com/CDCgov/Rt-without-renewal/blob/d6344cc6e451e3e6c4188e4984247f890ae60795/EpiAware/test/predictive_checking/fast_approx_for_r.jl
 * under Apache license 2.0.
 *
 * Reverse. By the implicit function theorem, at the returned r
 *   dr/dR   = -f_R / f_r   = S0 / (R S1),
 *   dr/dp_k = -f_p_k / f_r = exp(-r k) / S1,
 * with S0 = sum_k p_k exp(-r k), S1 = sum_k k p_k exp(-r k) and
 * f_r = -R S1. So rbar is passed back as Rbar += rbar S0 / (R S1) and
 * gt_rev_pmfbar[G - 1 - k] += rbar exp(-r k) / S1.
 *
 * The Stan version differentiated through the Newton steps, which gives the
 * derivative of the last iterate rather than of the root. The two agree to
 * within the solver tolerance. The implicit function theorem gradient is
 * exact at the returned value and does not record the steps on the tape.
 *
 * When all the generation time mass is at zero (G = 1, for example) f_r is
 * zero, the Newton step is not finite and the value is NaN, as in the Stan
 * version. The gradient is then not finite either.
 *
 * Either of R and gt_rev_pmf may be var or double; only the gradients of
 * var inputs are computed. As with convolve_with_rev_pmf(), the function
 * lives in namespace epinow2 and is made visible in stan::math.
 */

#include <stan/math.hpp>
#include <cmath>
#include <ostream>

namespace epinow2 {
namespace internal {

// Sums S0 = sum_k p_k exp(-r k) and S1 = sum_k k p_k exp(-r k), with
// p_k = g[G - 1 - k]. If e is given, exp(-r k) is stored in e[G - 1 - k].
inline void R_to_r_sums(double r, const Eigen::VectorXd& g, double& s0,
                        double& s1, Eigen::VectorXd* e = nullptr) {
  const int G = g.size();
  s0 = 0.0;
  s1 = 0.0;
  for (int k = 0; k < G; ++k) {
    const double p = g(G - 1 - k);
    const double ek = std::exp(-r * k);
    s0 += p * ek;
    s1 += p * k * ek;
    if (e) {
      (*e)(G - 1 - k) = ek;
    }
  }
}

inline double R_to_r_newton(double R, const Eigen::VectorXd& g,
                            double abs_tol) {
  const int G = g.size();
  double mean_gt = 0.0;
  for (int k = 0; k < G; ++k) {
    mean_gt += g(G - 1 - k) * k;
  }
  double r = std::fmax((R - 1) / (R * mean_gt), -1.0);
  double step = abs_tol + 1;
  while (std::abs(step) > abs_tol) {
    double s0, s1;
    R_to_r_sums(r, g, s0, s1);
    step = (R * s0 - 1) / (-R * s1);
    r -= step;
  }
  return r;
}

}  // namespace internal

/**
 * Estimate the growth rate r from the reproduction number R.
 *
 * The last argument is the output stream of the Stan calling convention,
 * which is not used.
 *
 * @param R Reproduction number (var or double).
 * @param gt_rev_pmf Reversed generation time PMF (var or double).
 * @param abs_tol Absolute tolerance of the Newton solver (double).
 * @return The growth rate r.
 */
template <typename T0, typename T1, typename T2,
          stan::require_all_stan_scalar_t<T0, T2>* = nullptr,
          stan::require_eigen_col_vector_t<T1>* = nullptr>
inline stan::return_type_t<T0, T1> R_to_r(const T0& R, const T1& gt_rev_pmf,
                                          const T2& abs_tol,
                                          std::ostream* /* pstream__ */) {
  using stan::arena_t;
  using stan::math::value_of;
  using stan::math::var;
  constexpr bool R_var = stan::is_var<T0>::value;
  constexpr bool g_var = stan::is_var<stan::value_type_t<T1>>::value;
  const double R_d = value_of(R);
  if constexpr (!R_var && !g_var) {
    return internal::R_to_r_newton(R_d, value_of(gt_rev_pmf),
                                   value_of(abs_tol));
  } else {
    arena_t<Eigen::Matrix<stan::value_type_t<T1>, -1, 1>> g_a = gt_rev_pmf;
    arena_t<Eigen::VectorXd> g_val = value_of(g_a);
    const double r
        = internal::R_to_r_newton(R_d, g_val, value_of(abs_tol));
    var res(r);
    T0 R_v = R;
    stan::math::reverse_pass_callback([=]() mutable {
      const double rbar = res.adj();
      double s0, s1;
      Eigen::VectorXd e(g_var ? g_val.size() : 0);
      internal::R_to_r_sums(r, g_val, s0, s1, g_var ? &e : nullptr);
      if constexpr (R_var) {
        R_v.adj() += rbar * s0 / (R_d * s1);
      }
      if constexpr (g_var) {
        g_a.adj() += (rbar / s1) * e;
      }
    });
    return res;
  }
}

}  // namespace epinow2

namespace stan {
namespace math {
using ::epinow2::R_to_r;
}  // namespace math
}  // namespace stan

#endif
