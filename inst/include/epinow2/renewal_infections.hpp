#ifndef EPINOW2_RENEWAL_INFECTIONS_HPP
#define EPINOW2_RENEWAL_INFECTIONS_HPP

/**
 * Renewal equation with a reverse-mode adjoint.
 *
 * Backs the Stan function
 * `renewal_infections(seed, R, gt_rev_pmf, pop, use_pop, pop_floor, nht)`
 * declared in inst/stan/functions/infections.stan, which is the loop of
 * generate_infections() after the seeding infections are set.
 *
 * Notation (1-based, as in the Stan code): the seeding infections are
 * I_1, ..., I_{t_s}, R_t (t = 1, ..., T) is the reproduction number,
 * g is the reversed generation time PMF of length G, and the generation
 * time weight of a gap of tau days is g_tau = gt_rev_pmf[G - tau].
 *
 * Forward, for t = 1, ..., T with u = t_s + t:
 *   lambda_t = sum_{tau = 1}^{min(G, u) - 1} g_tau I_{u - tau},
 * the total infectiousness (the tau = 0 term pairs with I_u, which is not
 * yet set). Without susceptible depletion
 *   I_u = R_t lambda_t.
 * With depletion (use_pop = 2, or use_pop = 1 and t > nht)
 *   S_t = fmax(pop_floor, pop - I^c_t),  a_t = R_t lambda_t / S_t,
 *   I_u = S_t fmax(0, 1 - exp(-a_t)),
 * where I^c_1 is the sum of the seeding infections and
 * I^c_{t + 1} = I^c_t + I_u.
 *
 * Reverse. Write xbar for the gradient of the target with respect to x.
 * Walk t = T, ..., 1. When step t is reached, Ibar_u already holds every
 * contribution from later times (through lambda and I^c). Then
 *   no depletion:  Rbar_t = Ibar_u lambda_t,  lambdabar_t = Ibar_u R_t;
 *   depletion:     with e_t = exp(-a_t),
 *                  abar_t = Ibar_u S_t e_t,
 *                  Rbar_t = abar_t lambda_t / S_t,
 *                  lambdabar_t = abar_t R_t / S_t,
 *                  Sbar_t = Ibar_u (1 - e_t) - abar_t a_t / S_t,
 *                  popbar += Sbar_t and I^cbar_t -= Sbar_t when
 *                  S_t = pop - I^c_t;
 *   then           Ibar_{u - tau} += lambdabar_t g_tau,
 *                  gbar_tau += lambdabar_t I_{u - tau},
 * and I^cbar_t is passed back to I_{u - 1} and I^cbar_{t - 1}. Finally
 * the seeding infections receive Ibar_{1:t_s} + I^cbar_1. The fmax()
 * branches follow Stan's fmax(): the second argument is taken, with its
 * gradient, when it is at least the first and not NaN.
 *
 * The forward pass and this reverse walk work on plain doubles, so the
 * whole renewal loop is one autodiff node, rather than one dot_product()
 * plus several scalar nodes per time step. Any of seed, R, gt_rev_pmf and
 * pop may be var or double; only the gradients of var inputs are
 * computed. As with convolve_with_rev_pmf(), the function lives in
 * namespace epinow2 and is made visible in stan::math.
 */

#include <stan/math.hpp>
#include <algorithm>
#include <cmath>
#include <ostream>
#include <stdexcept>

namespace epinow2 {
namespace internal {

// Forward values kept for the reverse pass. flag bits: 1 depletion,
// 2 S = pop - C (not the floor), 4 fmax(0, 1 - e) passes 1 - e through.
template <typename Vec>
struct renewal_state {
  Vec I, F, S, e, a, flag;
};

inline bool renewal_depletes(int use_pop, int s, int nht) {
  return (use_pop == 1 && s + 1 > nht) || use_pop == 2;
}

template <typename State>
inline void renewal_forward(const Eigen::VectorXd& seed,
                            const Eigen::VectorXd& R,
                            const Eigen::VectorXd& g, double pop,
                            int use_pop, double pop_floor, int nht,
                            State& st) {
  const int u0 = seed.size();
  const int ot = R.size();
  const int G = g.size();
  st.I = Eigen::VectorXd::Zero(u0 + ot);
  st.I.head(u0) = seed;
  st.F.resize(ot);
  st.S.resize(ot);
  st.e.resize(ot);
  st.a.resize(ot);
  st.flag = Eigen::VectorXd::Zero(ot);
  double C = use_pop ? seed.sum() : 0.0;
  for (int s = 0; s < ot; ++s) {
    const int u = u0 + s;
    const int K = std::min(G, u + 1) - 1;
    const double F
        = K > 0 ? st.I.segment(u - K, K).dot(g.segment(G - 1 - K, K)) : 0.0;
    st.F(s) = F;
    double inf;
    if (renewal_depletes(use_pop, s, nht)) {
      const double sraw = pop - C;
      const bool s_active = sraw >= pop_floor;
      const double S = s_active ? sraw : pop_floor;
      const double a = R(s) * F / S;
      const double e = std::exp(-a);
      const double h = 1.0 - e;
      const bool h_active = h >= 0.0;
      st.flag(s) = 1 + 2 * s_active + 4 * h_active;
      st.S(s) = S;
      st.a(s) = a;
      st.e(s) = e;
      inf = S * (h_active ? h : 0.0);
    } else {
      inf = R(s) * F;
    }
    st.I(u) = inf;
    if (use_pop && s < ot - 1) {
      C += inf;
    }
  }
}

template <typename State>
inline void renewal_reverse(const State& st, const Eigen::VectorXd& R,
                            const Eigen::VectorXd& g, int use_pop,
                            const Eigen::VectorXd& Ibar_in,
                            Eigen::VectorXd& seedbar, Eigen::VectorXd& Rbar,
                            Eigen::VectorXd& gbar, double& popbar) {
  const int ot = R.size();
  const int G = g.size();
  const int u0 = st.I.size() - ot;
  Eigen::VectorXd Ibar = Ibar_in;
  double Cbar = 0.0;
  for (int s = ot - 1; s >= 0; --s) {
    const int u = u0 + s;
    if (use_pop && s < ot - 1) {
      Ibar(u) += Cbar;
    }
    const double ib = Ibar(u);
    double Fbar;
    const int flag = static_cast<int>(st.flag(s));
    if (flag & 1) {
      const double S = st.S(s);
      double Sbar = 0.0;
      if (flag & 4) {
        const double abar = ib * S * st.e(s);
        Sbar = ib * (1.0 - st.e(s)) - abar * st.a(s) / S;
        Rbar(s) += abar * st.F(s) / S;
        Fbar = abar * R(s) / S;
      } else {
        Fbar = 0.0;
      }
      if (flag & 2) {
        popbar += Sbar;
        Cbar -= Sbar;
      }
    } else {
      Rbar(s) += ib * st.F(s);
      Fbar = ib * R(s);
    }
    const int K = std::min(G, u + 1) - 1;
    if (K > 0) {
      Ibar.segment(u - K, K) += Fbar * g.segment(G - 1 - K, K);
      gbar.segment(G - 1 - K, K) += Fbar * st.I.segment(u - K, K);
    }
  }
  seedbar = Ibar.head(u0);
  if (use_pop) {
    seedbar.array() += Cbar;
  }
}

}  // namespace internal

/**
 * Run the renewal equation after the seeding infections.
 *
 * The last argument is the output stream of the Stan calling convention,
 * which is not used.
 *
 * @param seed Seeding infections, length t_s >= 1 (var or double).
 * @param R Reproduction numbers, length T (var or double).
 * @param g Reversed generation time PMF (var or double).
 * @param pop Initial susceptible population (var or double).
 * @param use_pop Population adjustment (0 none, 1 after nht, 2 all).
 * @param pop_floor Minimum susceptible population (double).
 * @param nht Number of time steps before the population adjustment starts
 *   when use_pop = 1.
 * @return Infections, length t_s + T, starting with the seeds.
 * @throws std::domain_error if seed is empty.
 */
template <typename T0, typename T1, typename T2, typename T3, typename T5,
          stan::require_all_eigen_col_vector_t<T0, T1, T2>* = nullptr,
          stan::require_all_stan_scalar_t<T3, T5>* = nullptr>
inline Eigen::Matrix<stan::return_type_t<T0, T1, T2, T3>, Eigen::Dynamic, 1>
renewal_infections(const T0& seed, const T1& R, const T2& g, const T3& pop,
                   const int& use_pop, const T5& pop_floor, const int& nht,
                   std::ostream* /* pstream__ */) {
  using stan::arena_t;
  using stan::math::var;
  using stan::math::value_of;
  constexpr bool seed_var = stan::is_var<stan::value_type_t<T0>>::value;
  constexpr bool R_var = stan::is_var<stan::value_type_t<T1>>::value;
  constexpr bool g_var = stan::is_var<stan::value_type_t<T2>>::value;
  constexpr bool pop_var = stan::is_var<T3>::value;
  if (seed.size() < 1) {
    throw std::domain_error("renewal_infections: seeding time must be >= 1");
  }
  const double pop_d = value_of(pop);
  const double floor_d = value_of(pop_floor);
  if constexpr (!seed_var && !R_var && !g_var && !pop_var) {
    internal::renewal_state<Eigen::VectorXd> st;
    internal::renewal_forward(value_of(seed), value_of(R), value_of(g), pop_d,
                              use_pop, floor_d, nht, st);
    return st.I;
  } else {
    arena_t<Eigen::Matrix<stan::value_type_t<T0>, -1, 1>> seed_a = seed;
    arena_t<Eigen::Matrix<stan::value_type_t<T1>, -1, 1>> R_a = R;
    arena_t<Eigen::Matrix<stan::value_type_t<T2>, -1, 1>> g_a = g;
    arena_t<Eigen::VectorXd> R_val = value_of(R_a);
    arena_t<Eigen::VectorXd> g_val = value_of(g_a);
    internal::renewal_state<Eigen::VectorXd> fwd;
    internal::renewal_forward(value_of(seed_a), R_val, g_val, pop_d, use_pop,
                              floor_d, nht, fwd);
    internal::renewal_state<arena_t<Eigen::VectorXd>> st{
        fwd.I, fwd.F, fwd.S, fwd.e, fwd.a, fwd.flag};
    const int n = fwd.I.size();
    arena_t<Eigen::Matrix<var, -1, 1>> res(n);
    for (int i = 0; i < n; ++i) {
      res.coeffRef(i) = var(fwd.I(i));
    }
    T3 pop_v = pop;
    stan::math::reverse_pass_callback([=]() mutable {
      Eigen::VectorXd seedbar;
      Eigen::VectorXd Rbar = Eigen::VectorXd::Zero(R_val.size());
      Eigen::VectorXd gbar = Eigen::VectorXd::Zero(g_val.size());
      double popbar = 0.0;
      internal::renewal_reverse(st, R_val, g_val, use_pop, res.adj(),
                                seedbar, Rbar, gbar, popbar);
      if constexpr (seed_var) {
        seed_a.adj() += seedbar;
      }
      if constexpr (R_var) {
        R_a.adj() += Rbar;
      }
      if constexpr (g_var) {
        g_a.adj() += gbar;
      }
      if constexpr (pop_var) {
        pop_v.adj() += popbar;
      }
    });
    return Eigen::Matrix<var, -1, 1>(res);
  }
}

}  // namespace epinow2

namespace stan {
namespace math {
using ::epinow2::renewal_infections;
}  // namespace math
}  // namespace stan

#endif
