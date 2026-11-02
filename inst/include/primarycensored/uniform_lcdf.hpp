#ifndef PRIMARYCENSORED_UNIFORM_LCDF_HPP
#define PRIMARYCENSORED_UNIFORM_LCDF_HPP

/**
 * Uniform primary censored log CDFs at integer delays with a reverse-mode
 * adjoint, for lognormal, gamma and Weibull delays.
 *
 * Implements the Stan function
 *   primarycensored_uniform_lcdf_vectorized(start, n, dist_id, params,
 *                                           pwindow)
 * declared in inst/stan/functions/primarycensored_uniform_lcdf.stan, where
 * its Stan body is the reference and the fallback.
 *
 * Forward. For each integer t from max(start - w, 0) to n, with w =
 * pwindow, compute the two uniform primary terms
 *   [log(t F_T(t)), log M(t)],
 * where M(t) = int_0^t s f_T(s) ds is the partial expectation, exactly as
 * primarycensored_uniform_terms() does in Stan. For each d from start to n,
 * with q = max(d - w, 0), combine them as
 * primarycensored_uniform_lcdf_from_terms() does:
 *   log A = log_sum_exp(log(d F_T(d)), log M(q)),
 *   log B = log_sum_exp(log(q F_T(q)), log M(d)),
 *   lcdf  = log_diff_exp(log A, log B) - log w,
 * returning -inf when A and B both underflow and 0 when rounding puts lcdf
 * above 0.
 *
 * Reverse. A - B = G(d) - G(q), with G(t) = t F_T(t) - M(t) = E[(t - T)^+].
 * Writing T = T(theta, U) for a base variable U that does not depend on the
 * parameters gives dG/dtheta = -E[dT/dtheta 1{T < t}], so
 *   d lcdf / dtheta = (dG/dtheta(d) - dG/dtheta(q)) / (A - B).
 * Lognormal (mu, sigma), z = (log t - mu) / sigma:
 *   dG/dmu = -M,  dG/dsigma = -sigma M + t phi(z).
 * Gamma (k, beta), y = beta t, h = y^k e^{-y} / Gamma(k + 1),
 * P_a = dP/da for the regularised lower incomplete gamma P:
 *   dG/dbeta = M / beta,
 *   dG/dk = t P_a(k, y) - P(k + 1, y) / beta - (k / beta) P_a(k + 1, y),
 *   P_a(k + 1, y) = P_a(k, y) - h (log y - psi(k + 1)).
 * Weibull (k, lambda), x = (t / lambda)^k, a = 1 + 1 / k:
 *   dG/dlambda = -M / lambda,
 *   dG/dk = lambda Gamma(a) (P_a(a, x) + P(a, x) psi(a)) / k^2.
 * The gradient of an element that is -inf or clamped to 0 is 0, as in
 * Stan. Only params can be var, so the whole vector is one autodiff node
 * whose reverse pass is two dot products.
 *
 * P_a uses Eigen's igamma_der_a (Eigen 3.4 or later, in the unsupported
 * SpecialFunctions module), which is accurate to about 1e-15. With an older
 * Eigen, or when PRIMARYCENSORED_STAN_DPDA is defined, Stan Math's
 * grad_reg_lower_inc_gamma() is used instead.
 */

#include <stan/math.hpp>
#include <algorithm>
#include <cmath>
#include <limits>
#include <ostream>
#include <stdexcept>
#include <vector>

#if !defined(PRIMARYCENSORED_STAN_DPDA) && \
    defined(EIGEN_VERSION_AT_LEAST) && EIGEN_VERSION_AT_LEAST(3, 4, 0)
#include <unsupported/Eigen/SpecialFunctions>
#define PRIMARYCENSORED_EIGEN_DPDA 1
#endif

namespace primarycensored {
namespace internal {

constexpr double neg_inf = -std::numeric_limits<double>::infinity();

/**
 * Derivative of the regularised lower incomplete gamma with respect to a.
 *
 * @param a Shape.
 * @param x Argument.
 * @return dP(a, x) / da.
 */
inline double dP_da(double a, double x) {
#ifdef PRIMARYCENSORED_EIGEN_DPDA
  return Eigen::internal::igamma_der_a_impl<double>::run(a, x);
#else
  return stan::math::grad_reg_lower_inc_gamma(a, x);
#endif
}

/**
 * Log CDF of a unit rate gamma from log(x), as gamma_lcdf_logx() in Stan.
 *
 * Sums the lower series below x = (a + 1) / 2 and calls gamma_lcdf()
 * elsewhere.
 *
 * @param log_x Log of the argument.
 * @param a Shape, positive and finite.
 * @return log P(a, exp(log_x)).
 */
inline double gamma_lcdf_logx(double log_x, double a) {
  if (!(a > 0) || std::isinf(a)) {
    throw std::domain_error(
        "gamma_lcdf_logx: shape must be positive finite");
  }
  const double x = std::exp(log_x);
  if (!(x < 0.5 * (a + 1))) {
    return stan::math::gamma_lcdf(x, a, 1.0);
  }
  double term = 1;
  double total = 1;
  double n = a;
  while (term > 1e-16 * total) {
    n += 1;
    term *= x / n;
    total += term;
  }
  return a * log_x - x - stan::math::lgamma(a + 1) + std::log(total);
}

/**
 * Terms and gradient pieces at one integer delay.
 *
 * log_tF and log_M are the two Stan terms. dG1 and dG2 are dG/dtheta for
 * the two parameters, except for lognormal, which keeps log_tphi and forms
 * the gradient on the log scale in scaled_dG().
 */
struct Node {
  double log_tF = neg_inf;
  double log_M = neg_inf;
  double log_tphi = neg_inf;
  double dG1 = 0;
  double dG2 = 0;
};

/**
 * Lognormal terms at t, as primarycensored_lognormal_uniform_terms().
 *
 * @param t Delay, an integer.
 * @param mu Location on the log scale.
 * @param sigma Scale on the log scale.
 * @return The node at t.
 */
inline Node lognormal_node(double t, double mu, double sigma) {
  Node nd;
  if (t <= 0) {
    return nd;
  }
  const double log_t = std::log(t);
  const double s2 = sigma * sigma;
  const double z = (log_t - mu) / sigma;
  if (!(z < -38)) {
    nd.log_tF = log_t + stan::math::lognormal_lcdf(t, mu, sigma);
    nd.log_tphi = log_t - 0.5 * z * z - 0.5 * stan::math::LOG_TWO_PI;
  }
  if (!((log_t - (mu + s2)) / sigma < -38)) {
    nd.log_M = mu + 0.5 * s2 + stan::math::lognormal_lcdf(t, mu + s2, sigma);
  }
  return nd;
}

/**
 * Gamma terms at t, as primarycensored_gamma_uniform_terms().
 *
 * @param t Delay, an integer.
 * @param k Shape.
 * @param beta Rate, positive and finite.
 * @param grad Whether to compute dG1 and dG2.
 * @return The node at t.
 */
inline Node gamma_node(double t, double k, double beta, bool grad) {
  Node nd;
  if (t <= 0) {
    return nd;
  }
  if (!(beta > 0) || std::isinf(beta)) {
    throw std::domain_error("Gamma rate must be positive finite");
  }
  const double log_beta = std::log(beta);
  const double log_t = std::log(t);
  const double log_y = log_t + log_beta;
  const double log_P_k1 = gamma_lcdf_logx(log_y, k + 1);
  const double log_h = k * log_y - std::exp(log_y) - stan::math::lgamma(k + 1);
  nd.log_tF = log_t + stan::math::log_sum_exp(log_P_k1, log_h);
  nd.log_M = std::log(k) - log_beta + log_P_k1;
  if (grad) {
    const double y = std::exp(log_y);
    const double h = std::exp(log_h);
    const double P_k1 = std::exp(log_P_k1);
    const double dP_k = dP_da(k, y);
    const double dP_k1 = dP_k - h * (log_y - stan::math::digamma(k + 1));
    nd.dG1 = t * dP_k - P_k1 / beta - k / beta * dP_k1;
    nd.dG2 = std::exp(nd.log_M) / beta;
  }
  return nd;
}

/**
 * Weibull terms at t, as primarycensored_weibull_uniform_terms().
 *
 * @param t Delay, an integer.
 * @param k Shape.
 * @param lambda Scale.
 * @param grad Whether to compute dG1 and dG2.
 * @return The node at t.
 */
inline Node weibull_node(double t, double k, double lambda, bool grad) {
  Node nd;
  if (t <= 0) {
    return nd;
  }
  const double x = std::pow(t / lambda, k);
  const double a = 1 + 1 / k;
  const double log_P_a = stan::math::gamma_lcdf(x, a, 1.0);
  nd.log_tF = std::log(t) + stan::math::weibull_lcdf(t, k, lambda);
  nd.log_M = std::log(lambda) + log_P_a + stan::math::lgamma(a);
  if (grad) {
    const double lg = lambda * stan::math::tgamma(a);
    const double P_a = std::exp(log_P_a);
    nd.dG1 = lg * (dP_da(a, x) + P_a * stan::math::digamma(a)) / (k * k);
    nd.dG2 = -std::exp(nd.log_M) / lambda;
  }
  return nd;
}

/**
 * Node at t for the given delay distribution.
 *
 * @param dist_id 1 (lognormal), 2 (gamma) or 3 (Weibull).
 * @param t Delay, an integer.
 * @param p1 First parameter.
 * @param p2 Second parameter.
 * @param grad Whether gradient pieces are needed.
 * @return The node at t.
 */
inline Node node(int dist_id, double t, double p1, double p2, bool grad) {
  if (dist_id == 1) {
    return lognormal_node(t, p1, p2);
  } else if (dist_id == 2) {
    return gamma_node(t, p1, p2, grad);
  }
  return weibull_node(t, p1, p2, grad);
}

/**
 * dG/dtheta at a node divided by A - B, with log_den = log(A - B).
 *
 * @param dist_id Delay distribution.
 * @param nd The node.
 * @param sigma Lognormal scale (unused otherwise).
 * @param log_den log(A - B).
 * @param g1 Set to dG/dtheta_1 / (A - B).
 * @param g2 Set to dG/dtheta_2 / (A - B).
 */
inline void scaled_dG(int dist_id, const Node& nd, double sigma,
                      double log_den, double& g1, double& g2) {
  if (dist_id == 1) {
    const double M = std::exp(nd.log_M - log_den);
    g1 = -M;
    g2 = -sigma * M + std::exp(nd.log_tphi - log_den);
  } else {
    const double s = std::exp(-log_den);
    g1 = nd.dG1 * s;
    g2 = nd.dG2 * s;
  }
}

}  // namespace internal

/**
 * Uniform primary censored log CDFs at integer delays start to n.
 *
 * The last argument is the output stream of the Stan calling convention,
 * which is not used.
 *
 * @param start First delay to compute (at least 1).
 * @param n Last delay to compute, and the length of the result.
 * @param dist_id 1 (lognormal), 2 (gamma) or 3 (Weibull).
 * @param params The two delay parameters (var or double).
 * @param pwindow Primary event window, a positive integer.
 * @return Vector whose element d is the log CDF at d, for d in start:n.
 * Elements before start are NaN.
 */
template <typename T, stan::require_stan_scalar_t<T>* = nullptr>
inline Eigen::Matrix<T, -1, 1> primarycensored_uniform_lcdf_vectorized(
    const int& start, const int& n, const int& dist_id,
    const std::vector<T>& params, const double& pwindow,
    std::ostream* /* pstream__ */) {
  using stan::math::var;
  constexpr bool p_var = stan::is_var<T>::value;
  if (dist_id < 1 || dist_id > 3) {
    throw std::domain_error(
        "primarycensored_uniform_lcdf_vectorized: dist_id must be 1, 2 or 3");
  }
  if (params.size() != 2) {
    throw std::domain_error(
        "primarycensored_uniform_lcdf_vectorized: params must have length 2");
  }
  const double p1 = stan::math::value_of(params[0]);
  const double p2 = stan::math::value_of(params[1]);
  const int pw = static_cast<int>(pwindow);
  const double log_w = std::log(pwindow);
  const int first = std::max(start - pw, 0);
  std::vector<internal::Node> nodes(n - first + 1);
  for (int t = first; t <= n; ++t) {
    nodes[t - first] = internal::node(dist_id, t, p1, p2, p_var);
  }
  Eigen::VectorXd val = Eigen::VectorXd::Constant(
      n, std::numeric_limits<double>::quiet_NaN());
  Eigen::VectorXd g1, g2;
  if (p_var) {
    g1 = Eigen::VectorXd::Zero(n);
    g2 = Eigen::VectorXd::Zero(n);
  }
  for (int d = start; d <= n; ++d) {
    const int q = std::max(d - pw, 0);
    const internal::Node& nd = nodes[d - first];
    const internal::Node& nq = nodes[q - first];
    const double log_A = stan::math::log_sum_exp(nd.log_tF, nq.log_M);
    const double log_B = stan::math::log_sum_exp(nq.log_tF, nd.log_M);
    if (log_A == internal::neg_inf && log_B == internal::neg_inf) {
      val(d - 1) = internal::neg_inf;
      continue;
    }
    const double log_den = stan::math::log_diff_exp(log_A, log_B);
    const double log_cdf = log_den - log_w;
    if (log_cdf > 0) {
      val(d - 1) = 0;
      continue;
    }
    val(d - 1) = log_cdf;
    if (p_var && std::isfinite(log_den)) {
      double d1, d2;
      internal::scaled_dG(dist_id, nd, p2, log_den, d1, d2);
      double q1 = 0, q2 = 0;
      if (q > 0) {
        internal::scaled_dG(dist_id, nq, p2, log_den, q1, q2);
      }
      g1(d - 1) = d1 - q1;
      g2(d - 1) = d2 - q2;
    }
  }
  if constexpr (!p_var) {
    return val;
  } else {
    stan::arena_t<Eigen::Matrix<var, -1, 1>> res = val.template cast<var>();
    stan::arena_t<Eigen::VectorXd> g1_a = g1;
    stan::arena_t<Eigen::VectorXd> g2_a = g2;
    var p1_v = params[0];
    var p2_v = params[1];
    stan::math::reverse_pass_callback([res, g1_a, g2_a, p1_v, p2_v]() mutable {
      p1_v.adj() += res.adj().dot(g1_a);
      p2_v.adj() += res.adj().dot(g2_a);
    });
    return res;
  }
}

}  // namespace primarycensored

namespace stan {
namespace math {
using ::primarycensored::primarycensored_uniform_lcdf_vectorized;
}  // namespace math
}  // namespace stan

#endif
