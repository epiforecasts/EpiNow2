#ifndef PRIMARYCENSORED_UNIFORM_LCDF_HPP
#define PRIMARYCENSORED_UNIFORM_LCDF_HPP

/**
 * C++ versions, with closed-form gradients, of the primarycensored Stan
 * functions primarycensored_uniform_lcdf() and
 * primarycensored_uniform_lcdf_vectorized(), for lognormal, gamma and
 * Weibull delays with a uniform primary.
 *
 * Their Stan bodies, in inst/stan/functions/primarycensored_analytical_cdf.stan
 * of the primarycensored package, are the reference. Values match the Stan
 * code.
 *
 * With G(t) = t F_T(t) - M(t) = E[(t - T)^+], where M is the partial
 * expectation, the censored CDF is (G(d) - G(q)) / pwindow with
 * q = max(d - pwindow, 0). Writing T = T(theta, U) for a base variable U
 * gives dG/dtheta = -E[dT/dtheta 1{T < t}], so
 *   d lcdf / dtheta = (dG/dtheta(d) - dG/dtheta(q)) / (G(d) - G(q)).
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
 * Each function is one autodiff node.
 *
 * The functions are put in stan::math. For var arguments argument-dependent
 * lookup also finds them, so they are used even if the Stan bodies are
 * compiled in alongside this header.
 *
 * P_a uses Eigen's igamma_der_a when Eigen is 3.4 or later (the #if below).
 * Otherwise, or if PRIMARYCENSORED_STAN_DPDA is defined, it is found by
 * nested autodiff (see dP_da()).
 */

#include <stan/math.hpp>
#include <algorithm>
#include <cmath>
#include <limits>
#include <ostream>
#include <stdexcept>
#include <string>
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
 * Log CDF of a unit rate gamma from log(x), as gamma_lcdf_logx() in Stan.
 *
 * Sums the lower series below x = (a + 1) / 2 and calls gamma_lcdf()
 * elsewhere. The shape may be a var, which the dP/da fallback uses.
 *
 * @param log_x Log of the argument.
 * @param a Shape, positive and finite.
 * @return log P(a, exp(log_x)).
 */
template <typename T>
inline T gamma_lcdf_logx(double log_x, const T& a) {
  const double a_val = stan::math::value_of(a);
  if (!(a_val > 0) || std::isinf(a_val)) {
    throw std::domain_error(
        "gamma_lcdf_logx: shape must be positive finite");
  }
  const double x = std::exp(log_x);
  if (!(x < 0.5 * (a_val + 1))) {
    return stan::math::gamma_lcdf(x, a, 1.0);
  }
  T term = 1;
  T total = 1;
  T n = a;
  while (term > 1e-16 * total) {
    n += 1;
    term *= x / n;
    total += term;
  }
  return a * log_x - x - stan::math::lgamma(a + 1) + stan::math::log(total);
}

/**
 * Derivative of the regularised lower incomplete gamma with respect to a.
 *
 * Uses Eigen's igamma_der_a where available. Otherwise it differentiates
 * gamma_lcdf_logx() with nested autodiff, as Stan's autodiff through the
 * Stan code does. Stan Math's grad_reg_lower_inc_gamma() is not used, as it
 * is badly wrong for large shapes (for example a = 50).
 *
 * @param a Shape.
 * @param x Argument, positive.
 * @return dP(a, x) / da.
 */
inline double dP_da(double a, double x) {
#ifdef PRIMARYCENSORED_EIGEN_DPDA
  return Eigen::internal::igamma_der_a_impl<double>::run(a, x);
#else
  stan::math::nested_rev_autodiff nested;
  stan::math::var a_v = a;
  stan::math::var log_P = gamma_lcdf_logx(std::log(x), a_v);
  log_P.grad();
  return std::exp(log_P.val()) * a_v.adj();
#endif
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
  // As pow(t * inv(scale), shape) in Stan, so the values match exactly
  const double x = std::pow(t * (1 / lambda), k);
  const double a = 1 + 1 / k;
  const double log_P_a = stan::math::gamma_lcdf(x, a, 1.0);
  nd.log_tF = std::log(t) + stan::math::weibull_lcdf(t, k, lambda);
  nd.log_M = std::log(lambda) + (log_P_a + stan::math::lgamma(a));
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

/**
 * A log CDF (or log PMF) value and its gradient with respect to the two
 * delay parameters.
 */
struct Point {
  double val = neg_inf;
  double g1 = 0;
  double g2 = 0;
};

/**
 * Combine the nodes at d and q into the log CDF at d, as
 * primarycensored_uniform_lcdf_from_terms() does in Stan.
 *
 * @param dist_id Delay distribution.
 * @param nd Node at d.
 * @param nq Node at q = max(d - pwindow, 0).
 * @param q_pos Whether q > 0 (the node at q contributes to the gradient).
 * @param sigma Second parameter (the lognormal scale).
 * @param log_w log(pwindow).
 * @param grad Whether to compute the gradient.
 * @return The log CDF at d. Its gradient is 0 where it is -inf or clamped
 * to 0, as in Stan.
 */
inline Point combine(int dist_id, const Node& nd, const Node& nq, bool q_pos,
                     double sigma, double log_w, bool grad) {
  Point pt;
  const double log_A = stan::math::log_sum_exp(nd.log_tF, nq.log_M);
  const double log_B = stan::math::log_sum_exp(nq.log_tF, nd.log_M);
  if (log_A == neg_inf && log_B == neg_inf) {
    return pt;
  }
  const double log_den = stan::math::log_diff_exp(log_A, log_B);
  const double log_cdf = log_den - log_w;
  if (log_cdf > 0) {
    pt.val = 0;
    return pt;
  }
  pt.val = log_cdf;
  if (grad && std::isfinite(log_den)) {
    double d1, d2, q1 = 0, q2 = 0;
    scaled_dG(dist_id, nd, sigma, log_den, d1, d2);
    if (q_pos) {
      scaled_dG(dist_id, nq, sigma, log_den, q1, q2);
    }
    pt.g1 = d1 - q1;
    pt.g2 = d2 - q2;
  }
  return pt;
}

/**
 * Log CDF at a single delay d, with q = max(d - pwindow, 0).
 *
 * @param dist_id Delay distribution.
 * @param d Delay.
 * @param pwindow Primary event window.
 * @param p1 First parameter.
 * @param p2 Second parameter.
 * @param grad Whether to compute the gradient.
 * @return The log CDF at d.
 */
inline Point lcdf_at(int dist_id, double d, double pwindow, double p1,
                     double p2, bool grad) {
  const double q = std::max(d - pwindow, 0.0);
  const Node nd = node(dist_id, d, p1, p2, grad);
  const Node nq = node(dist_id, q, p1, p2, grad);
  return combine(dist_id, nd, nq, q > 0, p2, std::log(pwindow), grad);
}

/**
 * Log CDFs at the integer delays start to n, with the nodes shared between
 * d and q. Elements before start are NaN, as are all of them if start > n.
 *
 * @param start First delay.
 * @param n Last delay.
 * @param dist_id Delay distribution.
 * @param pw Primary event window, a positive integer.
 * @param p1 First parameter.
 * @param p2 Second parameter.
 * @param grad Whether to compute the gradient.
 * @return Vector of n points; element d - 1 is the log CDF at d.
 */
inline std::vector<Point> lcdf_at_integers(int start, int n, int dist_id,
                                           int pw, double p1, double p2,
                                           bool grad) {
  Point nan_pt;
  nan_pt.val = std::numeric_limits<double>::quiet_NaN();
  std::vector<Point> out(n, nan_pt);
  if (start > n) {
    return out;
  }
  const double log_w = std::log(static_cast<double>(pw));
  const int first = std::max(start - pw, 0);
  std::vector<Node> nodes(n - first + 1);
  for (int t = first; t <= n; ++t) {
    nodes[t - first] = node(dist_id, t, p1, p2, grad);
  }
  for (int d = start; d <= n; ++d) {
    const int q = std::max(d - pw, 0);
    out[d - 1] = combine(dist_id, nodes[d - first], nodes[q - first], q > 0,
                         p2, log_w, grad);
  }
  return out;
}

/**
 * Check the arguments shared by the exported functions.
 *
 * @param fn Function name for error messages.
 * @param dist_id Delay distribution.
 * @param n_params Number of parameters.
 */
inline void check_dist(const char* fn, int dist_id, std::size_t n_params) {
  if (dist_id < 1 || dist_id > 3) {
    throw std::domain_error(std::string(fn) + ": dist_id must be 1, 2 or 3");
  }
  if (n_params != 2) {
    throw std::domain_error(std::string(fn) + ": params must have length 2");
  }
}

/**
 * Check that pwindow is a positive integer and return it as an int.
 *
 * @param fn Function name for error messages.
 * @param pwindow Primary event window.
 * @return pwindow as an int.
 */
inline int check_integer_pwindow(const char* fn, double pwindow) {
  if (!(pwindow >= 1) || std::floor(pwindow) != pwindow) {
    throw std::domain_error(std::string(fn)
                            + ": pwindow must be a positive integer");
  }
  return static_cast<int>(pwindow);
}

/**
 * Return points as a vector of T. For var parameters this is one autodiff
 * node whose reverse pass adds the dot products of the adjoints with the
 * stored gradients.
 *
 * @param pts The points.
 * @param params The two parameters.
 * @return The values as a vector.
 */
template <typename T>
inline Eigen::Matrix<T, -1, 1> to_stan_vector(const std::vector<Point>& pts,
                                              const std::vector<T>& params) {
  const int n = pts.size();
  Eigen::VectorXd val(n);
  for (int i = 0; i < n; ++i) {
    val(i) = pts[i].val;
  }
  if constexpr (!stan::is_var<T>::value) {
    return val;
  } else {
    using stan::math::var;
    stan::arena_t<Eigen::VectorXd> g1(n), g2(n);
    for (int i = 0; i < n; ++i) {
      g1(i) = pts[i].g1;
      g2(i) = pts[i].g2;
    }
    stan::arena_t<Eigen::Matrix<var, -1, 1>> res = val.template cast<var>();
    var p1_v = params[0];
    var p2_v = params[1];
    stan::math::reverse_pass_callback([res, g1, g2, p1_v, p2_v]() mutable {
      p1_v.adj() += res.adj().dot(g1);
      p2_v.adj() += res.adj().dot(g2);
    });
    return res;
  }
}

}  // namespace internal

/**
 * Uniform primary censored log CDFs at integer delays start to n.
 *
 * The last argument is the output stream of the Stan calling convention,
 * which is not used.
 *
 * @param start First delay to compute, at least 1.
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
  constexpr const char* fn = "primarycensored_uniform_lcdf_vectorized";
  internal::check_dist(fn, dist_id, params.size());
  const int pw = internal::check_integer_pwindow(fn, pwindow);
  if (start < 1 || n < start) {
    throw std::domain_error(std::string(fn) + ": need 1 <= start <= n");
  }
  return internal::to_stan_vector(
      internal::lcdf_at_integers(start, n, dist_id, pw,
                                 stan::math::value_of(params[0]),
                                 stan::math::value_of(params[1]),
                                 stan::is_var<T>::value),
      params);
}

/**
 * Uniform primary censored log CDF at a single delay.
 *
 * @param d Delay.
 * @param dist_id 1 (lognormal), 2 (gamma) or 3 (Weibull).
 * @param params The two delay parameters (var or double).
 * @param pwindow Primary event window, positive.
 * @return The log CDF at d.
 */
template <typename T, stan::require_stan_scalar_t<T>* = nullptr>
inline T primarycensored_uniform_lcdf(const double& d, const int& dist_id,
                                      const std::vector<T>& params,
                                      const double& pwindow,
                                      std::ostream* /* pstream__ */) {
  internal::check_dist("primarycensored_uniform_lcdf", dist_id, params.size());
  const internal::Point pt = internal::lcdf_at(
      dist_id, d, pwindow, stan::math::value_of(params[0]),
      stan::math::value_of(params[1]), stan::is_var<T>::value);
  return internal::to_stan_vector(std::vector<internal::Point>{pt}, params)(0);
}

}  // namespace primarycensored

namespace stan {
namespace math {
using ::primarycensored::primarycensored_uniform_lcdf;
using ::primarycensored::primarycensored_uniform_lcdf_vectorized;
}  // namespace math
}  // namespace stan

#endif
