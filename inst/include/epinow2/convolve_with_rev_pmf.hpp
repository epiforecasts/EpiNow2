#ifndef EPINOW2_CONVOLVE_WITH_REV_PMF_HPP
#define EPINOW2_CONVOLVE_WITH_REV_PMF_HPP

/**
 * Convolution of a vector with a reversed PMF, with a reverse-mode adjoint.
 *
 * Implements `convolve_with_rev_pmf(x, y, len)` from
 * inst/stan/functions/convolve.stan.
 *
 * Notation (1-based, as in the Stan code): x has length n, y has length D
 * and is the reversed PMF, and z is the output of length len, with
 * n <= len <= n + D - 1. The weight of lag d (d = 0, ..., D - 1) is
 * w_d = y_{D - d}, and x_s = 0 outside 1, ..., n.
 *
 * Forward:
 *   z_t = sum_{d = 0}^{D - 1} w_d x_{t - d},  t = 1, ..., len.
 * This is evaluated lag by lag, one vector update per lag,
 *   z_{d+1:d+m} += w_d x_{1:m},  m = min(n, len - d),
 * which is z_{d+1:n} += w_d x_{1:n-d} when len = n. When len > n the
 * output runs past the end of x and m caps the update at the last input,
 * so the extra outputs are the tail of the full convolution.
 *
 * Reverse: the adjoint of a convolution is the matching correlation.
 * With zbar the adjoint of z, for each lag d
 *   xbar_{1:m} += w_d zbar_{d+1:d+m},
 *   wbar_d     += zbar_{d+1:d+m}^T x_{1:m},
 * and ybar_{D - d} = wbar_d.
 *
 * Both passes run on doubles in one reverse_pass_callback, so the
 * convolution is one autodiff node rather than one dot_product() node per
 * output. x and y may each be var or double. The function is put in
 * stan::math so that Stan-generated code finds it.
 */

#include <stan/math.hpp>
#include <algorithm>
#include <ostream>
#include <stdexcept>

namespace epinow2 {
namespace internal {

inline void check_convolve_len(int n, int D, int len) {
  if (n + D - 1 < len) {
    throw std::domain_error(
        "convolve_with_rev_pmf: len is longer than x and y convolved");
  }
  if (n > len) {
    throw std::domain_error("convolve_with_rev_pmf: len is shorter than x");
  }
}

// z_{d+1:d+m} += w_d x_{1:m} for each lag d, with w_d = y_{D - d}.
inline Eigen::VectorXd convolve_forward(const Eigen::VectorXd& x,
                                        const Eigen::VectorXd& y, int len) {
  const int n = x.size();
  const int D = y.size();
  Eigen::VectorXd z = Eigen::VectorXd::Zero(len);
  for (int d = 0; d < D; ++d) {
    const int m = std::min(n, len - d);
    if (m <= 0) {
      break;
    }
    z.segment(d, m) += y(D - 1 - d) * x.head(m);
  }
  return z;
}

// xbar_{1:m} += w_d zbar_{d+1:d+m}; wbar_d += zbar_{d+1:d+m}^T x_{1:m}.
inline void convolve_reverse(const Eigen::VectorXd& x,
                             const Eigen::VectorXd& y,
                             const Eigen::VectorXd& zbar,
                             Eigen::VectorXd* xbar, Eigen::VectorXd* ybar) {
  const int n = x.size();
  const int D = y.size();
  const int len = zbar.size();
  for (int d = 0; d < D; ++d) {
    const int m = std::min(n, len - d);
    if (m <= 0) {
      break;
    }
    if (xbar) {
      xbar->head(m) += y(D - 1 - d) * zbar.segment(d, m);
    }
    if (ybar) {
      (*ybar)(D - 1 - d) += zbar.segment(d, m).dot(x.head(m));
    }
  }
}

}  // namespace internal

/**
 * Convolve x with the reversed PMF y, returning a vector of length len.
 *
 * The last argument is the output stream of the Stan calling convention,
 * which is not used.
 *
 * @param x Input vector (var or double).
 * @param y Reversed PMF (var or double).
 * @param len Output length, n <= len <= n + D - 1.
 * @throws std::domain_error if len is outside that range.
 */
template <typename T0, typename T1,
          stan::require_all_eigen_col_vector_t<T0, T1>* = nullptr>
inline Eigen::Matrix<stan::return_type_t<T0, T1>, Eigen::Dynamic, 1>
convolve_with_rev_pmf(const T0& x, const T1& y, const int& len,
                      std::ostream* /* pstream__ */) {
  using stan::arena_t;
  using stan::math::var;
  constexpr bool x_var = stan::is_var<stan::value_type_t<T0>>::value;
  constexpr bool y_var = stan::is_var<stan::value_type_t<T1>>::value;
  internal::check_convolve_len(x.size(), y.size(), len);
  if constexpr (!x_var && !y_var) {
    return internal::convolve_forward(stan::math::value_of(x),
                                      stan::math::value_of(y), len);
  } else {
    arena_t<Eigen::Matrix<stan::value_type_t<T0>, Eigen::Dynamic, 1>>
        x_arena = x;
    arena_t<Eigen::Matrix<stan::value_type_t<T1>, Eigen::Dynamic, 1>>
        y_arena = y;
    arena_t<Eigen::VectorXd> x_val = stan::math::value_of(x_arena);
    arena_t<Eigen::VectorXd> y_val = stan::math::value_of(y_arena);
    const Eigen::VectorXd z = internal::convolve_forward(x_val, y_val, len);
    arena_t<Eigen::Matrix<var, Eigen::Dynamic, 1>> res(len);
    for (int i = 0; i < len; ++i) {
      res.coeffRef(i) = var(z(i));
    }
    stan::math::reverse_pass_callback(
        [=]() mutable {
          const Eigen::VectorXd zbar = res.adj();
          Eigen::VectorXd xbar;
          Eigen::VectorXd ybar;
          if constexpr (x_var) {
            xbar = Eigen::VectorXd::Zero(x_val.size());
          }
          if constexpr (y_var) {
            ybar = Eigen::VectorXd::Zero(y_val.size());
          }
          internal::convolve_reverse(x_val, y_val, zbar,
                                     x_var ? &xbar : nullptr,
                                     y_var ? &ybar : nullptr);
          if constexpr (x_var) {
            x_arena.adj() += xbar;
          }
          if constexpr (y_var) {
            y_arena.adj() += ybar;
          }
        });
    return Eigen::Matrix<var, Eigen::Dynamic, 1>(res);
  }
}

}  // namespace epinow2

namespace stan {
namespace math {
using ::epinow2::convolve_with_rev_pmf;
}  // namespace math
}  // namespace stan

#endif
