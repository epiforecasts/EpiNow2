#ifndef EPINOW2_CUMSUM_HOLD_HPP
#define EPINOW2_CUMSUM_HOLD_HPP

/**
 * Cumulative sum starting from 0, held at its last value, with a
 * reverse-mode adjoint.
 *
 * Implements `cumsum_hold(x, t)` declared in inst/stan/functions/rt.stan.
 *
 * Forward (1-based, x of length n, n + 1 <= t):
 *   y_1 = 0,  y_{k+1} = y_k + x_k  (k = 1, ..., n),
 *   y_j = y_{n+1}  (j > n + 1).
 * Reverse: x_k enters y_{k+1}, ..., y_t, so
 *   xbar_k += sum_{j = k + 1}^{t} ybar_j.
 *
 * In Stan this is append_row(0, cumulative_sum(x)) followed by a hold
 * forward, which copies several length-t autodiff vectors. Here it is one
 * reverse_pass_callback on doubles. x may be var or double. The function
 * is put in stan::math so that Stan-generated code finds it.
 */

#include <stan/math.hpp>
#include <ostream>

namespace epinow2 {
namespace internal {

// y_1 = 0, y_{k+1} = y_k + x_k, then y held at y_{n+1} up to length t.
template <typename V>
inline Eigen::VectorXd cumsum_hold_forward(const V& x_val, int t) {
  const int n = x_val.size();
  Eigen::VectorXd y = Eigen::VectorXd::Zero(t);
  for (int k = 0; k < n; ++k) {
    y(k + 1) = y(k) + x_val.coeff(k);
  }
  y.tail(t - n - 1).setConstant(y(n));
  return y;
}

}  // namespace internal

/**
 * Cumulative sum of x starting from 0, held to length t.
 *
 * The last argument is the output stream of the Stan calling convention,
 * which is not used.
 *
 * @param x Increments (var or double).
 * @param t Output length, at least `x.size() + 1`.
 * @throws std::domain_error if t is shorter than `x.size() + 1`.
 */
template <typename T, stan::require_eigen_col_vector_t<T>* = nullptr>
inline Eigen::Matrix<stan::value_type_t<T>, Eigen::Dynamic, 1> cumsum_hold(
    const T& x, const int& t, std::ostream* /* pstream__ */) {
  using stan::arena_t;
  using stan::math::var;
  const int n = x.size();
  stan::math::check_greater_or_equal("cumsum_hold", "t", t, n + 1);
  if constexpr (!stan::is_var<stan::value_type_t<T>>::value) {
    return internal::cumsum_hold_forward(stan::math::value_of(x), t);
  } else {
    arena_t<Eigen::Matrix<var, Eigen::Dynamic, 1>> x_arena = x;
    const Eigen::VectorXd y
        = internal::cumsum_hold_forward(stan::math::value_of(x_arena), t);
    arena_t<Eigen::Matrix<var, Eigen::Dynamic, 1>> res(t);
    for (int j = 0; j < t; ++j) {
      res.coeffRef(j) = var(y(j));
    }
    stan::math::reverse_pass_callback([x_arena, res, n, t]() mutable {
      double tail = 0;
      for (int j = t - 1; j >= 1; --j) {
        tail += res.adj().coeff(j);
        if (j <= n) {
          x_arena.coeffRef(j - 1).adj() += tail;
        }
      }
    });
    return Eigen::Matrix<var, Eigen::Dynamic, 1>(res);
  }
}

}  // namespace epinow2

namespace stan {
namespace math {
using ::epinow2::cumsum_hold;
}  // namespace math
}  // namespace stan

#endif
