#ifndef EPINOW2_EXP_ADD_HPP
#define EPINOW2_EXP_ADD_HPP

/**
 * exp(c + x) for a scalar c and a vector x, with a reverse-mode adjoint.
 *
 * Implements `exp_add(c, x)` declared in inst/stan/functions/rt.stan.
 *
 * Forward:  y_i = exp(c + x_i).
 * Reverse:  xbar_i += ybar_i y_i,  cbar += sum_i ybar_i y_i.
 *
 * Stan builds c + x and exp() of it as one autodiff node per element each.
 * Here the whole operation is one reverse_pass_callback on doubles. c and x
 * may each be var or double. The function is put in stan::math so that
 * Stan-generated code finds it.
 */

#include <stan/math.hpp>
#include <ostream>

namespace epinow2 {

/**
 * Exponential of a scalar added to each element of a vector.
 *
 * The last argument is the output stream of the Stan calling convention,
 * which is not used.
 *
 * @param c Scalar added to every element (var or double).
 * @param x Vector (var or double).
 * @return The vector exp(c + x).
 */
template <typename T0, typename T1, stan::require_stan_scalar_t<T0>* = nullptr,
          stan::require_eigen_col_vector_t<T1>* = nullptr>
inline Eigen::Matrix<stan::return_type_t<T0, T1>, Eigen::Dynamic, 1> exp_add(
    const T0& c, const T1& x, std::ostream* /* pstream__ */) {
  using stan::arena_t;
  using stan::math::var;
  constexpr bool c_var = stan::is_var<T0>::value;
  constexpr bool x_var = stan::is_var<stan::value_type_t<T1>>::value;
  if constexpr (!c_var && !x_var) {
    return (stan::math::value_of(x).array() + c).exp().matrix();
  } else {
    arena_t<Eigen::Matrix<stan::value_type_t<T1>, Eigen::Dynamic, 1>>
        x_arena = x;
    arena_t<Eigen::VectorXd> y
        = (stan::math::value_of(x_arena).array() + stan::math::value_of(c))
              .exp()
              .matrix();
    arena_t<Eigen::Matrix<var, Eigen::Dynamic, 1>> res(y.size());
    for (Eigen::Index i = 0; i < y.size(); ++i) {
      res.coeffRef(i) = var(y(i));
    }
    var c_v;
    if constexpr (c_var) {
      c_v = c;
    }
    stan::math::reverse_pass_callback([=]() mutable {
      const Eigen::VectorXd g = res.adj().cwiseProduct(y);
      if constexpr (x_var) {
        x_arena.adj() += g;
      }
      if constexpr (c_var) {
        c_v.adj() += g.sum();
      }
    });
    return Eigen::Matrix<var, Eigen::Dynamic, 1>(res);
  }
}

}  // namespace epinow2

namespace stan {
namespace math {
using ::epinow2::exp_add;
}  // namespace math
}  // namespace stan

#endif
