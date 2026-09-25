#ifndef EPINOW2_EXP_ADD_INDEXED_HPP
#define EPINOW2_EXP_ADD_INDEXED_HPP

/**
 * exp(c + levels[idx] + x) for a scalar c, a vector of levels, indices
 * into it and a vector x, with a reverse-mode adjoint.
 *
 * Implements `exp_add_indexed(c, levels, idx, x)` declared in
 * inst/stan/functions/rt.stan.
 *
 * Forward (1-based):  y_i = exp(c + levels_{idx_i} + x_i).
 * Reverse, with g_i = ybar_i y_i:
 *   cbar += sum_i g_i,  levelsbar_{idx_i} += g_i,  xbar_i += g_i.
 *
 * In Stan this is an indexed copy, two additions and exp(), each one
 * autodiff node per element. Here it is one reverse_pass_callback on
 * doubles. c, levels and x may each be var or double. The function is put
 * in stan::math so that Stan-generated code finds it.
 */

#include <stan/math.hpp>
#include <ostream>
#include <vector>

namespace epinow2 {
namespace internal {

// y_i = exp(c + levels_{idx_i} + x_i), checking each index.
template <typename L, typename X, typename Y>
inline void exp_add_indexed_forward(double c, const L& levels,
                                    const std::vector<int>& idx, const X& x,
                                    Y& y) {
  for (size_t i = 0; i < idx.size(); ++i) {
    stan::math::check_range("exp_add_indexed", "idx", levels.size(), idx[i]);
    y.coeffRef(i) = std::exp(c + levels.coeff(idx[i] - 1) + x.coeff(i));
  }
}

}  // namespace internal

/**
 * Exponential of c plus the indexed levels plus x.
 *
 * The last argument is the output stream of the Stan calling convention,
 * which is not used.
 *
 * @param c Scalar (var or double).
 * @param levels Vector of levels (var or double).
 * @param idx 1-based indices into levels, one per element of x.
 * @param x Vector (var or double).
 * @throws std::invalid_argument if idx and x differ in size.
 * @throws std::out_of_range if an index is out of range.
 */
template <typename T0, typename T1, typename T2,
          stan::require_stan_scalar_t<T0>* = nullptr,
          stan::require_all_eigen_col_vector_t<T1, T2>* = nullptr>
inline Eigen::Matrix<stan::return_type_t<T0, T1, T2>, Eigen::Dynamic, 1>
exp_add_indexed(const T0& c, const T1& levels, const std::vector<int>& idx,
                const T2& x, std::ostream* /* pstream__ */) {
  using stan::arena_t;
  using stan::math::var;
  constexpr bool c_var = stan::is_var<T0>::value;
  constexpr bool l_var = stan::is_var<stan::value_type_t<T1>>::value;
  constexpr bool x_var = stan::is_var<stan::value_type_t<T2>>::value;
  stan::math::check_size_match("exp_add_indexed", "idx", idx.size(), "x",
                               x.size());
  const int n = idx.size();
  const double c_val = stan::math::value_of(c);
  if constexpr (!c_var && !l_var && !x_var) {
    Eigen::VectorXd y(n);
    internal::exp_add_indexed_forward(c_val, stan::math::value_of(levels),
                                      idx, stan::math::value_of(x), y);
    return y;
  } else {
    arena_t<Eigen::Matrix<stan::value_type_t<T1>, Eigen::Dynamic, 1>>
        l_arena = levels;
    arena_t<Eigen::Matrix<stan::value_type_t<T2>, Eigen::Dynamic, 1>>
        x_arena = x;
    arena_t<std::vector<int>> idx_arena(idx.begin(), idx.end());
    arena_t<Eigen::VectorXd> y_arena(n);
    internal::exp_add_indexed_forward(c_val, stan::math::value_of(l_arena),
                                      idx, stan::math::value_of(x_arena),
                                      y_arena);
    arena_t<Eigen::Matrix<var, Eigen::Dynamic, 1>> res(n);
    for (int i = 0; i < n; ++i) {
      res.coeffRef(i) = var(y_arena(i));
    }
    var c_v;
    if constexpr (c_var) {
      c_v = c;
    }
    stan::math::reverse_pass_callback([=]() mutable {
      double c_adj = 0;
      for (int i = 0; i < n; ++i) {
        const double g = res.adj().coeff(i) * y_arena(i);
        c_adj += g;
        if constexpr (l_var) {
          l_arena.coeffRef(idx_arena[i] - 1).adj() += g;
        }
        if constexpr (x_var) {
          x_arena.coeffRef(i).adj() += g;
        }
      }
      if constexpr (c_var) {
        c_v.adj() += c_adj;
      }
    });
    return Eigen::Matrix<var, Eigen::Dynamic, 1>(res);
  }
}

}  // namespace epinow2

namespace stan {
namespace math {
using ::epinow2::exp_add_indexed;
}  // namespace math
}  // namespace stan

#endif
