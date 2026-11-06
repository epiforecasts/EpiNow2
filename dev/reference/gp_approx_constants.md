# Constants linking the lengthscale to the approximate GP settings

Constants from Riutort-Mayol et al. (2023, Section 4.3.1)
[doi:10.1007/s11222-022-10167-2](https://doi.org/10.1007/s11222-022-10167-2)
. With half-range `S`, boundary factor `c` and `m` basis functions,
lengthscales `l` are approximated accurately when
`m >= m_factor * c * S / l` and `c >= c_factor * l / S` (with
`c >= 1.2`). The paper does not cover Matern 1/2 (equivalently, the
Ornstein-Uhlenbeck kernel). Its `m_factor` is derived by repeating the
paper's simulation procedure (see the `hsgp_m_c_relationship` folder,
branch `matern-half-m-c-relationship`, of
<https://github.com/sbfnk-bot/basis_functions_approach_to_GP>). The same
procedure reproduced the squared exponential and Matern 3/2 and 5/2
constants to within 8-13%. Matern 1/2 is the roughest kernel, so it
needs the most basis functions. Its `c_factor` was not independently
validated and uses the Matern 3/2 value. The periodic kernel has no
boundary and needs `m >= m_factor * S / l` basis functions (Appendix B),
so its `c_factor` is `NA`.

## Usage

``` r
gp_approx_constants(gp_type, nu)
```

## Arguments

- gp_type:

  Integer kernel type as used in the Stan model (0: squared exponential,
  1: periodic, 2: Matern).

- nu:

  Numeric Matern order.

## Value

A list with elements `m_factor` and `c_factor`.
