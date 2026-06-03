skip_on_cran()
skip_on_os("windows")

# Integrate the natural-scale density implied by gp_init_lpdf over the
# supported region and check it equals one. gp_init_lpdf returns the density
# on the log scale (i.e. with a + log(x) Jacobian for the natural-to-log
# transform); dividing by x recovers the natural-scale truncated density.

integrate_gp_init_lpdf <- function(dist_type, params, lower, upper) {
  density_on_natural <- function(x) {
    vapply(
      x,
      function(xi) exp(
        gp_init_lpdf(xi, dist_type, params, lower, upper) - log(xi)
      ),
      numeric(1)
    )
  }
  stats::integrate(density_on_natural, lower = lower, upper = upper)$value
}

test_that("gp_init_lpdf integrates to one for a lognormal prior on its natural support", {
  expect_equal(
    integrate_gp_init_lpdf(0L, c(0, 0.5), lower = 0, upper = Inf),
    1, tolerance = 1e-4
  )
})

test_that("gp_init_lpdf integrates to one for a lognormal prior with a tighter lower bound", {
  expect_equal(
    integrate_gp_init_lpdf(0L, c(0, 0.5), lower = 0.5, upper = Inf),
    1, tolerance = 1e-4
  )
})

test_that("gp_init_lpdf integrates to one for a lognormal prior with an upper bound", {
  expect_equal(
    integrate_gp_init_lpdf(0L, c(0, 0.5), lower = 0, upper = 5),
    1, tolerance = 1e-4
  )
})

test_that("gp_init_lpdf integrates to one for a lognormal prior with both bounds tightened", {
  expect_equal(
    integrate_gp_init_lpdf(0L, c(0, 0.5), lower = 0.5, upper = 5),
    1, tolerance = 1e-4
  )
})

test_that("gp_init_lpdf integrates to one for a gamma prior on its natural support", {
  expect_equal(
    integrate_gp_init_lpdf(1L, c(2, 1), lower = 0, upper = Inf),
    1, tolerance = 1e-4
  )
})

test_that("gp_init_lpdf integrates to one for a gamma prior with both bounds tightened", {
  expect_equal(
    integrate_gp_init_lpdf(1L, c(2, 1), lower = 0.5, upper = 5),
    1, tolerance = 1e-4
  )
})

test_that("gp_init_lpdf integrates to one for an unbounded normal prior", {
  expect_equal(
    integrate_gp_init_lpdf(2L, c(0, 1), lower = -Inf, upper = Inf),
    1, tolerance = 1e-4
  )
})

test_that("gp_init_lpdf integrates to one for a half-normal prior matching params_lp", {
  expect_equal(
    integrate_gp_init_lpdf(2L, c(0, 1), lower = 0, upper = Inf),
    1, tolerance = 1e-4
  )
})

test_that("gp_init_lpdf integrates to one for a positive-truncated normal prior at typical R0", {
  expect_equal(
    integrate_gp_init_lpdf(2L, c(2, 0.5), lower = 0, upper = Inf),
    1, tolerance = 1e-4
  )
})

test_that("gp_init_lpdf integrates to one for a normal prior with both bounds finite", {
  expect_equal(
    integrate_gp_init_lpdf(2L, c(0, 1), lower = -2, upper = 2),
    1, tolerance = 1e-4
  )
})
