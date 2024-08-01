skip_on_cran()
skip_on_os("windows")

test_that("distributions are the same in R and stan", {
  args <- list(mean = 3, sd = 2, max = 15)

  lognormal_dist <- do.call(LogNormal, args)
  gamma_dist <- do.call(Gamma, args)

  lognormal_params <- unname(as.numeric(get_parameters(lognormal_dist)))
  gamma_params <- unname(as.numeric(get_parameters(gamma_dist)))

  pmf_r_lognormal <- get_pmf(discretise(lognormal_dist))
  pmf_r_gamma <- get_pmf(discretise(gamma_dist))

  pmf_stan_lognormal <- discretised_pmf(lognormal_params, args$max + 1, 0)
  pmf_stan_gamma <- discretised_pmf(gamma_params, args$max + 1, 1)

  expect_equal(pmf_r_lognormal, pmf_stan_lognormal)
  expect_equal(pmf_r_gamma, pmf_stan_gamma)
})
