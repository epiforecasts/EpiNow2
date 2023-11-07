skip_on_cran()
skip_on_os("windows")

test_that("distributions are the same in R and stan", {
  args <- list(mean = 3, mean_sd = 0, sd = 2, sd_sd = 0, max_value = 15)
  lognormal_params <-
    do.call(lognorm_dist_def, (c(args, list(samples = 1))))$params[[1]]
  gamma_params <-
    do.call(gamma_dist_def, (c(args, list(samples = 1))))$params[[1]]
  pmf_r_lognormal <- dist_skel(
    n = seq_len(args$max_value + 1) - 1, dist = TRUE, cum = FALSE,
    model = "lognormal", params = lognormal_params, max_value = args$max_value,
    discrete = TRUE
  )
  pmf_r_gamma <- dist_skel(
    n = seq_len(args$max_value + 1) - 1, dist = TRUE, cum = FALSE,
    model = "gamma", params = gamma_params, max_value = args$max_value,
    discrete = TRUE
  )

  pmf_stan_lognormal <- discretised_pmf(
    args$mean, args$sd, args$max_value + 1, 0
  )
  pmf_stan_gamma <- discretised_pmf(args$mean, args$sd, args$max_value + 1, 1)

  expect_equal(pmf_r_lognormal, pmf_stan_lognormal)
  expect_equal(pmf_r_gamma, pmf_stan_gamma)
})
