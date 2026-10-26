skip_on_cran()

fit <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_estimate_infections.rds"
))$fit

test_that("extract_stan_param summarises a single parameter", {
  out <- extract_stan_param(fit, params = "delay_params", CrIs = 0.5)
  draws <- as.matrix(fit, pars = "delay_params")

  expect_s3_class(out, "data.table")
  expect_named(
    out, c("mean", "se_mean", "sd", "lower_50", "median", "upper_50")
  )
  expect_equal(nrow(out), ncol(draws))
  expect_equal(out$mean, unname(colMeans(draws)))
  expect_equal(out$median, unname(apply(draws, 2, median)))
})

test_that("extract_stan_param names variables for several parameters", {
  out <- extract_stan_param(fit, params = c("params", "delay_params"))
  expect_named(
    out,
    c(
      "variable", "mean", "se_mean", "sd", "lower_90", "lower_50",
      "lower_20", "median", "upper_20", "upper_50", "upper_90"
    )
  )
  expect_true(all(grepl("^(params|delay_params)\\[", out$variable)))

  all_params <- extract_stan_param(fit)
  expect_true("variable" %in% colnames(all_params))
  expect_true(all(out$variable %in% all_params$variable))
})

test_that("extract_inits samples initial values from the posterior", {
  inits <- extract_inits(fit, current_inits = NULL, samples = 2)
  expect_type(inits, "closure")

  init <- inits()
  expect_type(init, "list")
  # generated quantities are not used as initial values
  excluded <- c(
    "r", "log_lik", "lp__", "infections", "reports", "obs_reports",
    "imputed_reports"
  )
  expect_false(any(excluded %in% names(init)))
  expect_true(all(c("params", "delay_params") %in% names(init)))
  expect_length(init$delay_params, ncol(as.matrix(fit, pars = "delay_params")))
})

test_that("extract_inits keeps excluded values from the current inits", {
  current_inits <- function() list(params = c(99, 99), noise = 1)
  inits <- extract_inits(
    fit, current_inits = current_inits, exclude_list = "params", samples = 2
  )
  init <- inits()
  expect_equal(init$params, c(99, 99))
  expect_false(isTRUE(all.equal(init$noise, 1)))
})
