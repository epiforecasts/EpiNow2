skip_on_cran()

delay_data <- suppressMessages(create_stan_delays(
  generation_time = gt_opts(
    LogNormal(meanlog = Normal(1, 0.1), sdlog = 0.5, max = 10)
  ),
  time_points = 10
))

test_that("reconstruct_delay returns priors and fixed values without a fit", {
  gt <- reconstruct_delay(
    list(args = delay_data, fit = NULL), "generation_time"
  )

  expect_s3_class(gt, "dist_spec")
  expect_equal(get_distribution(gt), "lognormal")
  expect_equal(max(gt), 10)
  # uncertain parameters fall back to their prior
  expect_equal(get_parameters(gt)$meanlog, Normal(1, 0.1))
  # fixed parameters keep their value
  expect_equal(get_parameters(gt)$sdlog, 0.5)
})

test_that("reconstruct_delay returns NULL for absent delays", {
  obj <- list(args = delay_data, fit = NULL)
  expect_null(reconstruct_delay(obj, "reporting"))

  obj$args$delay_id_generation_time <- 0
  expect_null(reconstruct_delay(obj, "generation_time"))

  obj <- list(args = delay_data, fit = NULL)
  obj$args$delay_types_groups <- NULL
  expect_null(reconstruct_delay(obj, "generation_time"))
})
