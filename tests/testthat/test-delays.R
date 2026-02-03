test_stan_delays <- function(generation_time = gt_opts(Fixed(1)),
                             delays = delay_opts(),
                             truncation = trunc_opts(),
                             params = c()) {
  data <- EpiNow2:::create_stan_delays(
    generation_time = generation_time,
    delays = delays,
    truncation = truncation,
    time_points = 10
  )
  return(unlist(unname(data[params])))
}

delay_params <-
  c("delay_params_mean", "delay_params_sd", "delay_max", "delay_np_pmf")

test_that("generation times can be specified in different ways", {
  expect_equal(
    test_stan_delays(params = delay_params),
    c(0, 1, 1, 1)
  )
  expect_equal(
    test_stan_delays(
      generation_time = gt_opts(Fixed(value = 3)),
      params = delay_params
    ),
    c(0, 0, 0, 1, 1, 1)
  )
  expect_equal(
    round(test_stan_delays(
      generation_time = gt_opts(
        LogNormal(meanlog = 3, sdlog = 1, max = 4)
      ),
      params = delay_params
    ), digits = 2),
    c(0.01, 0.12, 0.34, 0.53, 1.00, 1.00)
  )
})

test_that("delay parameters can be specified in different ways", {
  expect_equal(
    tail(test_stan_delays(
      delays = delay_opts(Fixed(value = 3)),
      params = delay_params
    ), n = -2),
    c(0, 0, 0, 1, 1)
  )
  expect_equal(
    tail(round(test_stan_delays(
      delays = delay_opts(
        LogNormal(meanlog = 3, sdlog = 1, max = 4)
      ),
      params = delay_params
    ), digits = 2), n = -2),
    c(0.01, 0.12, 0.34, 0.53, 1.00)
  )
  expect_equal(
    tail(round(test_stan_delays(
      delays = suppressMessages(delay_opts(
        LogNormal(meanlog = 0.5, sdlog = 0.5)
      )),
      params = delay_params
    ), digits = 2), n = -2),
    c(0.03, 0.38, 0.37, 0.14, 0.05, 0.02, 0.01, 0.00, 0.00, 1.00)
  )
  expect_equal(
    test_stan_delays(
      delays = delay_opts(NonParametric(pmf = c(0.1, 0.6, 0.3))),
      params = delay_params
    ),
    c(0.0, 1.0, 0.1, 0.6, 0.3, 1.0)
  )
})

test_that("truncation parameters can be specified in different ways", {
  expect_equal(
    tail(round(test_stan_delays(
      truncation = trunc_opts(
        dist = LogNormal(meanlog = 3, sdlog = 1, max = 4)
      ),
      params = delay_params
    ), digits = 2), n = -2),
    c(1.00, 0.01, 0.12, 0.34, 0.53)
  )
})

test_that("distributions incompatible with stan models are caught", {
  expect_error(suppressMessages(gt_opts(
    Gamma(2, 2),
    default_cdf_cutoff = 0
  )), "maximum")
  expect_error(delay_opts(
    Normal(2, 2, max = 10)
  ), "lognormal")
})

test_that("create_stan_delays creates delay_id_* variables with correct names", {
  # Test with all delay types (infection context)
  data <- EpiNow2:::create_stan_delays(
    generation_time = gt_opts(Fixed(1)),
    reporting = delay_opts(Fixed(2)),
    truncation = trunc_opts(Fixed(1))
  )

  expect_true("delay_id_generation_time" %in% names(data))
  expect_true("delay_id_reporting" %in% names(data))
  expect_true("delay_id_truncation" %in% names(data))

  # IDs should be sequential for non-empty delays
  expect_equal(data$delay_id_generation_time, 1)
  expect_equal(data$delay_id_reporting, 2)
  expect_equal(data$delay_id_truncation, 3)
})

test_that("create_stan_delays creates delay_id_* for secondary models", {
  # Test with reporting delay for secondary models
  data <- EpiNow2:::create_stan_delays(
    reporting = delay_opts(Fixed(2)),
    truncation = trunc_opts(Fixed(1))
  )

  expect_true("delay_id_reporting" %in% names(data))
  expect_true("delay_id_truncation" %in% names(data))

  expect_equal(data$delay_id_reporting, 1)
  expect_equal(data$delay_id_truncation, 2)
})

test_that("create_stan_delays sets ID to 0 for missing delays", {
  # Test with only one delay type
  data <- EpiNow2:::create_stan_delays(
    generation_time = gt_opts(Fixed(1))
  )

  expect_equal(data$delay_id_generation_time, 1)
  # No reporting or truncation delays provided
  expect_false("delay_id_reporting" %in% names(data))
  expect_false("delay_id_truncation" %in% names(data))
})

test_that("extract_delays works with delay_id_* naming", {
  # Create mock samples with delay_params (2 samples, 2 params)
  samples <- list(
    delay_params = matrix(c(1.5, 2.0, 1.8, 2.2), nrow = 2, ncol = 2)
  )
  # Args contain the ID lookup information using existing delay variables
  # Scenario: one delay type (generation_time) with one parametric delay
  args <- list(
    delay_id_generation_time = 1,
    delay_id_reporting = 0,
    delay_types_groups = c(1, 2),    # type 1 has flat delay 1
    delay_types_p = c(1),            # flat delay 1 is parametric
    delay_types_id = c(1),           # flat delay 1 is parametric delay 1
    delay_params_groups = c(1, 3)    # parametric delay 1 has params 1-2
  )

  result <- EpiNow2:::extract_delays(samples, args = args)

  expect_true(!is.null(result))
  expect_true("variable" %in% names(result))
  expect_true("sample" %in% names(result))
  expect_true("value" %in% names(result))

  # Check that generation_time parameters are named correctly
  expect_true(any(grepl("generation_time\\[1\\]", result$variable)))
  expect_true(any(grepl("generation_time\\[2\\]", result$variable)))
})

test_that("extract_delays returns NULL when delay_params don't exist", {
  samples <- list(some_other_param = 1:10)
  args <- list()  # Empty args
  result <- EpiNow2:::extract_delays(samples, args = args)
  expect_null(result)
})

test_that("extract_delays handles delays with no ID lookup gracefully", {
  # Samples with delay_params but args without delay_id_* variables
  samples <- list(
    delay_params = matrix(c(1.5, 2.0), nrow = 2, ncol = 1)
  )
  args <- list()  # No ID lookup information

  result <- EpiNow2:::extract_delays(samples, args = args)

  expect_true(!is.null(result))
  # Should fall back to indexed naming
  expect_true(any(grepl("delay_params\\[", result$variable)))
})

test_that("build_delay_name_lookup correctly names parameters", {
  # Scenario 1: Single parametric delay (reporting with 2 params)
  args_single <- list(
    delay_id_reporting = 1,
    delay_types_groups = c(1, 2),
    delay_types_p = c(1),
    delay_types_id = c(1),
    delay_params_groups = c(1, 3)
  )
  result <- EpiNow2:::build_delay_name_lookup(args_single, n_cols = 2)
  expect_equal(result, c("reporting[1]", "reporting[2]"))

  # Scenario 2: Nonparametric followed by parametric
  # This pattern caused bug #1236: truncation is Fixed(0), reporting is LogNormal
  args_nonparam_first <- list(
    delay_id_truncation = 1,
    delay_id_reporting = 2,
    delay_types_groups = c(1, 2, 3),
    delay_types_p = c(0, 1),
    delay_types_id = c(1, 1),
    delay_params_groups = c(1, 3)
  )
  result <- EpiNow2:::build_delay_name_lookup(args_nonparam_first, n_cols = 2)
  # Should be reporting, NOT truncation
  expect_equal(result, c("reporting[1]", "reporting[2]"))

  # Scenario 3: Two parametric delays (reporting then truncation)
  args_both_param <- list(
    delay_id_reporting = 1,
    delay_id_truncation = 2,
    delay_types_groups = c(1, 2, 3),
    delay_types_p = c(1, 1),
    delay_types_id = c(1, 2),
    delay_params_groups = c(1, 3, 5)
  )
  result <- EpiNow2:::build_delay_name_lookup(args_both_param, n_cols = 4)
  expect_equal(
    result,
    c("reporting[1]", "reporting[2]", "truncation[1]", "truncation[2]")
  )

  # Scenario 4: Parametric followed by nonparametric
  args_param_first <- list(
    delay_id_reporting = 1,
    delay_id_truncation = 2,
    delay_types_groups = c(1, 2, 3),
    delay_types_p = c(1, 0),
    delay_types_id = c(1, 1),
    delay_params_groups = c(1, 3)
  )
  result <- EpiNow2:::build_delay_name_lookup(args_param_first, n_cols = 2)
  expect_equal(result, c("reporting[1]", "reporting[2]"))

  # Scenario 5: Three delay types with mixed parametric/nonparametric
  # generation_time (parametric), reporting (nonparametric), truncation (param)
  args_three_mixed <- list(
    delay_id_generation_time = 1,
    delay_id_reporting = 2,
    delay_id_truncation = 3,
    delay_types_groups = c(1, 2, 3, 4),
    delay_types_p = c(1, 0, 1),
    delay_types_id = c(1, 1, 2),
    delay_params_groups = c(1, 3, 5)
  )
  result <- EpiNow2:::build_delay_name_lookup(args_three_mixed, n_cols = 4)
  expect_equal(
    result,
    c("generation_time[1]", "generation_time[2]",
      "truncation[1]", "truncation[2]")
  )
})
