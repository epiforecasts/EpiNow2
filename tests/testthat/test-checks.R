# Setup for testing -------------------------------------------------------

futile.logger::flog.threshold("FATAL")

# Create reports reports data for estimate_infection()
est_inf <- EpiNow2::example_confirmed[1:10]

# Create reports reports data for estimate_secondary()
est_sec <- data.table::copy(est_inf)[
  ,
  `:=`(
    primary = confirm,
    secondary = round(0.5 * confirm),
    confirm = NULL
  )
]

# Custom test functions ---------------------------------------------------
test_col_specs <- function(dt_list, model = "estimate_infections") {
  expect_error(
    check_reports_valid(dt_list$bad_col_name,
      for_estimate_secondary = for_estimate_secondary
    )
  )
  expect_error(
    check_reports_valid(dt_list$bad_col_type,
      for_estimate_secondary = for_estimate_secondary
    )
  )
  expect_error(
    check_reports_valid(dt_list$bad_col_entry,
      for_estimate_secondary = for_estimate_secondary
    )
  )
}

test_that("check_reports_valid errors for bad 'confirm' specifications", {
  # Bad "confirm" column spec scenarios
  confirm_col_dt <- list(
    # Bad column name
    bad_col_name = data.table::copy(est_inf)[
      ,
      `:=`(
        confirm_bad_name = confirm,
        confirm = NULL
      )
    ],
    # Bad column type
    bad_col_type = data.table::copy(est_inf)[
      ,
      lapply(.SD, as.character),
      by = confirm
    ],
    # Bad column entry
    bad_col_entry = data.table::copy(est_inf)[
      ,
      confirm := -confirm
    ]
  )
  # Run tests
  test_col_specs(confirm_col_dt, model = "estimate_infections")
})

test_that("check_reports_valid errors for bad 'date' specifications", {
  # Bad "date" column spec scenarios
  date_col_dt <- list(
    # Bad column name
    bad_col_name = data.table::copy(est_inf)[
      ,
      `:=`(
        date_bad_name = date,
        date = NULL
      )
    ],
    # Bad column type
    bad_col_type = data.table::copy(est_inf)[
      ,
      lapply(.SD, as.character),
      by = date
    ],
    # Bad column entry
    bad_col_entry = data.table::copy(est_inf)[
      c(1, 3),
      date := NA
    ]
  )
  # Run tests
  test_col_specs(date_col_dt, model = "estimate_infections")
})

test_that("check_reports_valid errors for bad 'primary' specifications", {
  # Bad "primary" column spec scenarios
  primary_col_dt <- list(
    # Bad column name
    bad_col_name = data.table::copy(est_sec)[
      ,
      `:=`(
        primary_bad_name = primary,
        primary = NULL
      )
    ],
    # Bad column type
    bad_col_type = data.table::copy(est_sec)[
      ,
      lapply(.SD, as.character),
      by = primary
    ],
    # Bad column entry
    bad_col_entry = data.table::copy(est_sec)[
      ,
      primary := -primary
    ]
  )
  # Run tests
  test_col_specs(primary_col_dt, model = "estimate_secondary")
})

test_that("check_reports_valid errors for bad 'secondary' specifications", {
  # Bad "secondary" column spec scenarios
  secondary_col_dt <- list(
    # Bad column name
    bad_col_name = data.table::copy(est_sec)[
      ,
      `:=`(
        secondary_bad_name = primary,
        secondary = NULL
      )
    ],
    # Bad column type
    bad_col_type = data.table::copy(est_sec)[
      ,
      lapply(.SD, as.character),
      by = secondary
    ],
    # Bad column entry
    bad_col_entry = data.table::copy(est_sec)[
      ,
      secondary := -secondary
    ]
  )
  # Run tests
  test_col_specs(secondary_col_dt, model = "estimate_secondary")
})

test_that("check_sparse_pmf_tail throws a warning as expected", {
  # NB: The warning is set to be thrown once every 8 hours, so hard to test
  # regularly. The fix is to change the local setting here to throw the
  # warning on demand for the sake of multiple runs of the test within
  # 8 hours. That's what the rlang call below does
  rlang::local_options(rlib_warning_verbosity = "verbose")
  pmf <- c(0.4, 0.30, 0.20, 0.05, 0.049995, 4.5e-06, rep(1e-7, 5))
  expect_warning(
    check_sparse_pmf_tail(pmf),
    "PMF tail has"
  )
})

test_that("check_np_delay_lengths returns invisibly when no nonparametric distributions", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with only parametric distributions
  stan_args <- list(
    data = list(
      delay_n_np = 0
    )
  )
  expect_invisible(
    check_np_delay_lengths(stan_args, data_length = 10)
  )
})

test_that("check_np_delay_lengths does not warn when PMFs are shorter than data", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with short PMF (length 5) and data length 10
  stan_args <- list(
    data = list(
      delay_n_np = 2,
      delay_np_pmf_groups = array(c(1, 6, 11))
    )
  )
  expect_no_warning(
    check_np_delay_lengths(stan_args, data_length = 10)
  )
})

test_that("check_np_delay_lengths warns when PMF is longer than data", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with long PMF (length 15) and short data (length 10)
  # Note: Warning uses .frequency = "once", so this is the only test that
  # expects the warning
  stan_args <- list(
    data = list(
      delay_n_np = 1,
      delay_np_pmf_groups = array(c(1, 16)) # One PMF of length 15
    )
  )
  expect_warning(
    check_np_delay_lengths(stan_args, data_length = 10),
    "non-parametric delay distributions are longer"
  )
})

test_that("check_np_delay_lengths handles zero delay_n_np", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with zero delay_n_np
  stan_args <- list(
    data = list(
      delay_n_np = 0
    )
  )
  expect_invisible(check_np_delay_lengths(stan_args, data_length = 10))
})

test_that("check_np_delay_lengths works with delays from create_stan_delays", {
  rlang::local_options(rlib_warning_verbosity = "verbose")

  # Short explicit NonParametric delay (should NOT warn)
  short_np_delay <- delay_opts(NonParametric(pmf = c(0.3, 0.5, 0.2)))
  stan_args_short_np <- list(
    data = create_stan_delays(
      delays = short_np_delay,
      time_points = 1L
    )
  )
  expect_invisible(
    check_np_delay_lengths(stan_args_short_np, data_length = 10)
  )

  # Long explicit NonParametric delay (should warn)
  long_np_delay <- delay_opts(NonParametric(pmf = rep(1/15, 15)))
  stan_args_long_np <- list(
    data = create_stan_delays(
      delays = long_np_delay,
      time_points = 1L
    )
  )
  expect_warning(
    check_np_delay_lengths(stan_args_long_np, data_length = 10),
    "non-parametric delay distributions are longer"
  )

  # Short fixed parametric delay that becomes non-parametric (should NOT warn)
  short_fixed_delay <- delay_opts(Fixed(value = 3))
  stan_args_short_fixed <- list(
    data = create_stan_delays(
      delays = short_fixed_delay,
      time_points = 1L
    )
  )
  expect_invisible(
    check_np_delay_lengths(stan_args_short_fixed, data_length = 10)
  )

  # Long fixed parametric delay that becomes non-parametric (should warn)
  long_fixed_delay <- delay_opts(LogNormal(meanlog = 2, sdlog = 0.5, max = 20))
  stan_args_long_fixed <- list(
    data = create_stan_delays(
      delays = long_fixed_delay,
      time_points = 1L
    )
  )
  expect_warning(
    check_np_delay_lengths(stan_args_long_fixed, data_length = 10),
    "non-parametric delay distributions are longer"
  )
})

test_that("check_np_delay_lengths works with explicitly defined non-parametric delay vectors", {
  rlang::local_options(rlib_warning_verbosity = "verbose")

  # Test with short explicit delay vector (5 days, data length 10)
  short_delay_vector <- c(0.1, 0.2, 0.3, 0.25, 0.15)
  short_np_delay <- delay_opts(NonParametric(pmf = short_delay_vector))
  stan_args_short <- list(
    data = create_stan_delays(
      delays = short_np_delay,
      time_points = 1L
    )
  )
  expect_invisible(
    check_np_delay_lengths(stan_args_short, data_length = 10)
  )

  # Test with long explicit delay vector (15 days, data length 10)
  long_delay_vector <- c(
    0.05, 0.08, 0.10, 0.12, 0.13,
    0.12, 0.10, 0.08, 0.06, 0.05,
    0.04, 0.03, 0.02, 0.01, 0.01
  )
  long_np_delay <- delay_opts(NonParametric(pmf = long_delay_vector))
  stan_args_long <- list(
    data = create_stan_delays(
      delays = long_np_delay,
      time_points = 1L
    )
  )
  expect_warning(
    check_np_delay_lengths(stan_args_long, data_length = 10),
    "non-parametric delay distributions are longer"
  )
})

test_that("check_truncation_length returns invisibly when no truncation", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with no truncation (trunc_id = 0)
  stan_args <- list(
    data = list(
      trunc_id = 0,
      delay_n_np = 1
    )
  )
  expect_invisible(
    check_truncation_length(stan_args, time_points = 10)
  )
})

test_that("check_truncation_length returns invisibly when no nonparametric delays", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with truncation but no non-parametric delays
  stan_args <- list(
    data = list(
      trunc_id = 1,
      delay_n_np = 0
    )
  )
  expect_invisible(
    check_truncation_length(stan_args, time_points = 10)
  )
})

test_that("check_truncation_length does not warn when truncation PMF is shorter than time_points", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with short truncation PMF (length 5) and time_points = 10
  # Create a stan_args structure with truncation as the only delay type
  stan_args <- list(
    data = list(
      trunc_id = 1,
      delay_n_np = 1,
      delay_types_groups = array(c(1, 2)), # Truncation maps to position 1
      delay_np_pmf_groups = array(c(1, 6)) # One PMF of length 5
    )
  )
  expect_no_warning(
    check_truncation_length(stan_args, time_points = 10)
  )
})

test_that("check_truncation_length warns when truncation PMF is longer than time_points", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with long truncation PMF (length 15) and time_points = 10
  stan_args <- list(
    data = list(
      trunc_id = 1,
      delay_n_np = 1,
      delay_types_groups = array(c(1, 2)), # Truncation maps to position 1
      delay_np_pmf_groups = array(c(1, 16)) # One PMF of length 15
    )
  )
  expect_warning(
    check_truncation_length(stan_args, time_points = 10),
    "truncation distribution is longer"
  )
})

test_that("check_truncation_length works with truncation from create_stan_delays", {
  rlang::local_options(rlib_warning_verbosity = "verbose")

  # Short truncation (should NOT warn)
  short_trunc <- trunc_opts(dist = LogNormal(mean = 1, sd = 0.5, max = 5))
  stan_args_short <- list(
    data = create_stan_delays(
      trunc = short_trunc,
      time_points = 10
    )
  )
  expect_no_warning(
    check_truncation_length(stan_args_short, time_points = 10)
  )

  # Long truncation (should warn)
  long_trunc <- trunc_opts(dist = LogNormal(mean = 2, sd = 0.5, max = 20))
  stan_args_long <- list(
    data = create_stan_delays(
      trunc = long_trunc,
      time_points = 10
    )
  )
  expect_warning(
    check_truncation_length(stan_args_long, time_points = 10),
    "truncation distribution is longer"
  )
})

test_that("check_truncation_length works when truncation is combined with other delays", {
  rlang::local_options(rlib_warning_verbosity = "verbose")

  # Create stan_args with generation time, reporting delay, and truncation
  # where only truncation is too long
  gt <- gt_opts(Fixed(5))
  delays <- delay_opts(Fixed(3))
  long_trunc <- trunc_opts(dist = LogNormal(mean = 2, sd = 0.5, max = 20))

  stan_args <- list(
    data = create_stan_delays(
      gt = gt,
      delay = delays,
      trunc = long_trunc,
      time_points = 10
    )
  )

  # Should warn about truncation being too long
  expect_warning(
    check_truncation_length(stan_args, time_points = 10),
    "truncation distribution is longer"
  )
})
