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
      delay_types_p = array(c(0)), # Truncation is nonparametric
      delay_types_id = array(c(1)), # ID within nonparametric array
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
      delay_id_truncation = 1,
      delay_n_np = 1,
      delay_types_groups = array(c(1, 2)), # Truncation maps to position 1
      delay_types_p = array(c(0)), # Truncation is nonparametric
      delay_types_id = array(c(1)), # ID within nonparametric array
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
      truncation = short_trunc,
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
      truncation = long_trunc,
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
      generation_time = gt,
      reporting = delays,
      truncation = long_trunc,
      time_points = 10
    )
  )

  # Should warn about truncation being too long
  expect_warning(
    check_truncation_length(stan_args, time_points = 10),
    "truncation distribution is longer"
  )
})

test_that("check_truncation_length correctly indexes when parametric delays precede nonparametric truncation", {
  rlang::local_options(rlib_warning_verbosity = "verbose")

  # Simulate scenario where parametric delays come before nonparametric truncation
  # This tests the fix for the indexing bug where trunc_start was used directly
  # to index into np_pmf_lengths, which only contains nonparametric delays
  stan_args <- list(
    data = list(
      delay_id_truncation = 3,
      delay_n_np = 1,
      delay_types_groups = array(c(1, 2, 3, 4)), # Three delays total
      delay_types_p = array(c(1, 1, 0)), # First two parametric, third nonparametric
      delay_types_id = array(c(1, 2, 1)), # IDs within their respective arrays
      delay_np_pmf_groups = array(c(1, 21)) # One nonparametric PMF of length 20
    )
  )

  # Should warn because truncation PMF (length 20) > time_points (10)
  expect_warning(
    check_truncation_length(stan_args, time_points = 10),
    "truncation distribution is longer"
  )

  # Should not warn when time_points is larger
  expect_no_warning(
    check_truncation_length(stan_args, time_points = 25)
  )
})
