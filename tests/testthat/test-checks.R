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

test_that("check_np_delay_lengths returns invisibly when PMFs are shorter than data", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with short PMF (length 5) and data length 10
  stan_args <- list(
    data = list(
      delay_n_np = 2,
      delay_np_pmf_groups = array(c(1, 6, 11)) # Two PMFs of length 5 each
    )
  )
  expect_invisible(
    check_np_delay_lengths(stan_args, data_length = 10)
  )
})

test_that("check_np_delay_lengths returns invisibly when PMFs equal data length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with PMF length equal to data length
  stan_args <- list(
    data = list(
      delay_n_np = 1,
      delay_np_pmf_groups = array(c(1, 6)) # One PMF of length 5
    )
  )
  expect_invisible(
    check_np_delay_lengths(stan_args, data_length = 5)
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
