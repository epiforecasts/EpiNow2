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

# Test data for PMF length checks --------------------------------------------

# Create nonparametric distributions of various lengths
# Note: For generation time, first element must be 0
short_pmf <- NonParametric(c(0, 0.3, 0.4, 0.2, 0.1))  # length 5
medium_pmf <- NonParametric(c(0, 0.2, 0.3, 0.2, 0.1, 0.05, 0.05, 0.1))  # length 8
long_pmf <- NonParametric(c(0, rep(0.1/14, 14)))  # length 15, first element 0
very_long_pmf <- NonParametric(c(0, rep(0.05/24, 24)))  # length 25, first element 0

# Create parametric distributions for comparison
gamma_dist <- Gamma(shape = 2, rate = 1, max = 10)
lognormal_dist <- LogNormal(meanlog = 1, sdlog = 0.5, max = 10)

test_that("check_np_delay_lengths returns invisibly when no nonparametric distributions", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with only parametric distributions
  stan_args <- list(
    data = list(
      delay_n_np = 0,
      t = 10
    )
  )
  expect_invisible(
    check_np_delay_lengths(
      stan_args,
      gt = gamma_dist,
      delay = delay_opts(dist = lognormal_dist)
    )
  )
})

test_that("check_np_delay_lengths returns invisibly when PMFs are shorter than data", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with short PMF (length 5) and data length 10
  stan_args <- list(
    data = list(
      delay_n_np = 2,
      t = 10,
      delay_np_pmf_groups = array(c(1, 6, 11))  # Two PMFs of length 5 each
    )
  )
  expect_invisible(
    check_np_delay_lengths(
      stan_args,
      gt = short_pmf,
      delay = delay_opts(dist = short_pmf)
    )
  )
})

test_that("check_np_delay_lengths returns invisibly when PMFs equal data length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with PMF length equal to data length
  stan_args <- list(
    data = list(
      delay_n_np = 1,
      t = 5,
      delay_np_pmf_groups = array(c(1, 6))  # One PMF of length 5
    )
  )
  expect_invisible(
    check_np_delay_lengths(
      stan_args,
      gt = short_pmf
    )
  )
})

test_that("check_np_delay_lengths warns when PMF is longer than data", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with long PMF (length 15) and short data (length 10)
  stan_args <- list(
    data = list(
      delay_n_np = 1,
      t = 10,
      delay_np_pmf_groups = array(c(1, 16))  # One PMF of length 15
    )
  )
  expect_warning(
    check_np_delay_lengths(
      stan_args,
      gt = long_pmf
    ),
    "Non-parametric delay distributions are longer than the input data"
  )
})

test_that("check_np_delay_lengths handles multiple long PMFs", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with multiple long PMFs
  stan_args <- list(
    data = list(
      delay_n_np = 2,
      t = 10,
      delay_np_pmf_groups = array(c(1, 16, 41))  # Two PMFs: length 15 and 25
    )
  )
  expect_warning(
    check_np_delay_lengths(
      stan_args,
      gt = long_pmf,
      delay = delay_opts(dist = very_long_pmf)
    ),
    "Non-parametric delay distributions are longer than the input data"
  )
})

test_that("check_np_delay_lengths handles mixed parametric and nonparametric distributions", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with mix of parametric and nonparametric
  stan_args <- list(
    data = list(
      delay_n_np = 1,
      t = 10,
      delay_np_pmf_groups = array(c(1, 16))  # One NP PMF of length 15
    )
  )
  expect_warning(
    check_np_delay_lengths(
      stan_args,
      gt = gamma_dist,  # parametric
      delay = delay_opts(dist = long_pmf)  # nonparametric
    ),
    "Non-parametric delay distributions are longer than the input data"
  )
})

test_that("check_np_delay_lengths works with single distribution", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with single distribution
  stan_args <- list(
    data = list(
      delay_n_np = 1,
      t = 10,
      delay_np_pmf_groups = array(c(1, 16))  # One PMF of length 15
    )
  )
  expect_warning(
    check_np_delay_lengths(
      stan_args,
      gt = long_pmf
    ),
    "Non-parametric delay distributions are longer than the input data"
  )
})

test_that("check_np_delay_lengths handles empty data gracefully", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with empty data (t = 0) and longer PMF
  stan_args <- list(
    data = list(
      delay_n_np = 1,
      t = 0,
      delay_np_pmf_groups = array(c(1, 6))  # One PMF of length 5
    )
  )

  expect_warning(
    check_np_delay_lengths(
      stan_args,
      gt = short_pmf
    ),
    "Non-parametric delay distributions are longer than the input data"
  )
})

test_that("check_np_delay_lengths handles single row data", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with single row data (t = 1) and longer PMF
  stan_args <- list(
    data = list(
      delay_n_np = 1,
      t = 1,
      delay_np_pmf_groups = array(c(1, 9))  # One PMF of length 8
    )
  )

  expect_warning(
    check_np_delay_lengths(
      stan_args,
      gt = medium_pmf
    ),
    "Non-parametric delay distributions are longer than the input data"
  )
})

test_that("check_np_delay_lengths handles missing delay_n_np", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with missing delay_n_np (should not error)
  stan_args <- list(
    data = list(t = 10)
  )

  expect_no_error(check_np_delay_lengths(stan_args, gt = short_pmf))
})

test_that("check_np_delay_lengths handles zero delay_n_np", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with zero delay_n_np
  stan_args <- list(
    data = list(
      delay_n_np = 0,
      t = 10
    )
  )

  expect_invisible(check_np_delay_lengths(stan_args, gt = gamma_dist))
})
