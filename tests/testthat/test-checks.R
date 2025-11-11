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

# Test check_single_np_pmf_lengths -----------------------------------------

test_that("check_single_np_pmf_lengths returns invisibly when no nonparametric distributions", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with only parametric distributions
  expect_invisible(
    check_single_np_pmf_lengths(
      generation_time = gamma_dist,
      delays = delay_opts(dist = lognormal_dist),
      data = est_inf
    )
  )
})

test_that("check_single_np_pmf_lengths returns invisibly when PMFs are shorter than data", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with short PMF and large data
  expect_invisible(
    check_single_np_pmf_lengths(
      generation_time = short_pmf,
      delays = delay_opts(dist = short_pmf),
      data = est_inf
    )
  )
})

test_that("check_single_np_pmf_lengths returns invisibly when PMFs equal data length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with PMF length equal to data length
  expect_invisible(
    check_single_np_pmf_lengths(
      generation_time = short_pmf,
      data = est_inf[1:5, ]  # 5 rows to match PMF length
    )
  )
})

test_that("check_single_np_pmf_lengths warns when PMF is longer than data", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with long PMF and short data
  expect_warning(
    check_single_np_pmf_lengths(
      generation_time = long_pmf,
      data = est_inf
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})

test_that("check_single_np_pmf_lengths handles multiple long PMFs", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with multiple long PMFs
  expect_warning(
    check_single_np_pmf_lengths(
      generation_time = long_pmf,
      delays = delay_opts(dist = very_long_pmf),
      data = est_inf
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})

test_that("check_single_np_pmf_lengths handles mixed parametric and nonparametric distributions", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with mix of parametric and nonparametric
  expect_warning(
    check_single_np_pmf_lengths(
      generation_time = gamma_dist,  # parametric
      delays = delay_opts(dist = long_pmf),  # nonparametric
      data = est_inf
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})

test_that("check_single_np_pmf_lengths works with single distribution", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with single distribution
  expect_warning(
    check_single_np_pmf_lengths(
      generation_time = long_pmf,
      data = est_inf
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})

# Test check_combined_np_pmf_lengths ---------------------------------------

test_that("check_combined_np_pmf_lengths returns invisibly when delay_np_pmf_length <= data_length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with equal lengths
  stan_args_equal <- list(
    data = list(
      delay_np_pmf_length = 10,
      t = 10
    )
  )
  expect_invisible(check_combined_np_pmf_lengths(stan_args_equal))
  
  # Test with shorter PMF length
  stan_args_shorter <- list(
    data = list(
      delay_np_pmf_length = 5,
      t = 10
    )
  )
  expect_invisible(check_combined_np_pmf_lengths(stan_args_shorter))
})

test_that("check_combined_np_pmf_lengths warns when delay_np_pmf_length > data_length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with longer PMF length
  stan_args_longer <- list(
    data = list(
      delay_np_pmf_length = 15,
      t = 10
    )
  )
  expect_warning(
    check_combined_np_pmf_lengths(stan_args_longer),
    "The combined non-parametric delays PMF is longer than the data"
  )
})

test_that("check_combined_np_pmf_lengths warns with correct length information", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test that warning includes correct length information
  stan_args_longer <- list(
    data = list(
      delay_np_pmf_length = 20,
      t = 10
    )
  )
  expect_warning(
    check_combined_np_pmf_lengths(stan_args_longer),
    "The combined non-parametric delays PMF is longer than the data"
  )
})

# Edge cases and error handling --------------------------------------------

test_that("check_single_np_pmf_lengths handles empty data gracefully", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  empty_data <- data.frame(date = as.Date(character()), confirm = numeric())
  
  # Should warn with empty data and longer PMF
  expect_warning(
    check_single_np_pmf_lengths(
      generation_time = short_pmf,
      data = empty_data
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})

test_that("check_single_np_pmf_lengths handles single row data", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  single_row_data <- data.frame(
    date = as.Date("2020-01-01"),
    confirm = 10
  )
  
  # Should warn with single row data and longer PMF
  expect_warning(
    check_single_np_pmf_lengths(
      generation_time = medium_pmf,
      data = single_row_data
    ),
    "Non-parametric PMFs are longer than the input data"
  )
})

test_that("check_combined_np_pmf_lengths handles missing delay_np_pmf_length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with missing delay_np_pmf_length (should not error)
  stan_args_missing <- list(
    data = list(t = 10)
  )
  
  # Should not error with missing delay_np_pmf_length
  expect_no_error(check_combined_np_pmf_lengths(stan_args_missing))
})

test_that("check_combined_np_pmf_lengths handles zero delay_np_pmf_length", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  # Test with zero delay_np_pmf_length
  stan_args_zero <- list(
    data = list(
      delay_np_pmf_length = 0,
      t = 10
    )
  )
  
  expect_invisible(check_combined_np_pmf_lengths(stan_args_zero))
})
