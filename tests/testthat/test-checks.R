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
  pmf <- c(0.4, 0.30, 0.20, 0.05,  0.049995, 4.5e-06, rep(1e-7, 5))
  expect_warning(
    check_sparse_pmf_tail(pmf),
    "PMF tail has"
  )
})

test_that("test_data_complete detects complete and incomplete data", {
  # example_confirmed with explicit missing dates
  ec_missing_date <- copy(example_confirmed)[c(1, 3), date := NA]
  # example_confirmed with explicit missing confirm
  ec_missing_confirm <- copy(example_confirmed)[c(1, 3), confirm := NA]
  # example_confirmed with implicit missing (missing entries)
  ec_implicit_missing <- copy(example_confirmed)[-c(1,3,5), ]
  # Create a hypothetical complete example_secondary
  es <- copy(example_confirmed)[
    , primary := confirm
  ][
    , secondary := primary * 0.4
  ]
  # example_secondary with explicit missing primary
  es_missing_primary <- copy(es)[c(1, 3), primary := NA]
  # example_secondary with explicit missing secondary
  es_missing_secondary <- copy(es)[c(1, 3), secondary := NA]
  
  # Expectations
  expect_true(test_data_complete(example_confirmed))
  expect_true(test_data_complete(es))
  expect_false(test_data_complete(ec_missing_date))
  expect_false(test_data_complete(ec_missing_confirm))
  expect_false(test_data_complete(es_missing_primary))
  expect_false(test_data_complete(es_missing_secondary))
  expect_false(test_data_complete(ec_implicit_missing))
})

test_that("check_na_setting_against_data works as expected", {
  # If data is incomplete and the default na = "missing" is being used,
  # expect a message thrown once every 8 hours.
  # NB: We change the local setting here to throw the message on demand, rather
  # than every 8 hours, for the sake of multiple runs of the test within
  # 8 hours.
  rlang::local_options(rlib_message_verbosity = "verbose")
  expect_message(
    check_na_setting_against_data(
      obs = obs_opts(),
      data = copy(example_confirmed)[c(1, 3), confirm := NA]
    ),
    "version 1.5.0 missing dates or dates"
  )
  # If data is incomplete but the user explicitly set na = "missing", then
  # expect no message
  expect_no_message(
    check_na_setting_against_data(
      obs = obs_opts(na = "missing"),
      data = copy(example_confirmed)[c(1, 3), confirm := NA]
    )
  )
  # If data is complete, expect no message even when using default na as
  # missing setting
  expect_no_message(
    check_na_setting_against_data(
      obs = obs_opts(),
      data = example_confirmed
    )
  )
})
