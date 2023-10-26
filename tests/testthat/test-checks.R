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
test_col_specs <- function(dt_list, for_estimate_secondary = FALSE) {
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
  test_col_specs(confirm_col_dt, for_estimate_secondary = FALSE)
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
  test_col_specs(date_col_dt, for_estimate_secondary = FALSE)
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
  test_col_specs(primary_col_dt, for_estimate_secondary = TRUE)
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
  test_col_specs(secondary_col_dt, for_estimate_secondary = TRUE)
})
