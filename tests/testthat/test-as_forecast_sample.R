skip_if_not_installed("scoringutils", minimum_version = "2.0.0")

# Two posterior samples for each of five dates around a forecast date of
# 2020-01-03, standing in for get_predictions(format = "sample") output
forecast_date <- as.Date("2020-01-03")
sample_predictions <- data.table::data.table(
  forecast_date = forecast_date,
  date = rep(forecast_date + -2:2, each = 2),
  horizon = rep(-2:2, each = 2),
  sample = rep(1:2, 5),
  predicted = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)
# No observation for the last date
observed <- data.frame(date = forecast_date + -2:1, confirm = c(10, 11, 12, 13))

local_predictions <- function(predictions = sample_predictions,
                              env = parent.frame()) {
  local_mocked_bindings(
    get_predictions = function(object, ...) data.table::copy(predictions),
    .env = env
  )
}

fake_fit <- function(class, ...) structure(list(...), class = class)

test_that("as_forecast_sample merges forecasts with observations", {
  local_predictions()
  out <- scoringutils::as_forecast_sample(
    fake_fit("estimate_infections"),
    observations = observed
  )
  expect_s3_class(out, "forecast_sample")
  expect_setequal(
    scoringutils::get_forecast_unit(out), c("forecast_date", "date", "horizon")
  )
  # Horizons before 0 are filtered and the unobserved date is dropped
  expect_equal(sort(unique(out$horizon)), c(0, 1))
  expect_equal(out[horizon == 1]$observed, c(13, 13))
  expect_equal(out[horizon == 1]$predicted, c(7, 8))
  expect_equal(sort(out[horizon == 1]$sample_id), 1:2)
})

test_that("as_forecast_sample horizon sets the lower bound on horizons", {
  local_predictions()
  fit <- fake_fit("estimate_infections")
  all_horizons <- scoringutils::as_forecast_sample(
    fit, observations = observed, horizon = -Inf
  )
  expect_equal(sort(unique(all_horizons$horizon)), -2:1)
  later <- scoringutils::as_forecast_sample(
    fit, observations = observed, horizon = 1
  )
  expect_equal(unique(later$horizon), 1)
})

test_that("as_forecast_sample ignores a user supplied forecast_unit", {
  skip("Known bug, see #1574")
  local_predictions()
  out <- scoringutils::as_forecast_sample(
    fake_fit("estimate_infections"),
    observations = observed, forecast_unit = "date"
  )
  expect_setequal(
    scoringutils::get_forecast_unit(out), c("forecast_date", "date", "horizon")
  )
})

test_that("as_forecast_sample.forecast_secondary scores against secondary", {
  local_predictions()
  fit <- fake_fit("forecast_secondary")
  secondary <- data.frame(date = observed$date, secondary = observed$confirm)
  out <- scoringutils::as_forecast_sample(fit, observations = secondary)
  expect_s3_class(out, "forecast_sample")
  expect_equal(out[horizon == 0]$observed, c(12, 12))
  expect_error(
    scoringutils::as_forecast_sample(fit, observations = observed),
    "secondary"
  )
})

test_that("as_forecast_sample.estimate_truncation keeps all horizons", {
  truncation_predictions <- data.table::rbindlist(list(
    data.table::copy(sample_predictions)[, dataset := 1],
    data.table::copy(sample_predictions)[, dataset := 2]
  ))
  local_predictions(truncation_predictions)
  out <- scoringutils::as_forecast_sample(
    fake_fit("estimate_truncation"),
    observations = observed
  )
  expect_s3_class(out, "forecast_sample")
  expect_setequal(
    scoringutils::get_forecast_unit(out),
    c("dataset", "forecast_date", "date", "horizon")
  )
  expect_equal(sort(unique(out$horizon)), -2:1)
  expect_equal(nrow(out), 2 * 4 * 2)
})

test_that("as_forecast_sample.epinow converts successful runs only", {
  local_predictions()
  out <- scoringutils::as_forecast_sample(
    fake_fit(c("epinow", "list")),
    observations = observed
  )
  expect_s3_class(out, "forecast_sample")
  expect_error(
    scoringutils::as_forecast_sample(
      fake_fit(c("epinow", "list"), error = "model did not converge"),
      observations = observed
    ),
    "model did not converge"
  )
})

test_that("as_forecast_sample errors for bad horizon and observations", {
  local_predictions()
  fit <- fake_fit("estimate_infections")
  expect_error(
    scoringutils::as_forecast_sample(
      fit, observations = observed, horizon = c(0, 1)
    ),
    "horizon"
  )
  expect_error(
    scoringutils::as_forecast_sample(
      fit, observations = observed, horizon = NA_real_
    ),
    "horizon"
  )
  negative <- observed
  negative$confirm[1] <- -1
  expect_error(
    scoringutils::as_forecast_sample(fit, observations = negative),
    ">= 0"
  )
  character_dates <- observed
  character_dates$date <- as.character(character_dates$date)
  expect_error(
    scoringutils::as_forecast_sample(fit, observations = character_dates),
    "date"
  )
})
