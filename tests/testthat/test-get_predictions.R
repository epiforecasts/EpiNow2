skip_on_cran()

# Two dates of three samples each. The last observed date is 2020-01-02, so
# the second date is one day ahead of it.
obs_dates <- as.Date("2020-01-01") + 0:1
prediction_samples <- function(variable = "reported_cases") {
  data.table(
    variable = variable,
    date = rep(obs_dates + 1, each = 3),
    sample = rep(1:3, 2),
    value = c(1, 2, 3, 10, 20, 30)
  )
}

expect_sample_predictions <- function(preds, forecast_date) {
  expect_named(
    preds, c("forecast_date", "date", "horizon", "sample", "predicted")
  )
  expect_equal(nrow(preds), 6)
  expect_equal(unique(preds$forecast_date), forecast_date)
  expect_equal(preds$horizon, as.numeric(preds$date - forecast_date))
  expect_equal(preds$predicted, c(1, 2, 3, 10, 20, 30))
}

expect_quantile_predictions <- function(preds, forecast_date) {
  expect_named(
    preds,
    c("forecast_date", "date", "horizon", "quantile_level", "predicted")
  )
  expect_equal(preds$quantile_level, rep(c(0, 0.5, 1), 2))
  expect_equal(preds$predicted, c(1, 2, 3, 10, 20, 30))
  expect_equal(unique(preds$forecast_date), forecast_date)
}

test_that("get_predictions.forecast_infections returns samples and quantiles", {
  samples <- rbind(
    prediction_samples(),
    prediction_samples("R")[, value := -1]
  )
  obj <- structure(
    list(samples = samples, observations = data.table(date = obs_dates)),
    class = "forecast_infections"
  )

  expect_sample_predictions(
    get_predictions(obj, format = "sample"), max(obs_dates)
  )
  expect_quantile_predictions(
    get_predictions(obj, format = "quantile", quantiles = c(0, 0.5, 1)),
    max(obs_dates)
  )
})

test_that("get_predictions.forecast_secondary uses the last observed date", {
  obj <- structure(
    list(
      samples = prediction_samples("sim_secondary"),
      observations = data.table(
        date = c(obs_dates, max(obs_dates) + 1),
        secondary = c(5, 6, NA)
      )
    ),
    class = "forecast_secondary"
  )

  expect_sample_predictions(
    get_predictions(obj, format = "sample"), max(obs_dates)
  )
  expect_quantile_predictions(
    get_predictions(obj, format = "quantile", quantiles = c(0, 0.5, 1)),
    max(obs_dates)
  )

  # with no observed secondary data the last date is used instead
  obj$observations$secondary <- NA_real_
  preds <- get_predictions(obj, format = "sample")
  expect_equal(unique(preds$forecast_date), max(obs_dates) + 1)
  expect_equal(unique(preds$horizon), c(-1, 0))
})

test_that("get_predictions.estimate_secondary returns samples and quantiles", {
  local_mocked_bindings(get_samples = function(object, ...) {
    rbind(
      prediction_samples("sim_secondary"),
      prediction_samples("phi")[, value := -1]
    )
  })
  obj <- structure(
    list(observations = data.table(date = obs_dates)),
    class = c("estimate_secondary", "epinowfit")
  )

  expect_sample_predictions(
    get_predictions(obj, format = "sample"), max(obs_dates)
  )
  expect_quantile_predictions(
    get_predictions(obj, format = "quantile", quantiles = c(0, 0.5, 1)),
    max(obs_dates)
  )
})
