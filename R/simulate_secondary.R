#' Simulate infections using the renewal equation
#'
#' Simulations are done from given initial infections and, potentially
#' time-varying, reproduction numbers. Delays and parameters of the observation
#' model can be specified using the same options as in [estimate_infections()].
#'
#' In order to simulate, all parameters that are specified such as the mean and
#' standard deviation of delays or observation scaling, must be fixed.
#' Uncertain parameters are not allowed.
#'
#' A previous function called [simulate_infections()] that simulates from a
#' given model fit has been renamed [forecast_infections()]. Using
#' [simulate_infections()] with existing estimates is now deprecated. This
#' option will be removed in version 2.1.0.
#' @param primary a data frame of primary reports (column `primary`) by date
#'   (column `date`). Column `primary` must be numeric and `date` must be in
#'   date format.  it will be assumed that `primary` is zero on the missing
#'   days.
#' @inheritParams simulate_infections
#' @inheritParams estimate_secondary
#' @importFrom checkmate assert_data_frame assert_date assert_numeric
#'   assert_subset
#' @return A data.table of simulated secondary observations (column `secondary`)
#'   by date.
#' @export
#' @examples
#' \donttest{
#'   ## load data.table to manipulate `example_confirmed` below
#'   library(data.table)
#'   cases <- as.data.table(example_confirmed)[, primary := confirm]
#'   sim <- simulate_secondary(
#'     cases,
#'     delays = delay_opts(fix_dist(example_reporting_delay)),
#'     obs = obs_opts(family = "poisson")
#'   )
#' }
simulate_secondary <- function(primary,
                               day_of_week_effect = NULL,
                               secondary = secondary_opts(),
                               delays = delay_opts(),
                               truncation = trunc_opts(),
                               obs = obs_opts(),
                               CrIs = c(0.2, 0.5, 0.9),
                               backend = "rstan",
                               ...) {
  ## deprecated usage
  assert_data_frame(primary, any.missing = FALSE)
  assert_subset(c("date", "primary"), colnames(primary))
  assert_date(primary$date)
  assert_numeric(primary$primary, lower = 0)
  assert_numeric(day_of_week_effect, lower = 0, null.ok = TRUE)
  assert_class(secondary, "secondary_opts")
  assert_class(delays, "delay_opts")
  assert_class(truncation, "trunc_opts")
  assert_class(obs, "obs_opts")

  ## create primary values for all dates modelled
  all_dates <- data.table(
    date = seq.Date(min(primary$date), max(primary$date), by = "day")
  )
  primary <- merge.data.table(all_dates, primary, by = "date", all.x = TRUE)
  primary <- primary[, primary := nafill(primary, type = "const", fill = 0)]

  data <- list(
    n = 1,
    t = nrow(primary),
    h = nrow(primary),
    all_dates = 0,
    obs = array(integer(0)),
    primary = array(primary$primary, dim =  c(1, nrow(primary))),
    seeding_time = 0L
  )

  data <- c(data, secondary)

  data <- c(data, create_stan_delays(
    delay = delays,
    trunc = truncation
  ))

  if ((length(data$delay_mean_sd) > 0 && any(data$delay_mean_sd > 0)) ||
      (length(data$delay_sd_sd) > 0 && any(data$delay_sd_sd > 0))) {
    stop(
      "Cannot simulate from uncertain parameters. Use the [fix_dist()] ",
      "function to set the parameters of uncertain distributions either the ",
      "mean or a randomly sampled value"
    )
  }
  data$delay_mean <- array(
    data$delay_mean_mean, dim = c(1, length(data$delay_mean_mean))
  )
  data$delay_sd <- array(
    data$delay_sd_mean, dim = c(1, length(data$delay_sd_mean))
  )
  data$delay_mean_sd <- NULL
  data$delay_sd_sd <- NULL

  data <- c(data, create_obs_model(
    obs, dates = primary$date
  ))

  if (data$obs_scale_sd > 0) {
    stop(
      "Cannot simulate from uncertain observation scaling; use fixed scaling ",
      "instead."
    )
  }
  if (data$obs_scale) {
    data$frac_obs <- array(data$obs_scale_mean, dim = c(1, 1))
  } else {
    data$frac_obs <- array(dim = c(1, 0))
  }
  data$obs_scale_mean <- NULL
  data$obs_scale_sd <- NULL

  if (obs$family == "negbin") {
    if (data$phi_sd > 0) {
      stop(
        "Cannot simulate from uncertain overdispersion; use fixed ",
        "overdispersion instead."
      )
    }
    data$rep_phi <- array(data$phi_mean, dim = c(1, 1))
  } else {
    data$rep_phi <- array(dim = c(1, 0))
  }
  data$phi_mean <- NULL
  data$phi_sd <- NULL

  ## day of week effect
  if (is.null(day_of_week_effect)) {
    day_of_week_effect <- rep(1, data$week_effect)
  }

  day_of_week_effect <- day_of_week_effect / sum(day_of_week_effect)
  data$day_of_week_simplex <- array(
    day_of_week_effect, dim = c(1, data$week_effect)
  )

  # Create stan arguments
  stan <- stan_opts(backend = backend, chains = 1, samples = 1, warmup = 1)
  args <- create_stan_args(
    stan, data = data, fixed_param = TRUE, model = "simulate_secondary",
    verbose = FALSE
  )

  ## simulate
  sim <- fit_model(args, id = "simulate_secondary")

  secondary <- extract_samples(sim, "sim_secondary")$sim_secondary[1, , ]
  out <- data.table(date = all_dates, secondary = secondary)

  return(out[])
}
