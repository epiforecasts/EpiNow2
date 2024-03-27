#' Estimate a Secondary Observation from a Primary Observation
#'
#' @description `r lifecycle::badge("stable")`
#' Estimates the relationship between a primary and secondary observation, for
#' example hospital admissions and deaths or hospital admissions and bed
#' occupancy. See [secondary_opts()] for model structure options. See parameter
#' documentation for model defaults and options. See the examples for case
#' studies using synthetic data and
#' [here](https://gist.github.com/seabbs/4f09d7609df298db7a86c31612ff9d17)
#' for an example of forecasting Covid-19 deaths from Covid-19 cases. See
#' [here](https://gist.github.com/seabbs/4dad3958ca8d83daca8f02b143d152e6) for
#' a prototype function that may be used to estimate and forecast a secondary
#' observation from a primary across multiple regions and
#' [here](https://github.com/epiforecasts/covid.german.forecasts/blob/master/rt-forecast/death-from-cases.R) # nolint
#' for an application forecasting Covid-19 deaths in Germany and Poland.
#'
#' @param secondary A call to [secondary_opts()] or a list containing the
#' following  binary variables: cumulative, historic, primary_hist_additive,
#' current, primary_current_additive. These parameters control the structure of
#' the secondary model, see [secondary_opts()] for details.
#'
#' @param delays A call to [delay_opts()] defining delay distributions between
#' primary and secondary observations See the documentation of [delay_opts()]
#' for details. By default a diffuse prior  is assumed with a mean of 14 days
#' and standard deviation of 7 days (with a standard deviation of 0.5 and 0.25
#' respectively on the log scale).
#'
#' @param reports A `<data.frame>` containing the `date` of report and both
#' `primary` and `secondary` reports.
#'
#' @param model A compiled stan model to override the default model. May be
#' useful for package developers or those developing extensions.
#'
#' @param priors A `<data.frame>` of named priors to be used in model fitting
#' rather than the defaults supplied from other arguments. This is typically
#' useful if wanting to inform an estimate from the posterior of another model
#' fit.
#'
#' @param burn_in Integer, defaults to 14 days. The number of data points to
#' use for estimation but not to fit to at the beginning of the time series.
#' This must be less than the number of observations.
#'
#' @param weigh_delay_priors Logical. If TRUE, all delay distribution priors
#' will be weighted by the number of observation data points, in doing so
#' approximately placing an independent prior at each time step and usually
#' preventing the posteriors from shifting. If FALSE (default), no weight will
#' be applied, i.e. delay distributions will be treated as a single
#' parameters.
#'
#' @param verbose Logical, should model fitting progress be returned. Defaults
#' to [interactive()].
#'
#' @param ... Additional parameters to pass to [stan_opts()].
#'
#' @return A list containing: `predictions` (a `<data.frame>` ordered by date
#' with the primary, and secondary observations, and a summary of the model
#' estimated secondary observations), `posterior` which contains a summary of
#' the entire model posterior, `data` (a list of data used to fit the
#' model), and `fit` (the `stanfit` object).
#' @export
#' @inheritParams estimate_infections
#' @inheritParams update_secondary_args
#' @inheritParams calc_CrIs
#' @importFrom rstan sampling
#' @importFrom lubridate wday
#' @importFrom data.table as.data.table merge.data.table nafill
#' @importFrom utils modifyList
#' @importFrom checkmate assert_class assert_numeric assert_data_frame
#' assert_logical
#' @examples
#' \donttest{
#' # set number of cores to use
#' old_opts <- options()
#' options(mc.cores = ifelse(interactive(), 4, 1))
#'
#' # load data.table for manipulation
#' library(data.table)
#'
#' #### Incidence data example ####
#'
#' # make some example secondary incidence data
#' cases <- example_confirmed
#' cases <- as.data.table(cases)[, primary := confirm]
#' # Assume that only 40 percent of cases are reported
#' cases[, scaling := 0.4]
#' # Parameters of the assumed log normal delay distribution
#' cases[, meanlog := 1.8][, sdlog := 0.5]
#'
#' # Simulate secondary cases
#' cases <- convolve_and_scale(cases, type = "incidence")
#' #
#' # fit model to example data specifying a weak prior for fraction reported
#' # with a secondary case
#' inc <- estimate_secondary(cases[1:60],
#'   obs = obs_opts(scale = list(mean = 0.2, sd = 0.2), week_effect = FALSE)
#' )
#' plot(inc, primary = TRUE)
#'
#' # forecast future secondary cases from primary
#' inc_preds <- forecast_secondary(
#'   inc, cases[seq(61, .N)][, value := primary]
#' )
#' plot(inc_preds, new_obs = cases, from = "2020-05-01")
#'
#' #### Prevalence data example ####
#'
#' # make some example prevalence data
#' cases <- example_confirmed
#' cases <- as.data.table(cases)[, primary := confirm]
#' # Assume that only 30 percent of cases are reported
#' cases[, scaling := 0.3]
#' # Parameters of the assumed log normal delay distribution
#' cases[, meanlog := 1.6][, sdlog := 0.8]
#'
#' # Simulate secondary cases
#' cases <- convolve_and_scale(cases, type = "prevalence")
#'
#' # fit model to example prevalence data
#' prev <- estimate_secondary(cases[1:100],
#'   secondary = secondary_opts(type = "prevalence"),
#'   obs = obs_opts(
#'     week_effect = FALSE,
#'     scale = list(mean = 0.4, sd = 0.1)
#'   )
#' )
#' plot(prev, primary = TRUE)
#'
#' # forecast future secondary cases from primary
#' prev_preds <- forecast_secondary(
#'  prev, cases[seq(101, .N)][, value := primary]
#' )
#' plot(prev_preds, new_obs = cases, from = "2020-06-01")
#'
#' options(old_opts)
#' }
estimate_secondary <- function(reports,
                               secondary = secondary_opts(),
                               delays = delay_opts(
                                 LogNormal(
                                   meanlog = Normal(2.5, 0.5),
                                   sdlog = Normal(0.47, 0.25),
                                   max = 30
                                 )
                               ),
                               truncation = trunc_opts(),
                               obs = obs_opts(),
                               stan = stan_opts(),
                               burn_in = 14,
                               CrIs = c(0.2, 0.5, 0.9),
                               filter_leading_zeros = FALSE,
                               zero_threshold = Inf,
                               priors = NULL,
                               model = NULL,
                               weigh_delay_priors = FALSE,
                               verbose = interactive(),
                               ...) {
  # Validate the inputs
  check_reports_valid(reports, model = "estimate_secondary")
  assert_class(secondary, "secondary_opts")
  assert_class(delays, "delay_opts")
  assert_class(truncation, "trunc_opts")
  assert_class(obs, "obs_opts")
  assert_numeric(burn_in, lower = 0)
  assert_numeric(CrIs, lower = 0, upper = 1)
  assert_logical(filter_leading_zeros)
  assert_numeric(zero_threshold, lower = 0)
  assert_data_frame(priors, null.ok = TRUE)
  assert_class(model, "stanfit", null.ok = TRUE)
  assert_logical(weigh_delay_priors)
  assert_logical(verbose)

  reports <- data.table::as.data.table(reports)
  secondary_reports <- reports[, list(date, confirm = secondary)]
  secondary_reports <- create_clean_reported_cases(
    secondary_reports,
    filter_leading_zeros = filter_leading_zeros,
    zero_threshold = zero_threshold
  )
  ## fill in missing data (required if fitting to prevalence)
  complete_secondary <- create_complete_cases(secondary_reports)

  ## fill down
  secondary_reports[, confirm := nafill(confirm, type = "locf")]
  ## fill any early data up
  secondary_reports[, confirm := nafill(confirm, type = "nocb")]

  # Ensure that reports and secondary_reports are aligned
  reports <- merge.data.table(
    reports, secondary_reports[, list(date)], by = "date"
  )

  if (burn_in >= nrow(reports)) {
    stop("burn_in is greater or equal to the number of observations.
         Some observations must be used in fitting")
  }
  # observation and control data
  data <- list(
    t = nrow(reports),
    primary = reports$primary,
    obs = secondary_reports$confirm,
    obs_time = complete_secondary[lookup > burn_in]$lookup - burn_in,
    lt = sum(complete_secondary$lookup > burn_in),
    burn_in = burn_in,
    seeding_time = 0
  )
  # secondary model options
  data <- c(data, secondary)
  # delay data
  data <- c(data, create_stan_delays(
    delay = delays,
    trunc = truncation,
    weight = ifelse(weigh_delay_priors, data$t, 1)
  ))

  # observation model data
  data <- c(data, create_obs_model(obs, dates = reports$date))

  # update data to use specified priors rather than defaults
  data <- update_secondary_args(data, priors = priors, verbose = verbose)

  # initial conditions (from estimate_infections)
  inits <- create_initial_conditions(
    c(data, list(estimate_r = 0, fixed = 1, bp_n = 0))
  )
  # fit
  args <- create_stan_args(
    stan = stan, data = data, init = inits, model = "estimate_secondary"
  )
  fit <- fit_model(args, id = "estimate_secondary")

  out <- list()
  out$predictions <- extract_stan_param(fit, "sim_secondary", CrIs = CrIs)
  out$predictions <- out$predictions[, lapply(.SD, round, 1)]
  out$predictions <- out$predictions[, date := reports[(burn_in + 1):.N]$date]
  out$predictions <- data.table::merge.data.table(
    reports, out$predictions,
    all = TRUE, by = "date"
  )
  out$posterior <- extract_stan_param(
    fit,
    CrIs = CrIs
  )
  out$data <- data
  out$fit <- fit
  class(out) <- c("estimate_secondary", class(out))
  return(out)
}

#' Update estimate_secondary default priors
#'
#' @description `r lifecycle::badge("stable")`
#' This functions allows the user to more easily specify data driven or model
#'   based priors for [estimate_secondary()] from example from previous model
#'   fits using a `<data.frame>` to overwrite other default settings. Note that
#'   default settings are still required.
#'
#' @param data A list of data and arguments as returned by `create_stan_data()`.
#'
#' @param priors A `<data.frame>` of named priors to be used in model fitting
#'   rather than the defaults supplied from other arguments. This is typically
#'   useful if wanting to inform a estimate from the posterior of another model
#'   fit. Priors that are currently use to update the defaults are the scaling
#'   fraction ("frac_obs"), and delay parameters ("delay_params"). The
#'   `<data.frame>` should have the following variables: `variable`, `mean`, and
#'   `sd`.
#'
#' @return A list as produced by `create_stan_data()`.
#' @export
#' @inheritParams create_stan_args
#' @importFrom data.table as.data.table
#' @examples
#' priors <- data.frame(variable = "frac_obs", mean = 3, sd = 1)
#' data <- list(obs_scale_mean = 4, obs_scale_sd = 3)
#' update_secondary_args(data, priors)
update_secondary_args <- function(data, priors, verbose = TRUE) {
  priors <- data.table::as.data.table(priors)
  if (!missing(priors) && !is.null(priors) && nrow(priors) > 0) {
    if (verbose) {
      message(
        "Replacing specified priors with those from the passed in prior dataframe" # nolint
      )
    }
    # replace scaling if present in the prior
    scale <- priors[grepl("frac_obs", variable, fixed = TRUE)]
    if (nrow(scale) > 0) {
      data$obs_scale_mean <- as.array(signif(scale$mean, 3))
      data$obs_scale_sd <- as.array(signif(scale$sd, 3))
    }
    # replace delay parameters if present
    delay_params <- priors[grepl("delay_params", variable, fixed = TRUE)]
    if (nrow(delay_params) > 0) {
      if (is.null(data$delay_params_mean)) {
        warning(
          "Cannot replace delay distribution parameters as no default has been set" # nolint
        )
      }
      data$delay_params_mean <- as.array(signif(delay_params$mean, 3))
      data$delay_params_sd <- as.array(signif(delay_params$sd, 3))
    }
    phi <- priors[grepl("rep_phi", variable, fixed = TRUE)]
    if (nrow(phi) > 0) {
      data$phi_mean <- signif(phi$mean, 3)
      data$phi_sd <- signif(phi$sd, 3)
    }
  }
  return(data)
}

#' Plot method for estimate_secondary
#'
#' @description `r lifecycle::badge("experimental")`
#' `plot` method for class "estimate_secondary".
#'
#' @param x A list of output as produced by `estimate_secondary`
#'
#' @param primary Logical, defaults to `FALSE`. Should `primary` reports also
#' be plot?
#'
#' @param from Date object indicating when to plot from.
#'
#' @param to Date object indicating when to plot up to.
#'
#' @param new_obs A `<data.frame>` containing the columns `date` and `secondary`
#' which replace the secondary observations stored in the `estimate_secondary`
#' output.
#'
#' @param ... Pass additional arguments to plot function. Not currently in use.
#'
#' @return A `ggplot` object.
#'
#' @seealso plot estimate_secondary
#' @method plot estimate_secondary
#' @importFrom ggplot2 ggplot aes geom_col geom_point labs scale_x_date
#' @importFrom ggplot2 scale_y_continuous theme theme_bw
#' @importFrom data.table as.data.table merge.data.table
#' @export
plot.estimate_secondary <- function(x, primary = FALSE,
                                    from = NULL, to = NULL,
                                    new_obs = NULL,
                                    ...) {
  predictions <- data.table::copy(x$predictions)[!is.na(secondary)]

  if (!is.null(new_obs)) {
    new_obs <- data.table::as.data.table(new_obs)
    new_obs <- new_obs[, .(date, secondary)]
    predictions <- predictions[, secondary := NULL]
    predictions <- data.table::merge.data.table(
      predictions, new_obs, all = TRUE, by = "date"
    )
  }
  if (!is.null(from)) {
    predictions <- predictions[date >= from]
  }
  if (!is.null(to)) {
    predictions <- predictions[date <= to]
  }

  plot <- ggplot2::ggplot(predictions, ggplot2::aes(x = date, y = secondary)) +
    ggplot2::geom_col(
      fill = "grey", col = "white",
      show.legend = FALSE, na.rm = TRUE
    )

  if (primary) {
    plot <- plot +
      ggplot2::geom_point(
        data = predictions,
        ggplot2::aes(y = primary),
        alpha = 0.4, size = 0.8
      ) +
      ggplot2::geom_line(
        data = predictions,
        ggplot2::aes(y = primary), alpha = 0.4
      )
  }
  plot <- plot_CrIs(plot, extract_CrIs(predictions),
    alpha = 0.6, linewidth = 1
  )
  plot <- plot +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Reports per day", x = "Date") +
    ggplot2::scale_x_date(date_breaks = "week", date_labels = "%b %d") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  return(plot)
}

#' Convolve and scale a time series
#'
#' This applies a lognormal convolution with given, potentially time-varying
#' parameters representing the parameters of the lognormal distribution used for
#' the convolution and an optional scaling factor. This is akin to the model
#' used in [estimate_secondary()] and [simulate_secondary()].
#'
#' Up to version 1.4.0 this function was called [simulate_secondary()].
#'
#' @param data A `<data.frame>` containing the `date` of report and `primary`
#' cases as a numeric vector.
#'
#' @param family Character string defining the observation model. Options are
#' Negative binomial ("negbin"), the default, Poisson ("poisson"), and "none"
#' meaning the expectation is returned.
#'
#' @param delay_max Integer, defaulting to 30 days. The maximum delay used in
#' the convolution model.
#'
#' @param ... Additional parameters to pass to the observation model (i.e
#'  `rnbinom` or `rpois`).
#'
#' @return A `<data.frame>` containing simulated data in the format required by
#' [estimate_secondary()].
#'
#' @seealso estimate_secondary
#' @inheritParams secondary_opts
#' @importFrom data.table as.data.table copy shift
#' @importFrom purrr pmap_dbl
#' @importFrom rlang arg_match
#' @export
#' @examples
#' # load data.table for manipulation
#' library(data.table)
#'
#' #### Incidence data example ####
#'
#' # make some example secondary incidence data
#' cases <- example_confirmed
#' cases <- as.data.table(cases)[, primary := confirm]
#'
#' # Assume that only 40 percent of cases are reported
#' cases[, scaling := 0.4]
#'
#' # Parameters of the assumed log normal delay distribution
#' cases[, meanlog := 1.8][, sdlog := 0.5]
#'
#' # Simulate secondary cases
#' cases <- convolve_and_scale(cases, type = "incidence")
#' cases
#' #### Prevalence data example ####
#'
#' # make some example prevalence data
#' cases <- example_confirmed
#' cases <- as.data.table(cases)[, primary := confirm]
#'
#' # Assume that only 30 percent of cases are reported
#' cases[, scaling := 0.3]
#'
#' # Parameters of the assumed log normal delay distribution
#' cases[, meanlog := 1.6][, sdlog := 0.8]
#'
#' # Simulate secondary cases
#' cases <- convolve_and_scale(cases, type = "prevalence")
#' cases
convolve_and_scale <- function(data, type = c("incidence", "prevalence"),
                               family = c("none", "poisson", "negbin"),
                               delay_max = 30, ...) {
  type <- arg_match(type)
  family <- arg_match(family)
  data <- data.table::as.data.table(data)
  data <- data.table::copy(data)
  data <- data[, index := seq_len(.N)]
  # apply scaling
  data <- data[, scaled := scaling * primary]
  # add convolution
  data <- data[
    ,
    conv := purrr::pmap_dbl(
      list(i = index, m = meanlog, s = sdlog),
      function(i, m, s) {
        discretised_lognormal_pmf_conv(
          scaled[max(1, i - delay_max - 1):i],
          meanlog = m, sdlog = s
        )
      }
    )
  ]
  # build model
  if (type == "incidence") {
    data <- data[, secondary := conv]
  } else if (type == "prevalence") {
    data <- data[1, secondary := scaled]
    for (i in 2:nrow(data)) {
      index <-
        data[c(i - 1, i)][, secondary := shift(secondary, 1) - conv]
      index <- index[secondary < 0, secondary := 0]
      data[i, ] <- index[2][, secondary := secondary + scaled]
    }
  }
  # check secondary is greater that zero
  data <- data[secondary < 0, secondary := 0]
  data <- data[!is.na(secondary)]
  # apply observation model
  if (family == "poisson") {
    data <- data[, secondary := purrr::map_dbl(secondary, ~ rpois(1, .))]
  } else if (family == "negbin") {
    data <- data[, secondary := purrr::map_dbl(
      secondary, ~ rnbinom(1, mu = .), ...
    )]
  }
  data <- data[, secondary := as.integer(secondary)]
  return(data[])
}

#' Forecast Secondary Observations Given a Fit from estimate_secondary
#'
#' @description `r lifecycle::badge("experimental")`
#' This function forecasts secondary observations using the output of
#' [estimate_secondary()] and either observed primary data or a forecast of
#' primary observations. See the examples of [estimate_secondary()]
#' for one use case. It can also be combined with [estimate_infections()] to
#' produce a forecast for a secondary observation from a forecast of a primary
#' observation. See the examples of [estimate_secondary()] for
#' example use cases on synthetic data. See
#' [here](https://gist.github.com/seabbs/4f09d7609df298db7a86c31612ff9d17)
#' for an example of forecasting Covid-19 deaths from Covid-19 cases.
#'
#' @param estimate An object of class "estimate_secondary" as produced by
#' [estimate_secondary()].
#'
#' @param primary A `<data.frame>` containing at least `date` and `value`
#' (integer) variables and optionally `sample`. Used as the primary observation
#' used to forecast the secondary observations. Alternatively, this may be an
#' object of class "estimate_infections" as produced by `estimate_infections()`.
#' If `primary` is of class "estimate_infections" then the internal samples will
#' be filtered to have a minimum date ahead of those observed in the `estimate`
#' object.
#'
#' @param primary_variable A character string indicating the primary variable,
#' defaulting to "reported_cases". Only used when primary is of class
#' `<estimate_infections>`.
#'
#' @param model A compiled stan model as returned by [rstan::stan_model()].
#'
#' @param samples Numeric, number of posterior samples to simulate from. The
#' default is to use all samples in the `primary` input when present. If not
#' present the default is to use 1000 samples.
#'
#' @param all_dates Logical, defaults to FALSE. Should a forecast for all dates
#' and not just those in the forecast horizon be returned.
#'
#' @return A list containing: `predictions` (a `<data.frame>` ordered by date
#' with the primary, and secondary observations, and a summary of the forecast
#' secondary observations. For primary observations in the forecast horizon
#' when uncertainty is present the median is used), `samples` a `<data.frame>`
#' of forecast secondary observation posterior samples, and `forecast` a summary
#' of the forecast secondary observation posterior.
#'
#' @importFrom rstan extract sampling
#' @importFrom data.table rbindlist merge.data.table as.data.table setorderv
#' @importFrom data.table setcolorder copy
#' @importFrom lubridate days wday
#' @importFrom utils tail
#' @importFrom purrr map
#' @inheritParams estimate_secondary
#' @inheritParams stan_opts
#' @seealso [estimate_secondary()]
#' @export
forecast_secondary <- function(estimate,
                               primary,
                               primary_variable = "reported_cases",
                               model = NULL,
                               backend = "rstan",
                               samples = NULL,
                               all_dates = FALSE,
                               CrIs = c(0.2, 0.5, 0.9)) {
  ## deal with input if data frame
  if (inherits(primary, "data.frame")) {
    primary <- data.table::as.data.table(primary)
    if (is.null(primary$sample)) {
      if (is.null(samples)) {
        samples <- 1000
      }
      primary <- primary[, .(date, sample = list(1:samples), value)]
      primary <- primary[,
       .(sample = as.numeric(unlist(sample))), by = c("date", "value")
      ]
    }
    primary <- primary[, .(date, sample, value)]
  }
  if (inherits(primary, "estimate_infections")) {
    primary <- data.table::as.data.table(
      primary$samples[variable == primary_variable]
    )
    primary <- primary[date > max(estimate$predictions$date, na.rm = TRUE)]
    primary <- primary[, .(date, sample, value)]
    if (!is.null(samples)) {
      primary <- primary[sample(seq_len(.N), samples, replace = TRUE)]
    }
  }
  ## rename to avoid conflict with estimate
  updated_primary <- primary

  ## extract samples from given stanfit object
  draws <- extract_samples(estimate$fit,
    pars = c(
      "sim_secondary", "log_lik",
      "lp__", "secondary"
    ),
    include = FALSE
  )
  # extract data from stanfit
  data <- estimate$data

  # combined primary from data and input primary
  primary_fit <- estimate$predictions[,
   .(date, value = primary, sample = list(unique(updated_primary$sample)))
  ]
  primary_fit <- primary_fit[date <= min(primary$date, na.rm = TRUE)]
  primary_fit <- primary_fit[,
   .(sample = as.numeric(unlist(sample))), by = c("date", "value")
  ]
  primary_fit <- data.table::rbindlist(
    list(primary_fit, updated_primary), use.names = TRUE
  )
  data.table::setorderv(primary_fit, c("sample", "date"))

  # update data with primary samples and day of week
  data$primary <- t(
    matrix(primary_fit$value, ncol = length(unique(primary_fit$sample)))
  )
  data$day_of_week <- add_day_of_week(
    unique(primary_fit$date), data$week_effect
  )
  data$n <- nrow(data$primary)
  data$t <- ncol(data$primary)
  data$h <- nrow(primary[sample == min(sample)])

  # extract samples for posterior of estimates
  posterior_samples <- sample(seq_len(data$n), data$n, replace = TRUE) # nolint
  draws <- purrr::map(draws, ~ as.matrix(.[posterior_samples, ]))
  # combine with data
  data <- c(data, draws)

  # allocate empty parameters
  data <- allocate_empty(
    data, c("frac_obs", "delay_param", "rep_phi"),
    n = data$n
  )
  data$all_dates <- as.integer(all_dates)

  ## simulate
  args <- create_stan_args(
    stan_opts(
      model = model, backend = backend, chains = 1, samples = 1, warmup = 1
    ),
    data = data, fixed_param = TRUE, model = "simulate_secondary"
  )

  sims <- fit_model(args, id = "simulate_secondary")

  # extract samples and organise
  dates <- unique(primary_fit$date)
  samples <- extract_samples(sims, "sim_secondary")$sim_secondary
  samples <- as.data.table(samples)
  colnames(samples) <- c("iterations", "sample", "time", "value")
  samples <- samples[, c("iterations", "time") := NULL]
  samples <- samples[,
    date := rep(tail(dates, ifelse(all_dates, data$t, data$h)), data$n)
  ]

  # summarise samples
  summarised <- calc_summary_measures(samples,
    summarise_by = "date",
    CrIs = CrIs
  )
  summarised <- summarised[, purrr::map(.SD, round, digits = 1)]

  # construct output
  out <- list()
  out$samples <- samples
  out$forecast <- summarised
  # link previous prediction observations with forecast observations
  forecast_obs <- data.table::rbindlist(
    list(
      estimate$predictions[, .(date, primary, secondary)],
      data.table::copy(primary)[, .(primary = median(value)), by = "date"]
    ),
    use.names = TRUE, fill = TRUE
  )
  data.table::setorderv(forecast_obs, "date")
  # add in predictions in estimate_secondary format
  out$predictions <- data.table::merge.data.table(summarised,
    forecast_obs, by = "date", all = TRUE
  )
  data.table::setcolorder(
    out$predictions, c("date", "primary", "secondary", "mean", "sd")
  )
  class(out) <- c("estimate_secondary", class(out))
  return(out)
}
