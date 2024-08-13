#' Create Clean Reported Cases
#' @description `r lifecycle::badge("stable")`
#' Filters leading zeros, completes dates, and applies an optional threshold at
#' which point 0 cases are replaced with a user supplied value (defaults to
#' `NA`).
#'
#' @param filter_leading_zeros Logical, defaults to TRUE. Should zeros at the
#' start of the time series be filtered out.
#'
#' @param zero_threshold `r lifecycle::badge("experimental")` Numeric defaults
#' to Inf. Indicates if detected zero cases are meaningful by using a threshold
#' number of cases based on the 7-day average. If the average is above this
#' threshold then the zero is replaced using `fill`.
#'
#' @param fill Numeric, defaults to NA. Value to use to replace NA values or
#' zeroes that are flagged because the 7-day average is above the
#' `zero_threshold`. If the default NA is used then dates with NA values or with
#' 7-day averages above the `zero_threshold` will be skipped in model fitting.
#' If this is set to 0 then the only effect is to replace NA values with 0.
#' @param add_breakpoints Logical, defaults to TRUE. Should a breakpoint column
#' be added to the data frame if it does not exist.
#'
#' @inheritParams estimate_infections
#' @importFrom data.table copy merge.data.table setorder setDT frollsum
#' @return A cleaned data frame of reported cases
#' @keywords internal
#' @examples
#' \dontrun{
#' create_clean_reported_cases(example_confirmed, 7)
#' }
create_clean_reported_cases <- function(data, horizon = 0,
                                        filter_leading_zeros = TRUE,
                                        zero_threshold = Inf,
                                        fill = NA_integer_,
                                        add_breakpoints = TRUE) {
  reported_cases <- data.table::setDT(data)
  reported_cases_grid <- data.table::copy(reported_cases)[,
   .(date = seq(min(date), max(date) + horizon, by = "days"))
  ]

  reported_cases <- data.table::merge.data.table(
    reported_cases, reported_cases_grid,
    by = "date", all.y = TRUE
  )

  if (is.null(reported_cases$breakpoint) && add_breakpoints) {
    reported_cases$breakpoint <- 0
  }
  if (!is.null(reported_cases$breakpoint)) {
    reported_cases[is.na(breakpoint), breakpoint := 0]
  }
  reported_cases <- data.table::setorder(reported_cases, date)
  ## Filter out 0 reported cases from the beginning of the data
  if (filter_leading_zeros) {
    reported_cases <- reported_cases[order(date)][
      date >= min(date[confirm[!is.na(confirm)] > 0])
    ]
  }
  # Calculate `average_7_day` which for rows with `confirm == 0`
  # (the only instance where this is being used) equates to the 7-day
  # right-aligned moving average at the previous data point.
  reported_cases <-
    reported_cases[
      ,
      `:=`(average_7_day = (
          data.table::frollsum(confirm, n = 8, na.rm = TRUE)
        ) / 7
      )
    ]
  # Check case counts preceding zero case counts and set to 7 day average if
  # average over last 7 days is greater than a threshold
  if (!is.infinite(zero_threshold)) {
    reported_cases <- reported_cases[
      confirm == 0 & average_7_day > zero_threshold,
      confirm := NA_integer_
    ]
  }
  reported_cases[is.na(confirm), confirm := fill]
  reported_cases[, "average_7_day" := NULL]
  return(reported_cases)
}

#' Create complete cases
#' @description `r lifecycle::badge("stable")`
#' Creates a complete data set without NA values and appropriate indices
#'
#' @param cases data frame with a column "confirm" that may contain NA values
#'
#' @return A data frame without NA values, with two columns: confirm (number)
#' @importFrom data.table setDT
#' @keywords internal
create_complete_cases <- function(cases) {
  cases <- setDT(cases)
  cases[, lookup := seq_len(.N)]
  cases <- cases[!is.na(cases$confirm)]
  return(cases[])
}

#' Create Delay Shifted Cases
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This functions creates a data frame of reported cases that has been smoothed
#' using a centred partial rolling average (with a period set by
#' `smoothing_window`) and shifted back in time by some delay. It is used by
#' [estimate_infections()] to generate the mean shifted prior on which the back
#' calculation method (see [backcalc_opts()]) is based.
#'
#' @details
#' The function first shifts all the data back in time by `shift` days (thus
#' discarding the first `shift` days of data) and then applies a centred
#' rolling mean of length `smoothing_window` to the shifted data except for
#' the final period. The final period (the forecast horizon plus half the
#' smoothing window) is instead replaced by a log-linear model fit (with 1
#' added to the data for fitting to avoid zeroes and later subtracted again),
#' projected to the end of the forecast horizon. The initial part of the data
#' (corresponding to the length of the smoothing window) is then removed, and
#' any non-integer resulting values rounded up.
#'
#' @param smoothing_window Numeric, the rolling average smoothing window
#' to apply. Must be odd in order to be defined as a centred average.
#'
#' @param shift Numeric, mean delay shift to apply.
#'
#' @inheritParams estimate_infections
#' @inheritParams create_stan_data
#' @importFrom data.table copy shift frollmean fifelse .N
#' @importFrom stats lm
#' @importFrom runner mean_run
#' @return A `<data.frame>` for shifted reported cases
#' @keywords internal
#' @examples
#' \dontrun{
#' shift <- 7
#' horizon <- 7
#' smoothing_window <- 14
#' ## add NAs for horizon
#' cases <- create_clean_reported_cases(example_confirmed, horizon = horizon)
#' ## add zeroes initially
#' cases <- data.table::rbindlist(list(
#'    data.table::data.table(
#'      date = seq(
#'        min(cases$date) - smoothing_window,
#'        min(cases$date) - 1,
#'        by = "days"
#'      ),
#'      confirm = 0, breakpoint = 0
#'    ),
#'    cases
#'  ))
#' create_shifted_cases(cases, shift, smoothing_window, horizon)
#' }
create_shifted_cases <- function(data, shift,
                                 smoothing_window, horizon) {
  shifted_reported_cases <- data.table::copy(data)[
    ,
    confirm := data.table::shift(confirm,
      n = shift,
      type = "lead", fill = NA
    )
  ][
    ,
    confirm := runner::mean_run(
      confirm, k = smoothing_window, lag = -floor(smoothing_window / 2)
    )
  ]

  ## Forecast trend on reported cases using the last week of data
  final_period <- data.table::data.table(
    confirm =
      shifted_reported_cases[!is.na(confirm)][
        max(1, .N - smoothing_window):.N
      ]$confirm
  )[,
    t := seq_len(.N)
  ]
  lm_model <- stats::lm(log(confirm + 1) ~ t, data = final_period)
  ## Estimate unreported future infections using a log linear model
  shifted_reported_cases <- shifted_reported_cases[
    ,
    t := seq_len(.N)
  ][
    ,
    t := t - (.N - horizon - shift - 6)
  ][
    ,
    confirm := data.table::fifelse(
      t >= 7,
      exp(lm_model$coefficients[1] + lm_model$coefficients[2] * t) - 1,
      confirm
    )
  ][, t := NULL]

  ## Drop median generation interval initial values
  shifted_reported_cases <- shifted_reported_cases[,
   confirm := ceiling(confirm)
  ]
  shifted_reported_cases <- shifted_reported_cases[-(1:smoothing_window)]
  return(shifted_reported_cases)
}

#' Construct the Required Future Rt assumption
#'
#' @description `r lifecycle::badge("stable")`
#' Converts the `future` argument from [rt_opts()] into arguments that can be
#' passed to stan.
#'
#' @param future A character string or integer. This argument indicates how to
#' set future Rt values. Supported options are to project using the Rt model
#' ("project"), to use the latest estimate based on partial data ("latest"),
#' to use the latest estimate based on data that is over 50% complete
#' ("estimate"). If an integer is supplied then the Rt estimate from this many
#' days into the future (or past if negative) past will be used forwards in
#' time.
#'
#' @param delay Numeric mean delay
#' @importFrom rlang arg_match
#' @keywords internal
#' @return A list containing a logical called fixed and an integer called from
create_future_rt <- function(future = c("latest", "project", "estimate"),
                             delay = 0) {
  out <- list(fixed = FALSE, from = 0)
  if (is.character(future)) {
    future <- arg_match(future)
    if (!(future == "project")) {
      out$fixed <- TRUE
      out$from <- ifelse(future == "latest", 0, -delay)
    }
  } else if (is.numeric(future)) {
    out$fixed <- TRUE
    out$from <- as.integer(future)
  }
  return(out)
}

#' Create Time-varying Reproduction Number Data
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output from [rt_opts()] and converts it into a list understood by
#' stan.
#' @param rt A list of options as generated by [rt_opts()] defining Rt
#' estimation. Defaults to [rt_opts()]. Set to `NULL` to switch to using back
#' calculation rather than generating infections using Rt.
#'
#' @param breakpoints An integer vector (binary) indicating the location of
#' breakpoints.
#'
#' @param horizon Numeric, forecast horizon.
#'
#' @seealso rt_settings
#' @return A list of settings defining the time-varying reproduction number
#' @inheritParams create_future_rt
#' @keywords internal
#' @examples
#' \dontrun{
#' # default Rt data
#' create_rt_data()
#'
#' # settings when no Rt is desired
#' create_rt_data(rt = NULL)
#'
#' # using breakpoints
#' create_rt_data(rt_opts(use_breakpoints = TRUE), breakpoints = rep(1, 10))
#' }
create_rt_data <- function(rt = rt_opts(), breakpoints = NULL,
                           delay = 0, horizon = 0) {
  # Define if GP is on or off
  if (is.null(rt)) {
    rt <- rt_opts(
      use_rt = FALSE,
      future = "project",
      gp_on = "R0"
    )
  }
  # define future Rt arguments
  future_rt <- create_future_rt(
    future = rt$future,
    delay = delay
  )
  # apply random walk
  if (rt$rw != 0) {
    breakpoints <- as.integer(seq_along(breakpoints) %% rt$rw == 0)
    if (!(rt$future == "project")) {
      max_bps <- length(breakpoints) - horizon + future_rt$from
      if (max_bps < length(breakpoints)) {
        breakpoints[(max_bps + 1):length(breakpoints)] <- 0
      }
    }
  }
  # check breakpoints
  if (is.null(breakpoints) || sum(breakpoints) == 0) {
    rt$use_breakpoints <- FALSE
  }
  # map settings to underlying gp stan requirements
  rt_data <- list(
    r_mean = rt$prior$mean,
    r_sd = rt$prior$sd,
    estimate_r = as.numeric(rt$use_rt),
    bp_n = ifelse(rt$use_breakpoints, sum(breakpoints, na.rm = TRUE), 0),
    breakpoints = breakpoints,
    future_fixed =  as.numeric(future_rt$fixed),
    fixed_from = future_rt$from,
    pop = rt$pop,
    stationary =  as.numeric(rt$gp_on == "R0"),
    future_time = horizon - future_rt$from
  )
  return(rt_data)
}
#' Create Back Calculation Data
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output of [backcalc_opts()] and converts it into a list understood
#' by stan.
#'
#' @param backcalc A list of options as generated by [backcalc_opts()] to
#' define the back calculation. Defaults to [backcalc_opts()].
#'
#' @seealso backcalc_opts
#' @importFrom data.table fcase
#' @return A list of settings defining the Gaussian process
#' @keywords internal
create_backcalc_data <- function(backcalc = backcalc_opts()) {
  data <- list(
   rt_half_window = as.integer((backcalc$rt_window - 1) / 2),
   backcalc_prior = data.table::fcase(
     backcalc$prior == "none", 0,
     backcalc$prior == "reports", 1,
     backcalc$prior == "infections", 2,
     default = 0
   )
 )
 return(data)
}

#' Create Gaussian Process Data
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output of [gp_opts()] and converts it into a list understood by
#' stan.
#' @param gp A list of options as generated by [gp_opts()] to define the
#' Gaussian process. Defaults to [gp_opts()]. Set to `NULL` to disable the
#' Gaussian process.
#' @param data A list containing the following numeric values:
#' `t`, `seeding_time`, `horizon`.
#' @importFrom data.table fcase
#' @seealso [gp_opts()]
#' @return A list of settings defining the Gaussian process
#' @keywords internal
#' @examples
#' \dontrun{
#' # define input data required
#' data <- list(
#'   t = 30,
#'   seeding_time = 7,
#'   horizon = 7
#' )
#'
#' # default gaussian process data
#' create_gp_data(data = data)
#'
#' # settings when no gaussian process is desired
#' create_gp_data(NULL, data)
#'
#' # custom lengthscale
#' create_gp_data(gp_opts(ls_mean = 14), data)
#' }
create_gp_data <- function(gp = gp_opts(), data) {
  # Define if GP is on or off
  if (is.null(gp)) {
    fixed <- TRUE
    data$stationary <- 1
    gp <- gp_opts()
  } else {
    fixed <- FALSE
  }

  time <- data$t - data$seeding_time
  if (data$fixed_from > 0) {
    time <- time + data$fixed_from - data$horizon
  }
  if (data$stationary == 1) {
    time <- time - 1
  }

  obs_time <- data$t - data$seeding_time
  if (gp$ls_max > obs_time) {
    gp$ls_max <- obs_time
  }

  times <- 1:time

  rescaled_times <- (times - mean(times)) / sd(times)
  gp$ls_mean <- gp$ls_mean / sd(times)
  gp$ls_sd <- gp$ls_sd / sd(times)
  gp$ls_min <- gp$ls_min / sd(times)
  gp$ls_max <- gp$ls_max /  sd(times)

  # basis functions
  M <- ceiling(time * gp$basis_prop)

  # map settings to underlying gp stan requirements
  gp_data <- list(
    fixed = as.numeric(fixed),
    M = M,
    L = gp$boundary_scale * max(rescaled_times),
    ls_meanlog = convert_to_logmean(gp$ls_mean, gp$ls_sd),
    ls_sdlog = convert_to_logsd(gp$ls_mean, gp$ls_sd),
    ls_min = gp$ls_min,
    ls_max = gp$ls_max,
    alpha_sd = gp$alpha_sd,
    gp_type = data.table::fcase(
      gp$kernel == "se", 0,
      gp$kernel == "periodic", 1,
      gp$kernel == "matern" || gp$kernel == "ou", 2,
      default = 2
    ),
    nu = gp$matern_order,
    w0 = gp$w0
  )

  gp_data <- c(data, gp_data)
  return(gp_data)
}

#' Create Observation Model Settings
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output of [obs_opts()] and converts it into a list understood
#' by stan.
#' @param obs A list of options as generated by [obs_opts()] defining the
#' observation model. Defaults to [obs_opts()].
#' @param dates A vector of dates used to calculate the day of the week.
#' @seealso [obs_opts()]
#' @return A list of settings ready to be passed to stan defining
#' the Observation Model
#' @keywords internal
#' @examples
#' \dontrun{
#' dates <- seq(as.Date("2020-03-15"), by = "days", length.out = 15)
#' # default observation model data
#' create_obs_model(dates = dates)
#'
#' # Poisson observation model
#' create_obs_model(obs_opts(family = "poisson"), dates = dates)
#'
#' # Applying a observation scaling to the data
#' create_obs_model(
#'  obs_opts(scale = list(mean = 0.4, sd = 0.01)), dates = dates
#' )
#'
#' # Apply a custom week week length
#' create_obs_model(obs_opts(week_length = 3), dates = dates)
#' }
create_obs_model <- function(obs = obs_opts(), dates) {
  data <- list(
    model_type = as.numeric(obs$family == "negbin"),
    phi_mean = obs$phi$mean,
    phi_sd = obs$phi$sd,
    week_effect = ifelse(obs$week_effect, obs$week_length, 1),
    obs_weight = obs$weight,
    obs_scale = as.integer(obs$scale$sd > 0 || obs$scale$mean != 1),
    obs_scale_mean = obs$scale$mean,
    obs_scale_sd = obs$scale$sd,
    accumulate = obs$accumulate,
    likelihood = as.numeric(obs$likelihood),
    return_likelihood = as.numeric(obs$return_likelihood)
  )

  data$day_of_week <- add_day_of_week(dates, data$week_effect)

  return(data)
}
#' Create Stan Data Required for estimate_infections
#'
#' @description`r lifecycle::badge("stable")`
#' Takes the output of [stan_opts()] and converts it into a list understood by
#' stan. Internally calls the other `create_` family of functions to
#' construct a single list for input into stan with all data required
#' present.
#'
#' @param shifted_cases A `<data.frame>` of delay shifted cases
#'
#' @param seeding_time Integer; seeding time, usually obtained using
#' [get_seeding_time()].
#'
#' @inheritParams estimate_infections
#' @inheritParams create_gp_data
#' @inheritParams create_obs_model
#' @inheritParams create_rt_data
#' @inheritParams create_backcalc_data
#' @importFrom stats lm
#' @importFrom purrr safely
#' @return A list of stan data
#' @keywords internal
#' @examples
#' \dontrun{
#' create_stan_data(
#'  example_confirmed, 7, rt_opts(), gp_opts(), obs_opts(), 7,
#'  backcalc_opts(), create_shifted_cases(example_confirmed, 7, 14, 7)
#' )
#' }
create_stan_data <- function(data, seeding_time,
                             rt, gp, obs, horizon,
                             backcalc, shifted_cases) {

  cases <- data[(seeding_time + 1):(.N - horizon)]
  complete_cases <- create_complete_cases(cases)
  cases <- cases$confirm

  stan_data <- list(
    cases = complete_cases$confirm,
    cases_time = complete_cases$lookup,
    lt = nrow(complete_cases),
    shifted_cases = shifted_cases,
    t = length(data$date),
    horizon = horizon,
    burn_in = 0,
    seeding_time = seeding_time
  )
  # add Rt data
  stan_data <- c(
    stan_data,
    create_rt_data(rt,
      breakpoints = data[(stan_data$seeding_time + 1):.N]$breakpoint,
      delay = stan_data$seeding_time, horizon = stan_data$horizon
    )
  )
  # initial estimate of growth
  first_week <- data.table::data.table(
    confirm = cases[seq_len(min(7, length(cases)))],
    t = seq_len(min(7, length(cases)))
  )[!is.na(confirm)]
  stan_data$prior_infections <- log(mean(first_week$confirm, na.rm = TRUE))
  stan_data$prior_infections <- ifelse(
    is.na(stan_data$prior_infections) || is.null(stan_data$prior_infections),
    0, stan_data$prior_infections
  )
  if (stan_data$seeding_time > 1 && nrow(first_week) > 1) {
    safe_lm <- purrr::safely(stats::lm)
    stan_data$prior_growth <- safe_lm(log(confirm) ~ t,
      stan_data = first_week
    )[[1]]
    stan_data$prior_growth <- ifelse(is.null(stan_data$prior_growth), 0,
      stan_data$prior_growth$coefficients[2]
    )
  } else {
    stan_data$prior_growth <- 0
  }

  # backcalculation settings
  stan_data <- c(stan_data, create_backcalc_data(backcalc))
  # gaussian process data
  stan_data <- create_gp_data(gp, stan_data)

  # observation model data
  stan_data <- c(
    stan_data,
    create_obs_model(
      obs,
      dates = data[(stan_data$seeding_time + 1):.N]$date
    )
  )

  # rescale mean shifted prior for back calculation if observation scaling is
  # used
  if (stan_data$obs_scale == 1) {
    stan_data$shifted_cases <-
      stan_data$shifted_cases / stan_data$obs_scale_mean
    stan_data$prior_infections <- log(
      exp(stan_data$prior_infections) / stan_data$obs_scale_mean
    )
  }
  return(stan_data)
}

##' Create initial conditions for delays
##'
##' @inheritParams create_initial_conditions
##' @return A list of initial conditions for delays
##' @keywords internal
create_delay_inits <- function(data) {
  out <- list()
  if (data$delay_n_p > 0) {
    out$delay_params <- array(truncnorm::rtruncnorm(
      n = data$delay_params_length, a = data$delay_params_lower,
      mean = data$delay_params_mean, sd = data$delay_params_sd * 0.1
    ))
  } else {
    out$delay_params <- array(numeric(0))
  }
  return(out)
}

#' Create Initial Conditions Generating Function
#' @description `r lifecycle::badge("stable")`
#' Uses the output of [create_stan_data()] to create a function which can be
#' used to sample from the prior distributions (or as close as possible) for
#' parameters. Used in order to initialise each stan chain within a range of
#' plausible values.
#' @param data A list of data as produced by [create_stan_data()].
#' @return An initial condition generating function
#' @importFrom purrr map2_dbl
#' @importFrom truncnorm rtruncnorm
#' @importFrom data.table fcase
#' @keywords internal
create_initial_conditions <- function(data) {
  init_fun <- function() {
    out <- create_delay_inits(data)

    if (data$fixed == 0) {
      out$eta <- array(rnorm(data$M, mean = 0, sd = 0.1))
      out$rescaled_rho <- array(rlnorm(1,
        meanlog = data$ls_meanlog,
        sdlog = ifelse(data$ls_sdlog > 0, data$ls_sdlog * 0.1, 0.01)
      ))

      out$rescaled_rho <- array(data.table::fcase(
        out$rescaled_rho > data$ls_max, data$ls_max - 0.001,
        out$rescaled_rho < data$ls_min, data$ls_min + 0.001,
        default = out$rescaled_rho
        ))

      out$alpha <- array(
        truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = data$alpha_sd)
      )
    } else {
      out$eta <- array(numeric(0))
      out$rho <- array(numeric(0))
      out$alpha <- array(numeric(0))
    }
    if (data$model_type == 1) {
      out$rep_phi <- array(
        truncnorm::rtruncnorm(
          1,
          a = 0, mean = data$phi_mean, sd = data$phi_sd / 10
        )
      )
    }
    if (data$estimate_r == 1) {
      out$initial_infections <- array(rnorm(1, data$prior_infections, 0.02))
      if (data$seeding_time > 1) {
        out$initial_growth <- array(rnorm(1, data$prior_growth, 0.01))
      }
      out$log_R <- array(rnorm(
        n = 1, mean = convert_to_logmean(data$r_mean, data$r_sd),
        sd = convert_to_logsd(data$r_mean, data$r_sd) * 0.1
      ))
    }

    if (data$bp_n > 0) {
      out$bp_sd <- array(truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = 0.1))
      out$bp_effects <- array(rnorm(data$bp_n, 0, 0.1))
    } else {
      out$bp_sd <- array(numeric(0))
      out$bp_effects <- array(numeric(0))
    }
    if (data$obs_scale_sd > 0) {
      out$frac_obs <- array(truncnorm::rtruncnorm(1,
        a = 0, b = 1,
        mean = data$obs_scale_mean,
        sd = data$obs_scale_sd * 0.1
      ))
    } else {
      out$frac_obs <- array(numeric(0))
    }
    if (data$week_effect > 0) {
      out$day_of_week_simplex <- array(
        rep(1 / data$week_effect, data$week_effect)
      )
    }
    return(out)
  }
  return(init_fun)
}

#' Create a List of Stan Arguments
#'
#' @description `r lifecycle::badge("stable")`
#' Generates a list of arguments as required by the stan sampling functions by
#' combining the required options with data, and type of initialisation.
#' Initialisation defaults to random but it is expected that
#' [create_initial_conditions()] will be used.
#'
#' @param stan A list of stan options as generated by [stan_opts()]. Defaults
#' to [stan_opts()]. Can be used to override `data`, `init`, and `verbose`
#' settings if desired.
#'
#' @param data A list of stan data as created by [create_stan_data()]
#'
#' @param init Initial conditions passed to `{rstan}`. Defaults to "random"
#' (initial values randomly drawn between -2 and 2) but can also be a
#' function (as supplied by [create_initial_conditions()]).
#'
#' @param model Character, name of the model for which arguments are
#' to be created.
#' @param fixed_param Logical, defaults to `FALSE`. Should arguments be
#' created to sample from fixed parameters (used by simulation functions).
#'
#' @param verbose Logical, defaults to `FALSE`. Should verbose progress
#' messages be returned.
#'
#' @importFrom utils modifyList
#'
#' @return A list of stan arguments
#' @keywords internal
#' @examples
#' \dontrun{
#' # default settings
#' create_stan_args()
#'
#' # increasing warmup
#' create_stan_args(stan = stan_opts(warmup = 1000))
#' }
create_stan_args <- function(stan = stan_opts(),
                             data = NULL,
                             init = "random",
                             model = "estimate_infections",
                             fixed_param = FALSE,
                             verbose = FALSE) {
  if (fixed_param) {
    if (stan$backend == "rstan") {
      stan$algorithm <- "Fixed_param"
    } else if (stan$backend == "cmdstanr") {
      stan$fixed_param <- TRUE
      stan$adapt_delta <- NULL
      stan$max_treedepth <- NULL
    }
  }
  ## generate stan model
  if (is.null(stan$object)) {
    stan$object <- epinow2_stan_model(stan$backend, model)
    stan$backend <- NULL
  }
  # cmdstanr doesn't have an init = "random" argument
  if (is.character(init) && init == "random" &&
      inherits(stan$object, "CmdStanModel")) {
    init <- 2
  }
  # set up shared default arguments
  args <- list(
    data = data,
    init = init,
    refresh = ifelse(verbose, 50, 0)
  )
  args <- modifyList(args, stan)
  args$return_fit <- NULL
  return(args)
}

##' Create delay variables for stan
##'
##' @param ... Named delay distributions. The names are assigned to IDs
##' @param time_points Integer, the number of time points in the data;
##'   determines weight associated with weighted delay priors; default: 1
##' @return A list of variables as expected by the stan model
##' @importFrom purrr transpose map flatten
##' @keywords internal
create_stan_delays <- function(..., time_points = 1L) {
  delays <- list(...)
  ## discretise
  delays <- map(delays, discretise, strict = FALSE)
  ## get maximum delays
  bounded_delays <- map(delays, function(x) discretise(fix_dist(x)))
  max_delay <- unname(as.numeric(flatten(map(bounded_delays, max))))
  ## number of different non-empty types
  type_n <- vapply(delays, ndist, integer(1))
  ## assign ID values to each type
  ids <- rep(0L, length(type_n))
  ids[type_n > 0] <- seq_len(sum(type_n > 0))
  names(ids) <- paste(names(type_n), "id", sep = "_")

  ## create "flat version" of delays, i.e. a list of all the delays (including
  ## elements of composite delays)
  if (length(delays) > 1) {
    flat_delays <- do.call(c, delays)
  } else {
    flat_delays <- delays
  }
  parametric <- unname(vapply(
    flat_delays, function(x) get_distribution(x) != "nonparametric", logical(1)
  ))
  param_length <- unname(vapply(flat_delays[parametric], function(x) {
    length(get_parameters(x))
  }, numeric(1)))
  nonparam_length <- unname(vapply(flat_delays[!parametric], function(x) {
    length(x$pmf)
  }, numeric(1)))
  distributions <- unname(as.character(
    map(flat_delays[parametric], get_distribution)
  ))

  ## create stan object
  ret <- list(
    n = length(flat_delays),
    n_p = sum(parametric),
    n_np = sum(!parametric),
    types = sum(type_n > 0),
    types_p = array(as.integer(parametric))
  )

  ## delay identifiers
  ret$types_id <- integer(0)
  ret$types_id[ret$types_p == 1] <- seq_len(ret$n_p)
  ret$types_id[ret$types_p == 0] <- seq_len(ret$n_np)
  ret$types_id <- array(ret$types_id)
  ## map delays to identifiers
  ret$types_groups <- array(c(0, cumsum(unname(type_n[type_n > 0]))) + 1)

  ret$params_mean <- array(unname(as.numeric(
    map(flatten(map(flat_delays[parametric], get_parameters)), mean)
  )))
  ret$params_sd <- array(unname(as.numeric(
    map(flatten(map(flat_delays[parametric], get_parameters)), sd)
  )))
  ret$params_sd[is.na(ret$params_sd)] <- 0
  ret$max <- array(max_delay[parametric])

  ret$np_pmf <- array(unname(as.numeric(
    flatten(map(flat_delays[!parametric], get_pmf))
  )))
  ## get non zero length delay pmf lengths
  ret$np_pmf_groups <- array(c(0, cumsum(nonparam_length)) + 1)
  ## calculate total np pmf length
  ret$np_pmf_length <- sum(nonparam_length)
  ## get non zero length param lengths
  ret$params_groups <- array(c(0, cumsum(param_length)) + 1)
  ## calculate total param length
  ret$params_length <- sum(param_length)
  ## set lower bounds
  ret$params_lower <- array(unname(as.numeric(flatten(
    map(flat_delays[parametric], function(x) {
      lower_bounds(get_distribution(x))[names(get_parameters(x))]
    })
  ))))
  ## assign prior weights
  weight_priors <- vapply(
    delays[parametric], attr, "weight_prior", FUN.VALUE = logical(1)
  )
  ret$weight <- array(rep(1, ret$n_p))
  ret$weight[weight_priors] <- time_points
  ## assign distribution
  ret$dist <- array(match(distributions, c("lognormal", "gamma")) - 1L)

  names(ret) <- paste("delay", names(ret), sep = "_")
  ret <- c(ret, ids)

  return(ret)
}
