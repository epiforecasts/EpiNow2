#' Create Clean Reported Cases
#' @description `r lifecycle::badge("stable")`
#' Cleans a data frame of reported cases by replacing missing dates with 0 cases and applies an optional
#' threshold at which point 0 cases are replaced with a moving average of observed cases. See `zero_threshold`
#' for details.
#' @param zero_threshold `r lifecycle::badge("experimental")` Numeric defaults to 50. Indicates if detected zero
#' cases are meaningful by using a threshold of 50 cases on average over the last 7 days. If the average is
#' above this threshold then the zero is replaced with the backwards looking rolling average. If set to infinity
#' then no changes are made.
#' @inheritParams estimate_infections
#' @importFrom data.table copy merge.data.table setorder setDT frollsum
#' @return A cleaned data frame of reported cases
#' @export
create_clean_reported_cases <- function(reported_cases, horizon, zero_threshold = 50) {
  reported_cases <- data.table::setDT(reported_cases)
  reported_cases_grid <- data.table::copy(reported_cases)[, .(date = seq(min(date), max(date) + horizon, by = "days"))]

  reported_cases <- data.table::merge.data.table(
    reported_cases, reported_cases_grid,
    by = c("date"), all.y = TRUE
  )

  if (is.null(reported_cases$breakpoint)) {
    reported_cases$breakpoint <- 0
  }
  reported_cases <- reported_cases[is.na(confirm), confirm := 0][, .(date = date, confirm, breakpoint)]
  reported_cases <- reported_cases[is.na(breakpoint), breakpoint := 0]
  reported_cases <- data.table::setorder(reported_cases, date)
  ## Filter out 0 reported cases from the beginning of the data
  reported_cases <- reported_cases[order(date)][
    ,
    cum_cases := cumsum(confirm)
  ][cum_cases > 0][, cum_cases := NULL]

  # Check case counts surrounding zero cases and set to 7 day average if average is over 7 days
  # is greater than a threshold
  if (is.infinite(zero_threshold)) {
    reported_cases <-
      reported_cases[
        ,
        `:=`(average_7 = data.table::frollsum(confirm, n = 8) / 7)
      ]
    reported_cases <- reported_cases[
      confirm == 0 & average_7 > zero_threshold,
      confirm := as.integer(average_7)
    ][
      ,
      c("average_7") := NULL
    ]
  }
  return(reported_cases)
}

#' Create Delay Shifted Cases
#'
#' @description `r lifecycle::badge("stable")`
#' This functions creates a data frame of reported cases that has been smoothed using
#' a centred partial rolling average (with a period set by `smoothing_window`) and shifted back in time
#' by some delay. It is used by `estimate_infections` to generate the mean shifted prior
#' on which the back calculation method (see `backcalc_opts()`) is based.
#' @param smoothing_window Numeric, the rolling average smoothing window
#' to apply. Must be odd in order to be defined as a centred average.
#' @param shift Numeric, mean delay shift to apply.
#' @inheritParams estimate_infections
#' @inheritParams create_stan_data
#' @importFrom data.table copy shift frollmean fifelse .N
#' @importFrom stats lm
#' @importFrom runner mean_run
#' @return A data frame for shifted reported cases
#' @export
#' @examples
#' create_shifted_cases(example_confirmed, 7, 14, 7)
create_shifted_cases <- function(reported_cases, shift,
                                 smoothing_window, horizon) {
  shifted_reported_cases <- data.table::copy(reported_cases)[
    ,
    confirm := data.table::shift(confirm,
      n = shift,
      type = "lead", fill = NA
    )
  ][
    ,
    confirm := runner::mean_run(confirm, k = smoothing_window, lag = -floor(smoothing_window / 2))
  ][
    ,
    confirm := data.table::fifelse(confirm == 0, 1, confirm)
  ]

  ## Forecast trend on reported cases using the last week of data
  final_week <- data.table::data.table(confirm = shifted_reported_cases[1:(.N - horizon - shift)][max(1, .N - 6):.N]$confirm)[
    ,
    t := 1:.N
  ]
  lm_model <- stats::lm(log(confirm) ~ t, data = final_week)
  ## Estimate unreported future infections using a log linear model
  shifted_reported_cases <- shifted_reported_cases[
    ,
    t := 1:.N
  ][
    ,
    t := t - (.N - horizon - shift - 6)
  ][
    ,
    confirm := data.table::fifelse(
      t >= 7,
      exp(lm_model$coefficients[1] + lm_model$coefficients[2] * t),
      confirm
    )
  ][, t := NULL]

  ## Drop median generation interval initial values
  shifted_reported_cases <- shifted_reported_cases[, confirm := ceiling(confirm)]
  shifted_reported_cases <- shifted_reported_cases[-(1:smoothing_window)]
  return(shifted_reported_cases)
}

#' Construct the Required Future Rt assumption
#'
#' @description `r lifecycle::badge("stable")`
#' Converts the `future` argument from `rt_opts()` into arguments that can be passed to `stan`.
#' @param future A character string or integer. This argument indicates how to set future Rt values. Supported
#' options are to project using the Rt model ("project"), to use the latest estimate based on partial data ("latest"),
#' to use the latest estimate based on data that is over 50% complete ("estimate"). If an integer is supplied then the Rt estimate
#' from this many days into the future (or past if negative) past will be used forwards in time.
#' @param delay Numeric mean delay
#' @return A list containing a logical called fixed and an integer called from
create_future_rt <- function(future = "latest", delay = 0) {
  out <- list(fixed = FALSE, from = 0)
  if (is.character(future)) {
    future <- match.arg(
      future,
      c(
        "project",
        "latest",
        "estimate"
      )
    )
    if (!(future %in% "project")) {
      out$fixed <- TRUE
      out$from <- ifelse(future %in% "latest", 0, -delay)
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
#' Takes the output from `rt_opts()` and converts it into a list understood by
#' `stan`.
#' @param rt A list of options as generated by `rt_opts()` defining Rt estimation.
#' Defaults to `rt_opts()`. Set to `NULL` to switch to using  back calculation
#' rather than generating infections using Rt.
#' @param breakpoints An integer vector (binary) indicating the location of breakpoints.
#' @param horizon Numeric, forecast horizon.
#' @seealso rt_settings
#' @return A list of settings defining the time-varying reproduction number
#' @inheritParams create_future_rt
#' @export
#' @examples
#' # default Rt data
#' create_rt_data()
#'
#' # settings when no Rt is desired
#' create_rt_data(rt = NULL)
#'
#' # using breakpoints
#' create_rt_data(rt_opts(use_breakpoints = TRUE), breakpoints = rep(1, 10))
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
    breakpoints <- as.integer(1:length(breakpoints) %% rt$rw == 0)
    if (!(rt$future %in% "project")) {
      max_bps <- length(breakpoints) - horizon + future_rt$from
      if (max_bps < length(breakpoints)) {
        breakpoints[(max_bps + 1):length(breakpoints)] <- 0
      }
    }
  }
  # check breakpoints
  if (is.null(breakpoints) | sum(breakpoints) == 0) {
    rt$use_breakpoints <- FALSE
  }
  # map settings to underlying gp stan requirements
  rt_data <- list(
    r_mean = rt$prior$mean,
    r_sd = rt$prior$sd,
    estimate_r = ifelse(rt$use_rt, 1, 0),
    bp_n = ifelse(rt$use_breakpoints, sum(breakpoints, na.rm = TRUE), 0),
    breakpoints = breakpoints,
    future_fixed = ifelse(future_rt$fixed, 1, 0),
    fixed_from = future_rt$from,
    pop = rt$pop,
    stationary = ifelse(rt$gp_on %in% "R0", 1, 0),
    future_time = horizon - future_rt$from
  )
  return(rt_data)
}
#' Create Back Calculation Data
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output of `backcalc_opts()` and converts it into a list understood by `stan`.
#' @param backcalc A list of options as generated by `backcalc_opts()` to define the
#' back calculation. Defaults to `backcalc_opts()`.
#' @seealso backcalc_opts
#' @return A list of settings defining the Gaussian process
#' @export
#' @examples
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
create_backcalc_data <- function(backcalc = backcalc_opts) {
  data <- list(
    rt_half_window = as.integer((backcalc$rt_window - 1) / 2),
    backcalc_prior = ifelse(backcalc$prior == "none", 0,
      ifelse(backcalc$prior == "reports", 1,
        ifelse(backcalc$prior == "infections", 2, 0)
      )
    )
  )
  return(data)
}
#' Create Gaussian Process Data
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output of `gp_opts()` and converts it into a list understood by `stan`.
#' @param gp A list of options as generated by `gp_opts()` to define the
#' Gaussian process. Defaults to `gp_opts()`.Set to NULL to disable the
#' Gaussian process.
#' @param data A list containing the following numeric values: `t`
#' @seealso gp_opts
#' @return A list of settings defining the Gaussian process
#' @export
#' @importFrom purrr transpose map map2
#' @examples
#' # define input data required
#' data <- list(t = 30)
#'
#' # settings when no gaussian process is desired
#' create_gp_data(data)
#'
#' # settings with a single gaussian process
#' create_gp_data(list(gp_opts(ls_mean = 14)), data)
#' 
#' # create settings with multiple GPs
#' create_gp_data(list(gp_opts(), gp_opts(order = "0")), data)
#' 
#' # create a GP with a discrete time step
#' create_gp_data(list(gp_opts(step = 7)))
create_gp_data <- function(gp = list(), data) {
  # Define if GP is on or off
  if (is.null(gp) | length(gp) == 0) {
    gps <- 0
  }else{
    gps <- length(gp)
  }

  single_gp <- function(gp, data) {
    # set dimension of gp
    time <- data$t

    # rescale for discrete steps
    if (gp$step >= time) {
      stop("Discrete step is greater or equal to the overall dimension.")
    }
    adj_time <- ceiling(time / gp$step)
    complete_steps <- floor(time / gp$step)
    incomplete_step <- time  - complete_steps * gp$step
    gp_steps <- rep(gp$step, complete_steps)
    if (incomplete_step > 0) {
      gp_steps <- c(gp_steps, incomplete_step)
    }
    gp$ls_max <- ceiling(gp$ls_max / gp$step)
    gp$ls_min <- ceiling(gp$ls_min / gp$step)
    gp$ls_mean <- ceiling(gp$ls_mean / gp$step)
    gp$ls_sd <- ceiling(gp$ls_sd / gp$step)

    if (gp$ls_max > adj_time) {
      gp$ls_max <- adj_time
    }

    if (gp$ls_mean > adj_time) {
      stop("The mean of the lengthscale is larger than the overall dimension.")
    }

    # rescale lengthscale to be between 0 and 1
    scaling <- gp$ls_max - gp$ls_min
    gp$ls_sd <- gp$ls_sd / scaling
    gp$ls_mean <- (gp$ls_mean - gp$ls_min)/ scaling

    # basis functions
    M <- adj_time
    M <- ceiling(M * gp$basis_prop)

    # map settings to underlying gp stan requirements
    gp_data <- list(
      M = M,
      L = gp$boundary_scale,
      ls_meanlog = convert_to_logmean(gp$ls_mean, gp$ls_sd),
      ls_sdlog = convert_to_logsd(gp$ls_mean, gp$ls_sd),
      ls_min = gp$ls_min,
      ls_max = gp$ls_max,
      alpha_sd = gp$alpha_sd,
      gp_type = ifelse(gp$kernel == "se", 0,
        ifelse(gp$kernel == "matern", 1, 0)
      ),
      gp_order = as.numeric(gp$order),
      gp_dims = time,
      gp_adj_dims = adj_time,
      gp_steps = gp_steps,
      gp_no_steps = length(gp_steps),
      gp_start = 1, 
      gp_end = time
    )
    return(gp_data)
  }

  if (gps != 0) {
    gp_data <- map(gp, single_gp, data)
    gp_data <- transpose(gp_data)
    gp_data <- map(gp_data, ~array(unlist(.)))
    gp_data$gp_dim <- sum(gp_data$gp_dims)
    gp_data$gp_mat_dim <- sum(gp_data$M * gp_data$gp_adj_dims)
    gp_data$gps <- length(gp_data$gp_dims)
    gp_data$gp_steps <- 
     split(gp_data$gp_steps, rep(1:gp_data$gps, gp_data$gp_no_steps))
    gp_data$gp_steps <- map2(gp_data$gp_steps, gp_data$gp_no_steps, 
                             ~ c(.x, rep(0, max(gp_data$gp_no_steps) - .y)))
    gp_data$gp_steps <- 
      matrix(unlist(gp_data$gp_steps), byrow = TRUE, nrow = gps)

    if (gps > 1) {
      for (i in 2:gps) {
        gp_data$gp_start[i] <- gp_data$gp_end[i -1] + 1
        gp_data$gp_end[i] <- gp_data$gp_start[i] + gp_data$gp_end[i] - 1
      }
    }
  }else{
    gp_data <- single_gp(gp_opts(), data)
    gp_data <- map(gp_data, ~ array(numeric(0)))
    gp_data$gps <- 0
    gp_data$gp_dim <- 0
    gp_data$gp_mat_dim <- 0
  }
  gp_data <- c(data, gp_data)
  return(gp_data)
}

#' Create Observation Model Settings
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output of `obs_opts()` and converts it into a list understood by `stan`.
#' @param obs A list of options as generated by `obs_opts()` defining the
#' observation model. Defaults to `obs_opts()`.
#' @param dates A vector of dates used to calculate the day of the week.
#' @seealso obs_opts
#' @return A list of settings ready to be passed to stan defining
#' the Observation Model
#' @export
#' @examples
#' dates <- seq(as.Date("2020-03-15"), by = "days", length.out = 15)
#' # default observation model data
#' create_obs_model(dates = dates)
#'
#' # Poisson observation model
#' create_obs_model(obs_opts(family = "poisson"), dates = dates)
#'
#' # Applying a observation scaling to the data
#' create_obs_model(obs_opts(scale = list(mean = 0.4, sd = 0.01)), dates = dates)
#'
#' # Apply a custom week week lenght
#' create_obs_model(obs_opts(week_length = 3), dates = dates)
create_obs_model <- function(obs = obs_opts(), dates) {
  data <- list(
    model_type = ifelse(obs$family %in% "poisson", 0, 1),
    phi_mean = obs$phi[1],
    phi_sd = obs$phi[2],
    week_effect = ifelse(obs$week_effect, obs$week_length, 1),
    obs_weight = obs$weight,
    obs_scale = ifelse(length(obs$scale) != 0, 1, 0),
    obs_scale_gp  = array(as.numeric(!is.null(obs$gp))),
    likelihood = as.numeric(obs$likelihood),
    return_likelihood = as.numeric(obs$return_likelihood)
  )

  data$day_of_week <- add_day_of_week(dates, data$week_effect)

  data <- c(data, list(
    obs_scale_mean = ifelse(data$obs_scale,
      obs$scale$mean, 0
    ),
    obs_scale_sd = ifelse(data$obs_scale,
      obs$scale$sd, 0
    )
  ))
  return(data)
}
#' Create Stan Data Required for estimate_infections
#'
#' @description`r lifecycle::badge("stable")`
#' Takes the output of `stan_opts()` and converts it into a list understood by `stan`. Internally
#' calls the other `create_` family of functions to construct a single list for input into stan
#' with all data required present.
#' @param shifted_cases A dataframe of delay shifted cases
#' @param truncation `r lifecycle::badge("experimental")` A list of options as generated by `trunc_opts()`
#' defining the truncation of observed data. Defaults to `trunc_opts()`. See `estimate_truncation()`
#' for an approach to estimating truncation from data.
#' @inheritParams create_gp_data
#' @inheritParams create_obs_model
#' @inheritParams create_rt_data
#' @inheritParams create_backcalc_data
#' @inheritParams estimate_infections
#' @importFrom stats lm
#' @importFrom purrr safely
#' @return A list of stan data
#' @export
create_stan_data <- function(reported_cases, generation_time,
                             rt, gp, obs, delays, horizon,
                             backcalc, shifted_cases,
                             truncation) {
  cases <- reported_cases[(delays$seeding_time + 1):(.N - horizon)]$confirm

  data <- list(
    cases = cases,
    shifted_cases = shifted_cases,
    t = length(reported_cases$date),
    horizon = horizon,
    gt_mean_mean = generation_time$mean,
    gt_mean_sd = generation_time$mean_sd,
    gt_sd_mean = generation_time$sd,
    gt_sd_sd = generation_time$sd_sd,
    max_gt = generation_time$max,
    burn_in = 0
  )
  # add delay data
  data <- c(data, delays)
  # add truncation data
  data <- c(data, truncation)
  # add Rt data
  data <- c(
    data,
    create_rt_data(rt,
      breakpoints = reported_cases[(data$seeding_time + 1):.N]$breakpoint,
      delay = data$seeding_time, horizon = data$horizon
    )
  )
  # initial estimate of growth
  first_week <- data.table::data.table(
    confirm = cases[1:min(7, length(cases))],
    t = 1:min(7, length(cases))
  )
  data$prior_infections <- log(mean(first_week$confirm, na.rm = TRUE))
  data$prior_infections <- ifelse(is.na(data$prior_infections) | is.null(data$prior_infections),
    0, data$prior_infections
  )
  if (data$seeding_time > 1) {
    safe_lm <- purrr::safely(stats::lm)
    data$prior_growth <- safe_lm(log(confirm) ~ t, data = first_week)[[1]]
    data$prior_growth <- ifelse(is.null(data$prior_growth), 0,
      data$prior_growth$coefficients[2]
    )
  } else {
    data$prior_growth <- 0
  }

  # backcalculation settings
  data <- c(data, create_backcalc_data(backcalc))
  # gaussian process data
  data <- create_gp_data(gp, data)

  # observation model data
  data <- c(
    data,
    create_obs_model(
      obs, dates = reported_cases[(data$seeding_time + 1):.N]$date
    )
  )

  # rescale mean shifted prior for back calculation if observation scaling is used
  if (data$obs_scale == 1) {
    data$shifted_cases <- data$shifted_cases / data$obs_scale_mean
    data$prior_infections <- log(exp(data$prior_infections) / data$obs_scale_mean)
  }
  return(data)
}

#' Create Initial Conditions Generating Function
#' @description `r lifecycle::badge("stable")`
#' Uses the output of `create_stan_data` to create a function which can be used to
#' sample from the prior distributions (or as close as possible) for parameters. Used
#' in order to initialise each `stan` chain within a range of plausible values.
#' @param data A list of data as produced by `create_stan_data.`
#' @return An initial condition generating function
#' @importFrom purrr map2_dbl map_dbl
#' @importFrom truncnorm rtruncnorm
#' @export
create_initial_conditions <- function(data) {
  init_fun <- function() {
    out <- list()
    if (data$delays > 0) {
      out$delay_mean <- array(purrr::map2_dbl(
        data$delay_mean_mean, data$delay_mean_sd * 0.1,
        ~ rnorm(1, mean = .x, sd = .y)
      ))
      out$delay_sd <- array(purrr::map2_dbl(
        data$delay_sd_mean, data$delay_sd_sd * 0.1,
        ~ rnorm(1, mean = .x, sd = .y)
      ))
    }
    if (data$truncation > 0) {
      out$truncation_mean <- array(rnorm(1,
        mean = data$trunc_mean_mean,
        sd = data$trunc_mean_sd * 0.1
      ))
      out$truncation_sd <- array(truncnorm::rtruncnorm(1,
        a = 0,
        mean = data$trunc_sd_mean,
        sd = data$trunc_sd_sd * 0.1
      ))
    }
    if (data$fixed == 0) {
      out$eta <- array(rnorm(sum(data$M), mean = 0, sd = 0.1))
      out$rho <- array(map2_dbl(data$ls_meanlog, data$ls_sdlog,
        ~ rlnorm(1, meanlog = .x, sdlog = .y * 0.1
      )))
      out$rho <- ifelse(out$rho > 1, 0.99,
        ifelse(out$rho < 0, 0.001,
          out$rho
        )
      )
      out$alpha <- array(map_dbl(data$alpha_sd, 
       ~ truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = .)))
    }
    if (data$model_type == 1) {
      out$rep_phi <- array(
        truncnorm::rtruncnorm(
          1, a = 0, mean = data$phi_mean, sd = data$phi_sd / 10
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
      out$gt_mean <- array(truncnorm::rtruncnorm(1,
        a = 0, mean = data$gt_mean_mean,
        sd = data$gt_mean_sd * 0.1
      ))
      out$gt_sd <- array(truncnorm::rtruncnorm(1,
        a = 0, mean = data$gt_sd_mean,
        sd = data$gt_sd_sd * 0.1
      ))
      if (data$bp_n > 0) {
        out$bp_sd <- array(truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = 0.1))
        out$bp_effects <- array(rnorm(data$bp_n, 0, 0.1))
      }
    }
    if (data$obs_scale == 1) {
      out$frac_obs <- array(truncnorm::rtruncnorm(1,
        a = 0,
        mean = data$obs_scale_mean,
        sd = data$obs_scale_sd * 0.1
      ))
    }
    return(out)
  }
  return(init_fun)
}

#' Create a List of Stan Arguments
#'
#' @description `r lifecycle::badge("stable")`
#' Generates a list of arguments as required by `rstan::sampling` or `rstan::vb` by combining the required options,
#' with data, and type of initialisation. Initialisation defaults to random but it is expected that `create_initial_conditions`
#' will be used.
#' @param stan A list of stan options as generated by `stan_opts()`. Defaults to `stan_opts()`. Can be used to override
#' `data`, `init`, and `verbose` settings if desired.
#' @param data A list of stan data as created by `create_stan_data`
#' @param init Initial conditions passed to `rstan`. Defaults to "random" but can also be a function (
#' as supplied by `create_intitial_conditions`).
#' @param verbose Logical, defaults to `FALSE`. Should verbose progress messages be returned.
#' @return A list of stan arguments
#' @export
#' @examples
#' # default settings
#' create_stan_args()
#'
#' # increasing warmup
#' create_stan_args(stan = stan_opts(warmup = 1000))
create_stan_args <- function(stan = stan_opts(),
                             data = NULL,
                             init = "random",
                             verbose = FALSE) {
  # set up shared default arguments
  args <- list(
    data = data,
    init = init,
    refresh = ifelse(verbose, 50, 0)
  )
  args <- update_list(args, stan)
  args$return_fit <- NULL
  return(args)
}
