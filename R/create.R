#' Create Clean Reported Cases
#'
#' @param zero_threshold Numeric defaults to 50. Indicates if detected zero cases are meaningful by 
#' using a threshold of 50 cases on average over the last 7 days. If the average is above this thresold
#' then the zero is replaced with the 
#' @inheritParams estimate_infections
#' @importFrom data.table copy merge.data.table setorder setDT frollsum
#' @return A cleaned data frame of reported cases
#' @export
create_clean_reported_cases <- function(reported_cases, horizon, zero_threshold = 50) {
  
  reported_cases <- data.table::setDT(reported_cases)
  reported_cases_grid <- data.table::copy(reported_cases)[, .(date = seq(min(date), max(date) + horizon, by = "days"))]
  
  reported_cases <- data.table::merge.data.table(
    reported_cases , reported_cases_grid, 
    by = c("date"), all.y = TRUE)
  
  if (is.null(reported_cases$breakpoint)) {
    reported_cases$breakpoint <- 0
  }
  reported_cases <- reported_cases[is.na(confirm), confirm := 0][,.(date = date, confirm, breakpoint)]
  reported_cases <- reported_cases[is.na(breakpoint), breakpoint := 0]
  reported_cases <- data.table::setorder(reported_cases, date)
  ## Filter out 0 reported cases from the beginning of the data
  reported_cases <- reported_cases[order(date)][,
                               cum_cases := cumsum(confirm)][cum_cases > 0][, cum_cases := NULL]
  
  # Check case counts surrounding zero cases and set to 7 day average if average is over 7 days
  # is greater than a threshold
  reported_cases <- reported_cases[, `:=`(average_7 = data.table::frollsum(confirm, n = 8) / 7)]
  reported_cases <- reported_cases[confirm == 0 & average_7 > zero_threshold,
                                   confirm := as.integer(average_7)][,
                                   c("average_7") := NULL]
  return(reported_cases)
}

#' Create a Data Frame of Mean Delay Shifted Cases
#'
#' @param smoothing_window Numeric, the rolling average smoothing window
#' to apply.
#' @param shift Numeric, mean delay shift to apply.
#' @inheritParams estimate_infections
#' @inheritParams create_stan_data
#' @importFrom data.table copy shift frollmean fifelse .N
#' @importFrom stats lm
#' @return A dataframe for shifted reported cases
#' @export
#' @examples
#' create_shifted_cases(example_confirmed, 7, 14, 7)
create_shifted_cases <- function(reported_cases, shift, 
                                 smoothing_window, horizon) {
  
  shifted_reported_cases <- data.table::copy(reported_cases)[,
              confirm := data.table::shift(confirm, n = shift,
                                           type = "lead", fill = NA)][,
              confirm := data.table::frollmean(confirm, n = smoothing_window, 
                                               align = "right", fill = 1)][,
              confirm := data.table::fifelse(confirm == 0, 1, confirm)]
  
  ## Forecast trend on reported cases using the last week of data
  final_week <- data.table::data.table(confirm = shifted_reported_cases[1:(.N - horizon - shift)][max(1, .N - 6):.N]$confirm)[,
                                                                        t := 1:.N]
  lm_model <- stats::lm(log(confirm) ~ t, data = final_week)
  ## Estimate unreported future infections using a log linear model
  shifted_reported_cases <- shifted_reported_cases[,
                                                   t := 1:.N][, 
                                                   t := t - (.N - horizon - shift - 6)][,
                                                   confirm := data.table::fifelse(t >= 7,
                                                                                  exp(lm_model$coefficients[1] + lm_model$coefficients[2] * t),
                                                                                  confirm)][, t := NULL]
  
  ##Drop median generation interval initial values
  shifted_reported_cases <- shifted_reported_cases[, confirm := ceiling(confirm)]
  shifted_reported_cases <- shifted_reported_cases[-(1:smoothing_window)]
  return(shifted_reported_cases)
}

#' Construct the Required Future Rt assumption
#'
#' @param future A character string or integer. This argument indicates how to set future Rt values. Supported 
#' options are to project using the Rt model ("project"), to use the latest estimate based on partial data ("latest"),
#' to use the latest estimate based on data that is over 50% complete ("estimate"). If an integer is supplied then the Rt estimate
#' from this many days into the future (or past if negative) past will be used forwards in time. 
#' @param delay Numeric mean delay
#' @return A list containing a logical called fixed and an integer called from
create_future_rt <- function(future = "latest", delay = 0) {
  out <- list(fixed = FALSE, from = 0)
  if (is.character(future)) {
    future <- match.arg(future,
                           c("project", 
                             "latest",
                             "estimate"))
    if (!(future %in% "project")) {
      out$fixed <- TRUE
      out$from <- ifelse(future %in% "latest", 0, -delay)
    }
  }else if (is.numeric(future)){
    out$fixed <- TRUE
    out$from <- as.integer(future)
  }
  return(out)
}

#' Create Time-varying Reproduction Number Data
#'
#' @param rt A list of options as generated by `rt_opts` defining Rt estimation. 
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
    rt <- rt_opts(use_rt = FALSE,
                  future = "project",
                  gp_on = "R0")
  }
  # define future Rt arguments
  future_rt <- create_future_rt(future = rt$future, 
                                delay = delay)
  # apply random walk
  if (rt$rw != 0) {
    breakpoints <- as.integer(1:length(breakpoints) %% rt$rw == 0)
    if (!(rt$future %in% "project")) {
      max_bps <- length(breakpoints) - horizon + future_rt$from
      if (max_bps < length(breakpoints)){
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

#' Create Gaussian Process Data
#'
#' @param gp A list of options as generated by `gp_opts` to define the 
#' Gaussian process. Defaults to `gp_opts()`.Set to NULL to disable the 
#' Gaussian process.
#' @param data A list containing the following numeric values: `t`, `seeding_time`,
#' `horizon`.
#' @seealso gp_opts
#' @return A list of settings defining the Gaussian process
#' @export
#' @examples
#' # define input data required
#' data <- list(
#'     t = 30,
#'     seeding_time = 7,
#'     horizon = 7)
#' 
#' # default gaussian process data     
#' create_gp_data(data = data)
#' 
#' # settings when no gaussian process is desired
#' create_gp_data(NULL, data)
#' 
#' # custom lengthscale
#' create_gp_data(gp_opts(ls_mean = 14), data)
create_gp_data <- function(gp = gp_opts(), data) {
  # Define if GP is on or off
  if (is.null(gp)) {
    fixed <- TRUE
    data$stationary <- 1
  }else{
    fixed <- FALSE
  }
  # reset ls_max if larger than observed time
  time <- data$t - data$seeding_time - data$horizon
  if (gp$ls_max > time) {
    gp$ls_max <- time
  }
  
  # basis functions
  M <- data$t - data$seeding_time
  M <- ifelse(data$future_fixed == 1, M - (data$horizon - data$fixed_from))
  M <- ceiling(M * gp$basis_prop)
  
  # map settings to underlying gp stan requirements
  gp_data <- list(
    fixed = ifelse(fixed, 1, 0),
    M = M,
    L = gp$boundary_scale,
    ls_meanlog = convert_to_logmean(gp$ls_mean, gp$ls_sd),
    ls_sdlog = convert_to_logsd(gp$ls_mean, gp$ls_sd),
    ls_min = gp$ls_min,
    ls_max = data$t - data$seeding_time - data$horizon,
    alpha_sd = gp$alpha_sd,
    gp_type = ifelse(gp$kernel == "se", 0, 
                      ifelse(gp$kernel == "matern", 1, 0))
  ) 
  
  gp_data <- c(data, gp_data)
  return(gp_data)
}

#' Create Observation Model Settings
#'
#' @param obs A list of options as generated by `obs_opts` defining the 
#' observation model. Defaults to `obs_opts()`.
#' @seealso obs_opts
#' @return A list of settings ready to be passed to stan defining 
#' the Observation Model
#' @export
#' @examples
#' # default observation model data
#' create_obs_model()
#' 
#' # Poisson observation model
#' create_obs_model(obs_opts(family = "poisson"))
#' 
#' # Applying a observation scaling to the data
#' create_obs_model(obs_opts(scale = list(mean = 0.4, sd = 0.01)))
create_obs_model <- function(obs = obs_opts()) {
  data <- list(
    model_type = ifelse(obs$family %in% "poisson", 0, 1),
    week_effect = ifelse(obs$week_effect, 1, 0),
    obs_weight = obs$weight,
    obs_scale = ifelse(length(obs$scale) != 0, 1, 0))
  data <- c(data, list(
    obs_scale_mean = ifelse(data$obs_scale,
                            obs$scale$mean, 0),
    obs_scale_sd = ifelse(data$obs_scale,
                          obs$scale$sd, 0)
  ))
  return(data)
}
#' Create Stan Data Required for estimate_infections
#'
#' @param shifted_cases A dataframe of delay shifted cases
#' @inheritParams create_gp_data
#' @inheritParams create_obs_model
#' @inheritParams create_rt_data
#' @inheritParams estimate_infections
#' @importFrom stats lm
#' @importFrom purrr safely
#' @return A list of stan data
#' @export 
create_stan_data <- function(reported_cases, generation_time,
                             rt, gp, obs, delays, horizon,
                             shifted_cases) {
  cases <- reported_cases[(delays$seeding_time + 1):(.N - horizon)]$confirm
  
  data <- list(
    day_of_week = reported_cases[(delays$seeding_time + 1):.N]$day_of_week,
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
  
  # add Rt data
  data <- c(data, 
            create_rt_data(rt,
                           breakpoints = reported_cases[(data$seeding_time + 1):.N]$breakpoint,
                           delay = data$seeding_time, horizon = data$horizon))
  # initial estimate of growth
  first_week <- data.table::data.table(confirm = cases[1:min(7, length(cases))],
                                       t = 1:min(7, length(cases)))
  data$prior_infections <- log(mean(first_week$confirm, na.rm = TRUE))
  data$prior_infections <- ifelse(is.na(data$prior_infections) | is.null(data$prior_infections), 
                                  0, data$prior_infections)
  if (data$seeding_time > 1) {
    safe_lm <- purrr::safely(stats::lm)
    data$prior_growth <-safe_lm(log(confirm) ~ t, data = first_week)[[1]]
    data$prior_growth <- ifelse(is.null(data$prior_growth), 0, 
                                data$prior_growth$coefficients[2])
  }else{
    data$prior_growth <- 0
  }

  # gaussian process data
  data <- create_gp_data(gp, data)
  
  # observation model data
  data <- c(data, create_obs_model(obs))
  
  # rescale mean shifted prior for back calculation if observation scaling is used
  if (data$obs_scale == 1) {
    data$shifted_cases <- data$shifted_cases / data$obs_scale_mean
    data$prior_infections <- log(exp(data$prior_infections) / data$obs_scale_mean)
  }
  return(data)
}

#' Create Initial Conditions Generating Function
#' @param data A list of data as produced by `create_stan_data.`
#' @return An initial condition generating function
#' @importFrom purrr map2_dbl
#' @importFrom truncnorm rtruncnorm
#' @export
create_initial_conditions <- function(data) {
  init_fun <- function(){
    out <- list()
    if (data$delays > 0) {
      out$delay_mean <- array(purrr::map2_dbl(data$delay_mean_mean, data$delay_mean_sd, 
                                              ~ truncnorm::rtruncnorm(1, a = 0, mean = .x, sd = .y)))
      out$delay_sd <- array(purrr::map2_dbl(data$delay_sd_mean, data$delay_sd_sd, 
                                            ~ truncnorm::rtruncnorm(1, a = 0, mean = .x, sd = .y)))
    }
    if (data$fixed == 0) {
      out$eta <- array(rnorm(data$M, mean = 0, sd = 0.1))
      out$rho <- array(rlnorm(1, meanlog = data$ls_meanlog, 
                              sdlog = data$ls_sdlog))
      out$rho <- ifelse(out$rho > data$ls_max, data$ls_max - 0.001, 
                        ifelse(out$rho < data$ls_min, data$ls_min + 0.001, 
                               out$rho))
      out$alpha <- array(truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = data$alpha_sd))
    }
    if (data$model_type == 1) {
      out$rep_phi <- array(truncnorm::rtruncnorm(1, a = 0, mean = 0,  sd = 1))
    }
    if (data$estimate_r == 1) {
      out$initial_infections <- array(rnorm(1, data$prior_infections, 0.2))
      if (data$seeding_time > 1) {
        out$initial_growth <- array(rnorm(1, data$prior_growth, 0.1))
      }
      out$log_R <- array(rnorm(n = 1, mean = convert_to_logmean(data$r_mean, data$r_sd),
                                      sd = convert_to_logsd(data$r_mean, data$r_sd)))
      out$gt_mean <- array(truncnorm::rtruncnorm(1, a = 0, mean = data$gt_mean_mean,  
                                                 sd = data$gt_mean_sd))
      out$gt_sd <-  array(truncnorm::rtruncnorm(1, a = 0, mean = data$gt_sd_mean,
                                                sd = data$gt_sd_sd))
      if (data$bp_n > 0) {
        out$bp_sd <- array(truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = 0.1))
        out$bp_effects <- array(rnorm(data$bp_n, 0, 0.1))
      }
    }
    if (data$obs_scale == 1) {
      out$frac_obs = array(truncnorm::rtruncnorm(1, a = 0, 
                                                 mean = data$obs_scale_mean,
                                                 sd = data$obs_scale_sd))
    }
    return(out)
  }
  return(init_fun)
}

#' Create a List of Stan Arguments
#'
#'
#' @description Generates a list of arguments as required by `rstan::sampling` (when `stan_args$method = "sampling`) 
#' or `rstan::vb` (when `stan_args$method = "vb`). See `create_stan_args()` for the defaults and the relevant `rstan`
#' functions for additional options. Defaults can be overwritten by passing a list containing the new setting to 
#' `stan_args`.
#' @param stan A list of stan options as generated by `stan_opts`. Defaults to `stan_opts()`. Can be used to override
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
  args <- update_defaults(args, stan)
  args$return_fit <- NULL
  return(args)
}
