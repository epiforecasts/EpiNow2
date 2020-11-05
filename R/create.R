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
#' @inheritParams estimate_infections
#' @inheritParams create_stan_data
#' @importFrom data.table copy shift frollmean fifelse .N
#' @importFrom stats lm
#' @return A dataframe for shifted reported cases
#' @export
create_shifted_cases <- function(reported_cases, mean_shift, 
                                 prior_smoothing_window, horizon) {
  
  shifted_reported_cases <- data.table::copy(reported_cases)[,
              confirm := data.table::shift(confirm, n = mean_shift,
                                           type = "lead", fill = NA)][,
              confirm := data.table::frollmean(confirm, n = prior_smoothing_window, 
                                               align = "right", fill = 0)][,
              confirm := data.table::fifelse(confirm == 0, 1e-3, confirm)]
  
  ## Forecast trend on reported cases using the last week of data
  final_week <- data.table::data.table(confirm = shifted_reported_cases[1:(.N - horizon - mean_shift)][max(1, .N - 6):.N]$confirm)[,
                                                                        t := 1:.N]
  lm_model <- stats::lm(log(confirm) ~ t, data = final_week)
  ## Estimate unreported future infections using a log linear model
  shifted_reported_cases <- shifted_reported_cases[,
                                                   t := 1:.N][, 
                                                   t := t - (.N - horizon - mean_shift - 6)][,
                                                   confirm := data.table::fifelse(t >= 7,
                                                                                  exp(lm_model$coefficients[1] + lm_model$coefficients[2] * t),
                                                                                  confirm)][, t := NULL]
  
  ##Drop median generation interval initial values
  shifted_reported_cases <- shifted_reported_cases[-(1:prior_smoothing_window)]
  return(shifted_reported_cases)
}


#' Construct the Required Future Rt assumption
#'
#' @param future_rt A character string or integer. This argument indicates how to set future Rt values. Supported 
#' options are to project using the Rt model ("project"), to use the latest estimate based on partial data ("latest"),
#' to use the latest estimate based on data that is over 50% complete ("estimate"). If an integer is supplied then the Rt estimate
#' from this many days into the future (or past if negative) past will be used forwards in time. 
#' @param delay Numeric mean delay
#' @return A list containing a logical called fixed and an integer called from
create_future_rt <- function(future_rt = "project", delay = 0) {
  out <- list(fixed = FALSE, from = 0)
  if (is.character(future_rt)) {
    future_rt <- match.arg(future_rt,
                           c("project", 
                             "latest",
                             "estimate"))
    if (!(future_rt %in% "project")) {
      out$fixed <- TRUE
      out$from <- ifelse(future_rt %in% "latest", 0, -delay)
    }
  }else if (is.numeric(future_rt)){
    out$fixed <- TRUE
    out$from <- as.integer(future_rt)
  }
  return(out)
}


gp_settings <- function(gp = list(), time = NA) {
  
  if (exists("kernal", gp)) {
    gp$kernal <- match.arg(gp$kernal, 
                           choices = c("se", "matern_3/2"))
  }
  defaults <- list(
    basis_prop = 0.3, 
    boundary_scale = 2, 
    ls_mean = min(data$t, 21, na.rm = TRUE), 
    ls_sd = min(data$t, 21, na.rm = TRUE) / 3, 
    ls_min = 3,
    ls_max = 
    alpha_sd = 0.1, 
    kernal = "se")
  
  # replace default settings with those specified by user
  if (length(gp) != 0) {
    defaults <- defaults[setdiff(names(defaults), names(defaults))]
    gp <- c(defaults, gp)
  }else{
    gp <- defaults
  }
  return(defaults)
}

create_gp_data <- funtion(gp = list(), 
                          data,
                          stationary,
                          future_rt) {
  
  # Define if GP is on or off
  if (is.null(gp)) {
    fixed <- TRUE
    stationary <- TRUE
    gp <- list()
  }else{
    fixed <- FALSE
  }
  
  # define future Rt arguments
  future_rt <- create_future_rt(future_rt = future_rt, 
                                delay = data$seeding_time)

  # set up default options
  gp <- gp_settings(gp, data$t)

  

  # map settings to underlying gp stan requirements
  gp_data <- list(
    fixed = ifelse(fixed, 1, 0),
    stationary = ifelse(stationary, 1, 0),
    future_fixed = ifelse(future_rt$fixed, 1, 0),
    fixed_from = future_rt$from,
    data$M = ceiling((data$t - data$seeding_time) * gp$basis_prop),
    data$L = (data$t - data$seeding_time) * gp$boundary_scale / 2,
    data$ls_meanlog = convert_to_logmean(gp$ls_mean, gp$ls_sd),
    data$ls_sdlog = convert_to_logsd(gp$ls_mean, gp$ls_sd),
    data$ls_min = gp$ls_min,
    data$ls_max = data$t - data$seeding_time - data$horizon,
    data$alpha_sd = gp$alpha_sd,
    data$gp_type <- ifelse(gp$kernal == "se", 0, 
                           ifelse(gp$kernal == "matern_3/2", 1, 0))
  ) 
  
  # Parameters for Hilbert space GP -----------------------------------------
  # no of basis functions

  
}
#' Create Stan Data Required for estimate_infections
#'
#' @param shifted_reported_cases A dataframe of delay shifted reported cases
#' @param no_delays Numeric, number of delays
#' @param mean_shift Numeric, mean delay shift
#' @param break_no Numeric, number of breakpoints
#' @param fixed Logical, should the gaussian process be used for non-parametric 
#' change over time.
#' @param estimate_rt Logical, should Rt be estimated.
#' @inheritParams estimate_infections
#' @inheritParams create_future_rt
#' @importFrom stats lm
#' @importFrom purrr safely
#' @return A list of stan data
#' @export 
create_stan_data <- function(reported_cases, shifted_reported_cases,
                             horizon, no_delays, mean_shift, generation_time,
                             rt_prior, estimate_rt, burn_in, week_effect,
                             stationary, fixed, break_no, future_rt, gp, family,
                             delays) {
  # create future_rt
  future_rt <- create_future_rt(future_rt = future_rt, 
                                delay = mean_shift)
  
  cases <- reported_cases[(mean_shift + 1):(.N - horizon)]$confirm
  
  data <- list(
    day_of_week = reported_cases[(mean_shift + 1):.N]$day_of_week,
    cases = cases,
    shifted_cases = unlist(ifelse(no_delays > 0, list(shifted_reported_cases$confirm),
                                  list(reported_cases$confirm))),
    t = length(reported_cases$date),
    seeding_time = mean_shift,
    horizon = horizon,
    gt_mean_mean = generation_time$mean,
    gt_mean_sd = generation_time$mean_sd,
    gt_sd_mean = generation_time$sd,
    gt_sd_sd = generation_time$sd_sd,
    max_gt = generation_time$max,
    r_mean = rt_prior$mean,
    r_sd = rt_prior$sd,
    estimate_r = ifelse(estimate_rt, 1, 0),
    burn_in = burn_in,
    week_effect = ifelse(week_effect, 1, 0),
    bp_n = break_no,
    breakpoints = reported_cases[(mean_shift + 1):.N]$breakpoint
  ) 
# initial estimate of growth ------------------------------------------
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
  # Delays ------------------------------------------------------------------
  data$delays <- no_delays
  data$delay_mean_mean <- allocate_delays(delays$mean, no_delays)
  data$delay_mean_sd <- allocate_delays(delays$mean_sd, no_delays)
  data$delay_sd_mean <- allocate_delays(delays$sd, no_delays)
  data$delay_sd_sd <- allocate_delays(delays$sd_sd, no_delays)
  data$max_delay <- allocate_delays(delays$max, no_delays)

  ## Set model to poisson or negative binomial
  family <- match.arg(family, c("poisson", "negbin"))
  data$model_type <- ifelse(family %in% "poisson", 0, 1)
  return(data)
}



#' Create Initial Conditions Generating Function
#' @param data A list of data as produced by `create_stan_data.`
#' @inheritParams create_stan_data
#'
#' @return An initial condition generating function
#' @importFrom purrr map2_dbl
#' @importFrom truncnorm rtruncnorm
#' @export
create_initial_conditions <- function(data, delays, rt_prior, generation_time, mean_shift) {
  init_fun <- function(){
    out <- list()
    if (data$delays > 0) {
      out$delay_mean <- array(purrr::map2_dbl(delays$mean, delays$mean_sd, 
                                              ~ truncnorm::rtruncnorm(1, a = 0, mean = .x, sd = .y)))
      out$delay_sd <- array(purrr::map2_dbl(delays$sd, delays$sd_sd, 
                                            ~ truncnorm::rtruncnorm(1, a = 0, mean = .x, sd = .y)))
    }
    if (data$fixed == 0) {
      out$eta <- array(rnorm(data$M, mean = 0, sd = 0.1))
      out$rho <- array(rlnorm(1, mean = data$ls_meanlog, 
                              sd = data$ls_sdlog))
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
      out$log_R <- array(rnorm(n = 1, mean = log(rt_prior$mean^2 / sqrt(rt_prior$sd^2 + rt_prior$mean^2)), 
                               sd = sqrt(log(1 + (rt_prior$sd^2 / rt_prior$mean^2)))))
      out$gt_mean <- array(truncnorm::rtruncnorm(1, a = 0, mean = generation_time$mean,  
                                                 sd = generation_time$mean_sd))
      out$gt_sd <-  array(truncnorm::rtruncnorm(1, a = 0, mean = generation_time$sd,
                                                sd = generation_time$sd_sd))
      if (data$bp_n > 0) {
        out$bp_effects <- array(rnorm(data$bp_n, 0, 0.1))
      }
    }
    return(out)
  }
  return(init_fun)
}



#' Create a List of Stan Arguments
#'
#'
#' @description Generates a list of arguments as required by `rstan::sampling` (when `method = "exact`) or 
#' `rstan::vb` (when `method = "approximate`). See `create_stan_args()` for the defaults and the relevant `rstan`
#' functions for additional options.
#' @param model A stan model object, defaults to packaged model if not supplied.
#' @param data A list of stan data as created by `create_stan_data`
#' @param init Initial conditions passed to `rstan`. Defaults to "random" but can also be a function (
#' as supplied by `create_intitial_conditions`).
#' @param samples Numeric, defaults to 1000. The overall number of posterior samples to return (Note: not the 
#' number of samples per chain as is the default in stan).
#' @param stan_args A list of stan arguments to be passed to `rstan::sampling` or `rstan::vb` (when using the "exact"
#' or "approximate" method). For `method = approximate` an additional argument `trials` indicates the number of attempts to make 
#' using variational inference before returning an error (as stochastic failure is possible). The default for this is 5.
#' @param method A character string defaults to "exact". Also accepts "approximate". Indicates the fitting method to be used
#' this can either be "exact" (NUTs sampling) or "approximate" (variational inference). The exact approach returns samples
#' from the posterior whilst the approximate method returns approximate samples. The approximate method is likely to return results 
#' several order of magnitudes faster than the exact method.
#' @param verbose Logical, defaults to `FALSE`. Should verbose progress messages be returned.
#'
#' @return A list of stan arguments
#' @export
#'
#' @examples
#' 
#' # default settings
#' create_stan_args()
#' 
#' # approximate settings
#' create_stan_args(method = "approximate") 
#' # increasing warmup
#' create_stan_args(stan_args = list(warmup = 1000))
create_stan_args <- function(model, data = NULL, init = "random", 
                             samples = 1000, stan_args = NULL, method = "exact", 
                             verbose = FALSE) {
  # use built in model if not supplied by the user
  if (missing(model)) {
    model <- NULL
  }
  if (is.null(model)) {
    model <- stanmodels$estimate_infections
  }
  # set up shared default arguments
  default_args <- list(
    object = model,
    data = data,
    init = init,
    refresh = ifelse(verbose, 50, 0)
  )
  # set up independent default arguments
  if (method == "exact") {
    default_args$cores <- 4
    default_args$warmup <- 500
    default_args$chains <- 4
    default_args$control <- list(adapt_delta = 0.99, max_treedepth = 15)
    default_args$save_warmup <- FALSE
    default_args$seed <- as.integer(runif(1, 1, 1e8))
  }else if (method == "approximate") {
    default_args$trials <- 10
    default_args$iter <- 10000
    default_args$output_samples <- samples
  }
  # join with user supplied settings
  if (!is.null(stan_args)) {
    default_args <- default_args[setdiff(names(default_args), names(stan_args))]
    args <- c(default_args, stan_args)
  }else{
    args <- default_args
  }
  # set up dependent arguments
  if (method == "exact") {
    args$iter <-  ceiling(samples / args$chains) + args$warmup
  }
  return(args)
}
