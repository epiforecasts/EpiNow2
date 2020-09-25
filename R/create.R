#' Create Clean Reported Cases
#'
#' @inheritParams estimate_infections
#' @importFrom data.table copy merge.data.table setorder setDT
#' @return A cleaned data frame of reported cases
create_clean_reported_cases <- function(reported_cases, horizon) {
  
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
  
  return(reported_cases)
}

#' Create a Data Frame of Mean Delay Shifted Cases
#'
#' @inheritParams estimate_infections
#' @inheritParams create_stan_data
#' @importFrom data.table copy shift frollmean fifelse .N
#' @importFrom stats lm
#' @return A dataframe for shifted reported cases
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



#' Create Stan Data Required for estimate_infections
#'
#' @param shifted_reported_cases A dataframe of delay shifted reported cases
#' @param no_delays Numeric, number of delays
#' @param mean_shift Numeric, mean delay shift
#' @param break_no Numeric, number of breakpoints
#' @inheritParams estimate_infections
#' @return A list of stan data
create_stan_data <- function(reported_cases,  shifted_reported_cases,
                             horizon, no_delays, mean_shift, generation_time,
                             rt_prior, estimate_rt, estimate_week_eff, stationary,
                             fixed, break_no, fixed_future_rt, gp, family, delays) {
  
  data <- list(
    day_of_week = reported_cases[(mean_shift + 1):.N]$day_of_week,
    cases = reported_cases[(mean_shift + 1):(.N - horizon)]$confirm,
    shifted_cases = unlist(ifelse(no_delays > 0, list(shifted_reported_cases$confirm),
                                  list(reported_cases$confirm))),
    t = length(reported_cases$date),
    rt = length(reported_cases$date) - mean_shift,
    time = 1:(length(reported_cases$date) - mean_shift),
    inf_time = 1:(length(reported_cases$date)),
    horizon = horizon,
    gt_mean_mean = generation_time$mean,
    gt_mean_sd = generation_time$mean_sd,
    gt_sd_mean = generation_time$sd,
    gt_sd_sd = generation_time$sd_sd,
    max_gt = generation_time$max,
    r_mean = rt_prior$mean,
    r_sd = rt_prior$sd,
    estimate_r = ifelse(estimate_rt, 1, 0),
    est_week_eff = ifelse(estimate_week_eff, 1, 0),
    stationary = ifelse(stationary, 1, 0),
    fixed = ifelse(fixed, 1, 0),
    break_no = break_no,
    breakpoints = reported_cases[(mean_shift + 1):.N]$breakpoint,
    future_fixed = ifelse(fixed_future_rt, 1, 0)
  ) 
  
  
  # Delays ------------------------------------------------------------------
  
  data$delays <- no_delays
  data$delay_mean_mean <- allocate_delays(delays$mean, no_delays)
  data$delay_mean_sd <- allocate_delays(delays$mean_sd, no_delays)
  data$delay_sd_mean <- allocate_delays(delays$sd, no_delays)
  data$delay_sd_sd <- allocate_delays(delays$sd_sd, no_delays)
  data$max_delay <- allocate_delays(delays$max, no_delays)
  
  # Parameters for Hilbert space GP -----------------------------------------
  
  # no of basis functions
  data$M <- ceiling(data$rt * gp$basis_prop)
  # Boundary value for c
  data$L <- max(data$time) * gp$boundary_scale
  data$lengthscale_mean <- gp$lengthscale_mean
  data$lengthscale_sd <- gp$lengthscale_sd
  
  
  ## Set model to poisson or negative binomial
  if (family %in% "poisson") {
    data$model_type <- 0
  }else if (family %in% "negbin"){
    data$model_type <- 1
  }
  return(data)
}



#' Create Initial Conditions Generating Function
#' @param data A list of data as produced by `create_stan_data.`
#' @inheritParams create_stan_data
#'
#' @return An initial condition generating function
#' @importFrom purrr map2_dbl
#' @importFrom truncnorm rtruncnorm
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
      out$eta <- array(rnorm(data$M, mean = 0, sd = 1))
      out$rho <- array(truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = 2))
      out$alpha <- array(truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = 0.1))
    }
    if (data$model_type == 1) {
      out$rep_phi <- array(rexp(1, 1))
    }
    
    if (data$estimate_r == 1) {
      out$initial_infections <- array(rnorm(mean_shift, mean = 0, sd = 0.1))
      out$initial_R <- array(rgamma(n = 1, shape = (rt_prior$mean / rt_prior$sd)^2, 
                                    scale = (rt_prior$sd^2) / rt_prior$mean))
      out$gt_mean <- array(truncnorm::rtruncnorm(1, a = 0, mean = generation_time$mean,  
                                                 sd = generation_time$mean_sd))
      out$gt_sd <-  array(truncnorm::rtruncnorm(1, a = 0, mean = generation_time$sd,
                                                sd = generation_time$sd_sd))
      
      if (data$break_no > 0) {
        out$rt_break_eff <- array(rlnorm(data$break_no, 0, 0.1))
      }
    }
    
    return(out)
  }
  
  return(init_fun)
}