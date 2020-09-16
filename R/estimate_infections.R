#' Estimate Infections, the Time-Varying Reproduction Number and the Rate of Growth
#'
#' @description This function uses a non-parametric approach to reconstruct cases by date of infection from reported 
#' cases. It can optionally then estimate the time-varying reproduction number and the rate of growth.
#' @param reported_cases A data frame of confirmed cases (confirm) by date (date). confirm must be integer and date must be 
#' in date format.
#' @param family A character string indicating the reporting model to use. Defaults to negative 
#' binomial ("negbin") with poisson ("poisson") also supported.
#' @param generation_time A list containing the mean, standard deviation of the mean (mean_sd), 
#' standard deviation (sd), standard deviation of the standard deviation and the maximum allowed value for the
#' generation time (assuming a gamma distribution).
#' @param delays A list of delays (i.e incubation period/reporting delay) between infection and report.
#' Each list entry must also be a list containing the mean, standard deviation of the mean (mean_sd), 
#' standard deviation (sd), standard deviation of the standard deviation and the maximum allowed value for the
#' that delay (assuming a lognormal distribution with all parameters excepting the max allowed value 
#' on the log scale).
#' @param rt_prior A list contain the mean and standard deviation (sd) of the gamma distributed prior for
#' Rt. By default this is assumed to be mean 1 with a standard deviation of 1.
#' @param prior_smoothing_window Numeric defaults to 7. The number of days over which to take a rolling average
#' for the prior based on reported cases.
#' @param horizon Numeric, defaults to 7. Number of days into the future to forecast.
#' @param model A compiled stan model. By default uses the internal package model.
#' @param cores Numeric, defaults to 2. The number of cores to use when fitting the stan model.
#' @param chains Numeric, defaults to 2. The number of MCMC chains to use.
#' @param samples Numeric, defaults to 1000. Number of samples post warmup.
#' @param warmup Numeric, defaults to 200. Number of iteration of warmup to use.
#' @param estimate_rt Logical, defaults TRUE. Should Rt be estimated when imputing infections.
#' @param estimate_week_eff Logical, defaults TRUE. Should weekly reporting effects be estimated.
#' @param estimate_breakpoints Logical, defaults to FALSE. Should breakpoints in Rt be estimated. If true then `reported_cases`
#' must contain a `breakpoint` variable that is 1 on the dates with breakpoints and otherwise 0. Breakpoints are fit jointly with
#' a global non-parametric effect and so represent a conservative estimate of breakpoint changes.
#' @param burn_in Numeric, defaults to 0. The number of initial estimates to discard. This argument may be used to reduce 
#' spurious findings when running `estimate_infections` on a partial timeseries (as the earliest estimates will not be informed by 
#' all cases that occurred only those supplied to `estimate_infections`). The combined delays used will inform the appropriate length
#' of this burn in but 7 days is likely a sensible starting point.
#' @param stationary Logical, defaults to FALSE. Should Rt be estimated with a global mean. When estimating Rt 
#' this should substantially improve run times but will revert to the global average for real time and forecasted estimates.
#' This setting is most appropriate when estimating historic Rt or when combined with breakpoints.
#' @param fixed Logical, defaults to FALSE. If TRUE then a Gaussian process is not used and Rt is assumed to be constant over time
#' (apart from any manually included breakpoints). If `estimate_rt` is FALSE then this reduces the backcalculation to a simple mean shift.
#' This option can be used to produce a null model estimate, to produce a single Rt estimate for a short timeseries or as part of a wider 
#' analysis on the impact of interventions.
#' @param fixed_future_rt Logical, defaults to FALSE. IF TRUE then the estimated Rt from the last time point with data is used for all
#' future time points without data. 
#' @param adapt_delta Numeric, defaults to 0.99. See ?rstan::sampling.
#' @param max_treedepth Numeric, defaults to 15. See ?rstan::sampling.
#' @param return_fit Logical, defaults to FALSE. Should the fitted stan model be returned.
#' @param gp List controlling the Gaussian process approximation. Must contain
#' the `basis_prop` (number of basis functions based on scaling the time points) which defaults to 0.3 and must be 
#' between 0 and 1 (increasing this increases the accuracy of the approximation and the cost of additional compute. 
#' Must also contain the `boundary_scale` (multiplied by half the range of the input time series). Increasing this 
#' increases the accuracy of the approximation at the cost of additional compute. 
#' See here: https://arxiv.org/abs/2004.11408 for more information on setting these parameters.
#' Can optionally also contain the  `lengthscale_mean` and `lengthscale_sd`. If these are specified this will override 
#' the defaults of 0 and 2 (normal distributed truncated at zero).
#' @param verbose Logical, defaults to `TRUE`. Should verbose progress messages be printed.
#' @param debug Logical, defaults to `FALSE`. Enables debug model in which additional diagnostics are available
#' @export
#' @importFrom rstan sampling extract 
#' @importFrom data.table data.table copy merge.data.table as.data.table setorder rbindlist setDTthreads melt .N setDT
#' @importFrom purrr transpose map_dbl
#' @importFrom lubridate wday days
#' @importFrom truncnorm rtruncnorm
#' @importFrom stats lm
#' @importFrom HDInterval hdi
#' @importFrom purrr transpose
#' @importFrom futile.logger flog.threshold flog.warn flog.debug
#' @examples
#' \donttest{
#' # Get example case counts
#' reported_cases <- EpiNow2::example_confirmed[1:50]
#' 
#' # Add a dummy breakpoint (used only when optionally estimating breakpoints)
#' reported_cases <- reported_cases[, breakpoint := data.table::fifelse(date == as.Date("2020-03-16"),
#'                                                                      1, 0)]
#' # Set up example generation time
#' generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
#'                         mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
#'                         sd = EpiNow2::covid_generation_times[1, ]$sd,
#'                         sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
#'                         max = 30)
#' # Set delays between infection and case report 
#' # (any number of delays can be specifed here)             
#' incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
#'                           mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
#'                           sd = EpiNow2::covid_incubation_period[1, ]$sd,
#'                           sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
#'                           max = 30)
#'                    
#' reporting_delay <- list(mean = log(5),
#'                         mean_sd = log(2),
#'                         sd = log(2),
#'                         sd_sd = log(1.5),
#'                         max = 30)
#'                         
#' # Run model with default settings
#' def <- estimate_infections(reported_cases, family = "negbin",
#'                            generation_time = generation_time,
#'                            delays = list(incubation_period, reporting_delay),
#'                            samples = 1000, warmup = 200, cores = ifelse(interactive(), 4, 1),
#'                            chains = 4, estimate_rt = TRUE, verbose = FALSE, return_fit = TRUE)
#'
#' 
#' # Plot output
#' plots <- report_plots(summarised_estimates = def$summarised,
#'                       reported = reported_cases)
#'                      
#' plots$summary
#'
#' # Run model with Rt fixed into the future
#' fixed_rt <- estimate_infections(reported_cases, family = "negbin",
#'                                 generation_time = generation_time,
#'                                 delays = list(incubation_period, reporting_delay),
#'                                 samples = 1000, warmup = 200, cores = ifelse(interactive(), 4, 1),
#'                                 chains = 4, estimate_rt = TRUE, fixed_future_rt = TRUE,
#'                                 return_fit = TRUE)
#'
#' 
#' # Plot output
#' plots <- report_plots(summarised_estimates = fixed_rt$summarised,
#'                       reported = reported_cases)
#'                       
#' plots$summary
#'
#'# Run the model with default settings on a later snapshot of 
#'# data (use burn_in here to remove the first week of
#'# estimates that may be impacted by this most).
#' snapshot_cases <- EpiNow2::example_confirmed[80:130]
#' snapshot <- estimate_infections(snapshot_cases, family = "negbin",
#'                                 generation_time = generation_time,
#'                                 delays = list(incubation_period, reporting_delay),
#'                                 samples = 1000, warmup = 400, cores = ifelse(interactive(), 4, 1), 
#'                                 chains = 4, estimate_rt = TRUE, verbose = FALSE, return_fit = TRUE,
#'                                 burn_in = 7)
#'
#' 
#' # Plot output
#' plots <- report_plots(summarised_estimates = snapshot$summarised,
#'                       reported = snapshot_cases)
#'
#' plots$summary    
#' 
#' ## Run model with stationary Rt assumption (likely to provide biased real-time estimates)
#' stat <- estimate_infections(reported_cases, family = "negbin",
#'                             generation_time = generation_time,
#'                             delays = list(incubation_period, reporting_delay),
#'                             samples = 1000, warmup = 200, cores = ifelse(interactive(), 4, 1),
#'                             chains = 4, estimate_rt = TRUE, stationary = TRUE,
#'                             verbose = FALSE, return_fit = TRUE)
#'
#' 
#' # Plot output
#' plots <- report_plots(summarised_estimates = stat$summarised,
#'                       reported = reported_cases)
#'
#' plots$summary
#'        
#' # Run model with fixed Rt assumption 
#' fixed <- estimate_infections(reported_cases, family = "negbin",
#'                             generation_time = generation_time,
#'                             delays = list(incubation_period, reporting_delay),
#'                             samples = 1000, warmup = 200, cores = ifelse(interactive(), 4, 1),
#'                             chains = 4, estimate_rt = TRUE, fixed = TRUE, 
#'                             verbose = FALSE, return_fit = TRUE)
#'
#' 
#' # Plot output
#' plots <- report_plots(summarised_estimates = fixed$summarised,
#'                       reported = reported_cases)
#'                       
#'                       
#' plots$summary
#' 
#' 
#' # Run model with no delays 
#' no_delay <- estimate_infections(reported_cases, family = "negbin",
#'                             generation_time = generation_time,
#'                             samples = 1000, warmup = 200, 
#'                             cores = ifelse(interactive(), 4, 1),
#'                             chains = 4, estimate_rt = TRUE,
#'                             verbose = FALSE, return_fit = TRUE)
#'
#' 
#' # Plot output
#' plots <- report_plots(summarised_estimates = no_delay$summarised,
#'                       reported = reported_cases)
#'                       
#'                       
#' plots$summary         
#'               
#' # Run model with breakpoints                                                                      
#' bkp <- estimate_infections(reported_cases, family = "negbin",
#'                            generation_time = generation_time,
#'                            delays = list(incubation_period, reporting_delay),
#'                            samples = 1000, warmup = 200, cores = ifelse(interactive(), 4, 1), 
#'                            chains = 4, estimate_rt = TRUE, estimate_breakpoints = TRUE, 
#'                            verbose = FALSE, return_fit = TRUE)
#'
#' 
#' # Plot output
#' plots <- report_plots(summarised_estimates = bkp$summarised,
#'                       reported = reported_cases)
#'
#' plots$summary
#'              
#' # Run model with breakpoints but with constrained non-linear change over time 
#' # This formulation may increase the apparent effect of the breakpoint but needs to be tested using
#' # model fit criteria (i.e LFO).                                                                    
#' cbkp <- estimate_infections(reported_cases, family = "negbin",
#'                             generation_time = generation_time,
#'                             gp = list(basis_prop = 0.3, boundary_scale = 2, 
#'                                      lengthscale_mean = 20, lengthscale_sd = 1),
#'                             delays = list(incubation_period, reporting_delay),
#'                             samples = 1000, warmup = 200, cores = ifelse(interactive(), 4, 1),
#'                             chains = 4, estimate_rt = TRUE, estimate_breakpoints = TRUE,
#'                             verbose = FALSE, return_fit = TRUE)
#'
#' 
#' # Plot output
#' plots <- report_plots(summarised_estimates = cbkp$summarised,
#'              reported = reported_cases)
#'              
#' plots$summary
#' 
#' # Pull out breakpoint summary
#' cbkp$summarised[variable == "breakpoints"]
#' 
#' # Run model with breakpoints but otherwise static Rt
#' # This formulation may increase the apparent effect of the breakpoint but needs to be tested using
#' # model fit criteria (i.e LFO).                                                                    
#' fbkp <- estimate_infections(reported_cases, family = "negbin",
#'                             generation_time = generation_time,
#'                             delays = list(incubation_period, reporting_delay),
#'                             samples = 1000, warmup = 200, cores = ifelse(interactive(), 4, 1),
#'                             chains = 4, estimate_breakpoints = TRUE, fixed = TRUE, 
#'                             verbose = FALSE, return_fit = TRUE)
#'
#' 
#' # Plot output
#' plots <- report_plots(summarised_estimates = fbkp$summarised,
#'              reported = reported_cases)
#'              
#' plots$summary
#' 
#' # Pull out breakpoint summary
#' fbkp$summarised[variable == "breakpoints"]
#' 
#' # Run model without Rt estimation (just backcalculation)
#' backcalc <- estimate_infections(reported_cases, family = "negbin",
#'                                 generation_time = generation_time,
#'                                 delays = list(incubation_period, reporting_delay),
#'                                 samples = 1000, warmup = 200, cores = ifelse(interactive(), 4, 1),
#'                                 chains = 4, estimate_rt = FALSE, verbose = FALSE, return_fit = TRUE)
#'
#'  
#' # plot just infections as report_plots does not support the backcalculation only model
#' plot_estimates(estimate = backcalc$summarised[variable == "infections"],
#'                reported = reported_cases, ylab = "Cases")
#' }                                
estimate_infections <- function(reported_cases, family = "negbin",
                                generation_time, delays,
                                gp = list(basis_prop = 0.3, boundary_scale = 2),
                                rt_prior = list(mean = 1, sd = 1),
                                prior_smoothing_window = 7,
                                horizon = 7, model, cores = 1, chains = 4,
                                samples = 1000, warmup = 200,
                                estimate_rt = TRUE, estimate_week_eff = TRUE,
                                estimate_breakpoints = FALSE, burn_in = 0,
                                stationary = FALSE, fixed = FALSE, fixed_future_rt = FALSE,
                                adapt_delta = 0.99, max_treedepth = 15, 
                                return_fit = FALSE, verbose = TRUE, debug = FALSE){
  

  # Check fix setting -------------------------------------------------------
  if (fixed) {
    stationary <- TRUE
  }

  # Check verbose settings and set logger to match---------------------------
  if (verbose) {
    futile.logger::flog.threshold(futile.logger::DEBUG)
  }

  # Check breakpoints -------------------------------------------------------
  if (is.null(reported_cases$breakpoint)) {
    reported_cases$breakpoint <- NA
  }
  
  if (estimate_breakpoints) {
   break_no <- sum(reported_cases$breakpoint, na.rm = TRUE)
   if (break_no == 0) {
     futile.logger::flog.warn("Breakpoint estimation was specified but no breakpoints were detected.")
   }
  }else{
    break_no <- 0
  }
 
  # Organise delays ---------------------------------------------------------
  if (missing(delays)) {
    delays <- list()
  }
  
  no_delays <- length(delays)
  
  if (no_delays > 0) {
    delays <- purrr::transpose(delays)
  }
  
  # Set up data.table -------------------------------------------------------

  suppressMessages(data.table::setDTthreads(threads = 1))
  
  reported_cases <- data.table::setDT(reported_cases)
  
  # Make sure there are no missing dates and order cases --------------------
  reported_cases_grid <- data.table::copy(reported_cases)[, .(date = seq(min(date), max(date) + horizon, by = "days"))]
  
  reported_cases <- data.table::merge.data.table(
    reported_cases , reported_cases_grid, 
    by = c("date"), all.y = TRUE)
  

  
  reported_cases <- reported_cases[is.na(confirm), confirm := 0][,.(date = date, confirm, breakpoint)]
  reported_cases <- reported_cases[is.na(breakpoint), breakpoint := 0]
  reported_cases <- data.table::setorder(reported_cases, date)
  
  ## Filter out 0 reported cases from the beginning of the data
  reported_cases <- reported_cases[order(date)][,
                                 cum_cases := cumsum(confirm)][cum_cases > 0][, 
                                 cum_cases := NULL]
  
  # Record earliest date with data ------------------------------------------
  
  start_date <- min(reported_cases$date, na.rm = TRUE)
  
  # Estimate the mean delay -----------------------------------------------
 
  if (no_delays > 0) {
    mean_shift <- as.integer(
      sum(
        purrr::map2_dbl(delays$mean, delays$sd, ~ exp(.x + .y^2/2))
      )
    )
  }else{
    mean_shift <- 1
  }


  # Add the mean delay and incubation period on as 0 case days ------------
  
  if (no_delays > 0) {
    reported_cases <- data.table::rbindlist(list(
      data.table::data.table(date = seq(min(reported_cases$date) - mean_shift - prior_smoothing_window, 
                                        min(reported_cases$date) - 1, by = "days"),
                             confirm = 0,
                             breakpoint = 0),
      reported_cases
    ))  
    
  }

  # Calculate smoothed prior cases ------------------------------------------
  
  if (no_delays > 0) {
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
                                                   confirm)][,
                    t := NULL]
    
    ##Drop median generation interval initial values
    shifted_reported_cases <- shifted_reported_cases[-(1:prior_smoothing_window)]
    reported_cases <- reported_cases[-(1:prior_smoothing_window)]
  }

  
  # Add week day info -------------------------------------------------------
  
  reported_cases <- reported_cases[, day_of_week := lubridate::wday(date, week_start = 1)]
  
  # Define stan model parameters --------------------------------------------
  
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
  
  allocate_delays <- function(delay_var, no_delays = data$delays) {
    if (no_delays > 0) {
      out <- unlist(delay_var)
    }else{
      out <- 1
    }
    return(array(out))
  }
  
  data$delay_mean_mean <- allocate_delays(delays$mean)
  data$delay_mean_sd <- allocate_delays(delays$mean_sd)
  data$delay_sd_mean <- allocate_delays(delays$sd)
  data$delay_sd_sd <- allocate_delays(delays$sd_sd)
  data$max_delay <- allocate_delays(delays$max)

  # Parameters for Hilbert space GP -----------------------------------------
  
  # no of basis functions
  data$M <- ceiling(data$rt * gp$basis_prop)
  # Boundary value for c
  data$L <- max(data$time) * gp$boundary_scale
  
  if (is.null(gp$lengthscale_mean)) {
    data$lengthscale_mean <- 0
  }else{
    data$lengthscale_mean <- gp$lengthscale_mean
  }
  
  if (is.null(gp$lengthscale_sd)) {
    data$lengthscale_sd <- 2
  }else{
    data$lengthscale_sd <- gp$lengthscale_sd
  }
  
  ## Set model to poisson or negative binomial
  if (family %in% "poisson") {
    data$model_type <- 0
  }else if (family %in% "negbin"){
    data$model_type <- 1
  }
  

  # Set up initial conditions fn --------------------------------------------
  
  init_fun <- function(){
    
    out <- list()
    
    if (data$delays > 0) {
      out$delay_mean <- array(purrr::map2_dbl(delays$mean, delays$mean_sd, 
                                         ~ truncnorm::rtruncnorm(1, a = 0, mean = .x, sd = .y)))
      out$delay_sd <- array(purrr::map2_dbl(delays$sd, delays$sd_sd, 
                                       ~ truncnorm::rtruncnorm(1, a = 0, mean = .x, sd = .y)))
  
    }

  if (!fixed) {
    out$eta <- array(rnorm(data$M, mean = 0, sd = 1))
    out$rho <- array(truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = 2))
    out$alpha <- array(truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = 0.1))
  }
  if (data$model_type == 1) {
    out$rep_phi <- array(rexp(1, 1))
  }
  
  if (estimate_rt) {
    out$initial_infections <- array(rnorm(mean_shift, mean = 0, sd = 0.1))
    out$initial_R <- array(rgamma(n = 1, shape = (rt_prior$mean / rt_prior$sd)^2, 
                          scale = (rt_prior$sd^2) / rt_prior$mean))
    out$gt_mean <- array(truncnorm::rtruncnorm(1, a = 0, mean = generation_time$mean,  
                                               sd = generation_time$mean_sd))
    out$gt_sd <-  array(truncnorm::rtruncnorm(1, a = 0, mean = generation_time$sd,
                                              sd = generation_time$sd_sd))
    
    if (break_no > 0) {
      out$rt_break_eff <- array(rlnorm(break_no, 0, 0.1))
    }
  }
  
  return(out)
  }
  
  # Load and run the stan model ---------------------------------------------
  if (missing(model)) {
    model <- NULL
  }
  
  if (is.null(model)) {
    model <- stanmodels$estimate_infections
  }
  
  if (verbose) {
    futile.logger::flog.debug(paste0("Running for ", samples," samples (across ", chains,
                                     " chains each with a warm up of ", warmup, " iterations each) and ",
                                     data$t," time steps of which ", horizon, " are a forecast"))
  }

  fit <- rstan::sampling(model, data = data, chains = chains,
                         init = init_fun, 
                         iter = ceiling(samples / chains) + warmup, 
                         warmup = warmup, cores = cores,
                         control = list(adapt_delta = adapt_delta,
                                        max_treedepth = max_treedepth),
                         refresh = ifelse(verbose, 50, 0),
                         save_warmup = debug)
  
  # Extract parameters of interest from the fit -----------------------------
  
  ## Extract sample from stan object
  samples <- rstan::extract(fit)
  
  ## Construct reporting list
  out <- list()
  
  ## Generic data.frame reporting function
  extract_parameter <- function(param, samples, dates) {
    param_df <- data.table::as.data.table(
      t(
        data.table::as.data.table(
          samples[[param]]
        )
      ))
    
    param_df <- param_df[, time := 1:.N]
    param_df <- 
      data.table::melt(param_df, id.vars = "time",
                       variable.name = "var")
    
    param_df <- param_df[, var := NULL][, sample := 1:.N, by = .(time)]
    param_df <- param_df[, date := dates, by = .(sample)]
    param_df <- param_df[, .(parameter = param, time, date, 
                             sample, value)]
    
    return(param_df)
  }
  
  ## Report infections, and R
  out$infections <- extract_parameter("infections", 
                                      samples,
                                      reported_cases$date)
  
  out$reported_cases <- extract_parameter("imputed_reports", 
                                          samples, 
                                          reported_cases$date[-(1:mean_shift)])
  
  if (estimate_rt) {
    out$R <- extract_parameter("R", 
                               samples,
                               reported_cases$date[-(1:mean_shift)])
    
    out$growth_rate <- extract_parameter("r", 
                                         samples,
                                         reported_cases$date[-(1:mean_shift)])
    
    if (break_no > 0) {
      out$breakpoints <- extract_parameter("rt_break_eff", 
                                           samples, 
                                           1:break_no)
      
      out$breakpoints <- out$breakpoints[, strat := date][, c("time", "date") := NULL]
    }
  }
  
    
    if (estimate_week_eff) {
      out$day_of_week <- extract_parameter("day_of_week_eff", 
                                           samples,
                                           1:7)
      
      char_day_of_week <- data.table::data.table(wday = c("Monday", "Tuesday", "Wednesday",
                                                          "Thursday", "Friday", "Saturday",
                                                          "Sunday"),
                                                 time = 1:7)
      out$day_of_week <- out$day_of_week[char_day_of_week, on = "time"][, 
                                         strat := as.character(wday)][,`:=`(time = NULL, date = NULL, wday = NULL)]
    }
 
  if (data$delays > 0) {
    out$delay_mean <- extract_parameter("delay_mean", samples, 1:data$delays)
    out$delay_mean <- out$delay_mean[, strat := as.character(time)][,
                                                                    time := NULL][, date := NULL]
    
    out$delay_sd <- extract_parameter("delay_sd", samples, 1:data$delays)
    out$delay_sd <- out$delay_sd[, strat :=  as.character(time)][,
                                                                 time := NULL][, date := NULL]
    
  }

    extract_static_parameter <- function(param) {
      data.table::data.table(
        parameter = param,
        sample = 1:length(samples[[param]]),
        value = samples[[param]])
    }
    
    if (estimate_rt) {
      out$gt_mean <- extract_static_parameter("gt_mean")
      out$gt_mean <- out$gt_mean[, value := value.V1][, value.V1 := NULL]
      
      out$gt_sd <- extract_static_parameter("gt_sd")
      out$gt_sd <- out$gt_sd[, value := value.V1][, value.V1 := NULL]
    }
  
    ## Add prior infections
    if (no_delays > 0) {
      out$prior_infections <- shifted_reported_cases[, 
                .(parameter = "prior_infections", time = 1:.N, 
                  date, value = confirm, sample = 1)]
      
    }
    
# Format output -----------------------------------------------------------
    
 format_out <- list()
  
 ## Bind all samples together
 format_out$samples <- data.table::rbindlist(out, fill = TRUE, idcol = "variable")
 
 if (is.null(format_out$samples$strat)) {
  format_out$samples <- format_out$samples[, strat := NA]
 }
 ## Add type based on horizon
 format_out$samples <- format_out$samples[,
          type := data.table::fifelse(date > (max(date, na.rm = TRUE) - horizon), 
                                      "forecast", 
                                      data.table::fifelse(date > (max(date, na.rm = TRUE) - horizon - mean_shift),
                                      "estimate based on partial data",                    
                                      "estimate"))]
 
 ## Remove burn in period if specified
 if (burn_in > 0) {
   format_out$samples <- format_out$samples[is.na(date) | date >= (start_date + lubridate::days(burn_in))]
 }
 
 ## Summarise samples
 format_out$summarised <- data.table::copy(format_out$samples)[, .(
   bottom  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.9)), ~ .[[1]])),
   top = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.9)), ~ .[[2]])),
   lower  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.5)), ~ .[[1]])),
   upper = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.5)), ~ .[[2]])),
   central_lower = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.2)), ~ .[[1]])), 
   central_upper = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.2)), ~ .[[2]])),
   median = as.numeric(median(value, na.rm = TRUE)),
   mean = as.numeric(mean(value, na.rm = TRUE)),
   sd = as.numeric(sd(value, na.rm = TRUE))), by = .(date, variable, strat, type)]
 
 ## Order summarised samples
 data.table::setorder(format_out$summarised, variable, date)  
 
 if (return_fit) {
  format_out$fit <- fit
 }
 
  return(format_out)
}
