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
#' @param stan_args List, defaults to `NULL`. User supplied arguments to be passed to `rstan::sampling`. Overwrites all 
#' defaults.
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
#' @param future Logical, defaults to `FALSE`. Should stan chains be run in parallel using `future`. This allows users to have chains
#' fail gracefully (i.e when combined with `max_execution_time`). Should be combined with a call to `future::plan`
#' @param max_execution_time Numeric, defaults to Inf. If set will kill off processing of each chain if not finished within the specified timeout. 
#' When more than 2 chains finish successfully estimates will still be returned. If less than 2 chains return within the allowed time then estimation 
#' will fail with an informative error.
#' @export
#' @inheritParams fit_model
#' @importFrom data.table data.table copy merge.data.table as.data.table setorder rbindlist setDTthreads melt .N setDT
#' @importFrom purrr transpose 
#' @importFrom lubridate wday days
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
#' # Run the model with default settings using the future backend 
#' ## (combine with a call to future::plan to make this parallel).
#' def_future <- estimate_infections(reported_cases, family = "negbin",
#'                                   generation_time = generation_time, 
#'                                  delays = list(incubation_period, reporting_delay),
#'                                  samples = 500, warmup = 200, chains = 2, 
#'                                  verbose = FALSE, future = TRUE)
#' 
#' plots <- report_plots(summarised_estimates = def_future$summarised,
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
                                gp = list(basis_prop = 0.3, boundary_scale = 2,
                                          lengthscale_mean = 0, lengthscale_sd = 2),
                                rt_prior = list(mean = 1, sd = 1),
                                prior_smoothing_window = 7,
                                horizon = 7, model, cores = 1, chains = 4,
                                samples = 1000, warmup = 200, stan_args = NULL,
                                estimate_rt = TRUE, estimate_week_eff = TRUE,
                                estimate_breakpoints = FALSE, burn_in = 0,
                                stationary = FALSE, fixed = FALSE, fixed_future_rt = FALSE,
                                adapt_delta = 0.99, max_treedepth = 15, 
                                future = FALSE, max_execution_time = Inf,
                                return_fit = FALSE, verbose = TRUE, debug = FALSE, stuck_chains = 0){
  

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
  
  # Make sure there are no missing dates and order cases --------------------
  reported_cases <- create_clean_reported_cases(reported_cases, horizon)

  # Record earliest date with data ------------------------------------------
  start_date <- min(reported_cases$date, na.rm = TRUE)
  
  # Estimate the mean delay -----------------------------------------------
  if (no_delays > 0) {
    mean_shift <- as.integer(sum(purrr::map2_dbl(delays$mean, delays$sd, ~ exp(.x + .y^2/2))))
  }else{
    mean_shift <- 1
  }
  # Add the mean delay and incubation period on as 0 case days ------------
  # Create mean shifted reported cases as prio ------------------------------
  if (no_delays > 0) {
    reported_cases <- data.table::rbindlist(list(
      data.table::data.table(date = seq(min(reported_cases$date) - mean_shift - prior_smoothing_window, 
                                        min(reported_cases$date) - 1, by = "days"),
                             confirm = 0,  breakpoint = 0),
      reported_cases))  
    
    shifted_reported_cases <- create_shifted_cases(reported_cases, mean_shift, 
                                                   prior_smoothing_window, horizon)
    reported_cases <- reported_cases[-(1:prior_smoothing_window)]
  }
  
  # Add week day info -------------------------------------------------------
  reported_cases <- reported_cases[, day_of_week := lubridate::wday(date, week_start = 1)]
  
  # Define stan model parameters --------------------------------------------
  data <- create_stan_data(reported_cases = reported_cases, 
                           shifted_reported_cases = shifted_reported_cases,
                           horizon = horizon, no_delays = no_delays,
                           mean_shift = mean_shift, generation_time = generation_time,
                           rt_prior = rt_prior, estimate_rt = estimate_rt,
                           estimate_week_eff = estimate_week_eff, stationary = stationary,
                           fixed = fixed, break_no = break_no, 
                           fixed_future_rt = fixed_future_rt,  gp = gp,
                           family = family, delays = delays)

  # Set up default settings -------------------------------------------------
  if (missing(model)) {
    model <- NULL
  }
  
  if (is.null(model)) {
    model <- stanmodels$estimate_infections
  }
  
 default_args <- list(
   object = model,
   data = data,
   init = create_initial_conditions(data, delays, rt_prior, generation_time, mean_shift),
   iter = ceiling(samples / chains) + warmup,
   warmup = warmup, 
   cores = cores, 
   chains = chains,
   control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
   refresh = ifelse(verbose, 50, 0),
   save_warmup = debug)  

 # Join with user supplied settings ----------------------------------------
 if (!is.null(stan_args)) {
   default_args <- default_args[[setdiff(names(default_args), names(stan_args))]]
   args <- c(default_args, stan_args)
 }else{
   args <- default_args
 }
 
  # Fit model ---------------------------------------------------------------
  fit <- fit_model(args, future = future, max_execution_time = max_execution_time,
                   verbose = verbose, stuck_chains = stuck_chains)
  
  # Extract parameters of interest from the fit -----------------------------
  out <- extract_parameter_samples(fit, data, 
                                   reported_inf_dates = reported_cases$date,
                                   reported_dates = reported_cases$date[-(1:mean_shift)])
  
    ## Add prior infections
    if (no_delays > 0) {
      out$prior_infections <- shifted_reported_cases[, 
                .(parameter = "prior_infections", time = 1:.N, 
                  date, value = confirm, sample = 1)]
      
    }
    
# Format output -----------------------------------------------------------
  format_out <- format_fit(posterior_samples = out, 
                           horizon = horizon,
                           shift = mean_shift,
                           burn_in = burn_in,
                           start_date = start_date)
  
  ## Join stan fit if required
  if (return_fit) {
    format_out$fit <- fit
  }
  
  return(format_out)
}


#' Fit a Stan Model
#'
#' @param args List of stan arguments
#' @param future Logical, defaults to `FALSE`. Should `future` be used to run stan chains in parallel.
#' @param max_execution_time Numeric, defauls to Inf. What is the maximum execution time per chain. Results will
#' still be returned as long as at least 2 chains complete successfully within the timelimit. 
#' @param verbose Logical, defaults to `FALSE`. Should verbose progress information be returned.
#' @param stuck_chains Numeric, defaults to 0. Used to test failing chain scenarios
#' @importFrom futile.logger flog.debug flog.info flog.error
#' @importFrom R.utils withTimeout
#' @importFrom future.apply future_lapply
#' @importFrom purrr compact
#' @importFrom rstan sflist2stanfit sampling
#' @return A stan model object
fit_model <- function(args, future = FALSE, max_execution_time = Inf, verbose = FALSE, 
                      test = FALSE, stuck_chains = 0) {
  if (verbose) {
    futile.logger::flog.debug(paste0("Running for ", ceiling(args$iter - args$warmup) * args$chains," samples (across ", args$chains,
                                     " chains each with a warm up of ", args$warmup, " iterations each) and ",
                                     args$data$t," time steps of which ", args$data$horizon, " are a forecast"))
  }
  
  fit_chain <- function(chain, stan_args, max_time) {
    fit <- R.utils::withTimeout(do.call(rstan::sampling, stan_args), 
                                timeout = max_time,
                                onTimeout = "silent")
    return(fit)
  }
  
  stop_timeout <- function() {
    if (is.null(fit)) {
      futile.logger::flog.error("timed out")
      stop("model fitting timed out - try increasing max_execution_time")
    }
  }
  
  if(!future) {
    fit <- fit_chain(1, stan_args = args, max_time = max_execution_time)
    if (stuck_chains > 0) {fit <- NULL}
    stop_timeout()
  }else{
    chains <- args$chains
    args$chains <- 1
    args$cores <- 1
     
    fits <- future.apply::future_lapply(1:chains, fit_chain, 
                                       stan_args = args, 
                                       max_time = max_execution_time)
    if (stuck_chains > 0) {fits[[1:stuck_chains]] <- NULL}
    fit <- purrr::compact(fits)
    if (length(fit) == 0) {
      fit <- NULL
      stop_timeout()
    }else{
      failed_chains <- chains - length(fit)
      if (failed_chains > 0) {
        futile.logger::flog.info(paste0(failed_chains, " chains failed or were timed out."))
        if ((chains - failed_chains) < 2) {
          stop("model fitting failed as too few chains were returned to assess convergence (2 or more required)")
        }
      }
      fit <- rstan::sflist2stanfit(fit)
    }
  }
  return(fit)
}

#' Format Posterior Samples
#'
#' @param posterior_samples A list of posterior samples as returned by `extract_parameter_samples`
#' @param horizon Numeric, forecast horizon
#' @param shift Numeric, the shift to apply to estimates
#' @param burn_in Numeric, number of days to discard estimates for
#' @param start_date Date, earliest date with data
#' @importFrom data.table fifelse rbindlist copy setorder
#' @importFrom lubridate days
#' @importFrom purrr map_dbl
#' @importFrom HDInterval hdi
#' @return A list of samples and summarised posterior parameter estimates
format_fit <- function(posterior_samples, horizon, shift, burn_in, start_date){
 
  format_out <- list()
  
  ## Bind all samples together
  format_out$samples <- data.table::rbindlist(posterior_samples, fill = TRUE, idcol = "variable")
  
  if (is.null(format_out$samples$strat)) {
    format_out$samples <- format_out$samples[, strat := NA]
  }
  ## Add type based on horizon
  format_out$samples <- format_out$samples[,
                                           type := data.table::fifelse(date > (max(date, na.rm = TRUE) - horizon), 
                                                                       "forecast", 
                                                                       data.table::fifelse(date > (max(date, na.rm = TRUE) - horizon - shift),
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
  
  return(format_out)
}

