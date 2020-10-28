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
#' on the log scale). To use no delays set this to `list()`.
#' @param rt_prior A list contain the mean and standard deviation (sd) of the lognormally distributed prior for
#' Rt. By default this is assumed to be mean 1 with a standard deviation of 1 (note in model these will be mapped to
#' log space). To infer infections and then calculate Rt using backcalculation set this to `list()`.
#' @param prior_smoothing_window Numeric defaults to 7. The number of days over which to take a rolling average
#' for the prior based on reported cases.
#' @param horizon Numeric, defaults to 7. Number of days into the future to forecast.
#' @param model A compiled stan model. By default uses the internal package model.
#' @param samples Numeric, defaults to 1000. Number of samples post warmup.
#' @param week_effect Logical, defaults TRUE. Should weekly reporting effects be estimated.
#' @param use_breakpoints Logical, defaults to TRUE but only active if a `breakpoint` variable is present in the input data. 
#'  Breakpoints should be defined as 1 if present and otherwise 0. By default breakpoints are fit jointly with
#' a global non-parametric effect and so represent a conservative estimate of breakpoint changes. To specify a random walk define
#' breakpoints every n days (so every 7 days for a weekly random walk) and disable the gaussian process using `gp = list()`.
#' @param burn_in Numeric, defaults to 0. The number of initial Rt estimates to discard. This argument may be used to reduce 
#' spurious findings when running `estimate_infections` on a partial timeseries (as the earliest estimates will not be informed by 
#' all cases that occurred only those supplied to `estimate_infections`). The combined delays used will inform the appropriate length
#' of this burn in but 7 days is likely a sensible starting point.
#' @param stationary Logical, defaults to FALSE. Should Rt be estimated with a global mean. When estimating Rt 
#' this should substantially improve run times but will revert to the global average for real time and forecasted estimates.
#' This setting is most appropriate when estimating historic Rt or when combined with breakpoints.
#' @param return_fit Logical, defaults to FALSE. Should the fitted stan model be returned.
#' @param gp List controlling the Gaussian process approximation if set to `list()` then `Rt` is assumed to be constant unless
#' other settings introduce variation. If set must contain the `basis_prop` (number of basis functions based on scaling the time points)
#'  which defaults to 0.3 and must be between 0 and 1 (increasing this increases the accuracy of the approximation and the cost of 
#'  additional compute. Must also contain the `boundary_scale` (multiplied by half the range of the input time series). Increasing this 
#' increases the accuracy of the approximation at the cost of additional compute. 
#' See here: https://arxiv.org/abs/2004.11408 for more information on setting these parameters.
#' Must also contain the  `lengthscale_alpha` and `lengthscale_beta`. These tune the prior of the lengthscale. Principled 
#' values can be obtained using `tune_inv_gamma` which optimises based on the desired truncation (which should be based on the scale
#' of the observed data). The default is tuned to have 98% of the density of the distribution between 2 and 21 days. Finally the list must 
#' contain `alpha_sd` the standard deviation for the alpha parameter of the gaussian process. This defaults to 0.1.
#' @param verbose Logical, defaults to `FALSE`. Should verbose debug progress messages be printed. Corresponds to the "DEBUG" level from 
#' `futile.logger`. See `setup_logging` for more detailed logging options.
#' @param future Logical, defaults to `FALSE`. Should stan chains be run in parallel using `future`. This allows users to have chains
#' fail gracefully (i.e when combined with `max_execution_time`). Should be combined with a call to `future::plan`
#' @param max_execution_time Numeric, defaults to Inf (seconds). If set will kill off processing of each chain if not finished within the specified timeout. 
#' When more than 2 chains finish successfully estimates will still be returned. If less than 2 chains return within the allowed time then estimation 
#' will fail with an informative error.
#' @export
#' @inheritParams create_stan_args
#' @inheritParams create_future_rt
#' @inheritParams fit_model_with_nuts
#' @inheritParams calc_CrIs
#' @importFrom data.table data.table copy merge.data.table as.data.table setorder rbindlist setDTthreads melt .N setDT
#' @importFrom purrr transpose 
#' @importFrom lubridate wday days
#' @importFrom purrr transpose
#' @importFrom futile.logger flog.threshold flog.warn flog.debug
#' @examples
#' \donttest{
#' # get example case counts
#' reported_cases <- EpiNow2::example_confirmed[1:50]
#' 
#' # set up example generation time
#' generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#' # set delays between infection and case report 
#' incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#' reporting_delay <- list(mean = log(3), mean_sd = 0.1,
#'                         sd = log(1), sd_sd = 0.1, max = 15)
#'       
#' # Note: all examples below have been tuned to reduce the runtimes of examples
#' # these settings are not suggesed for real world use.                   
#' # run model with default setting
#' def <- estimate_infections(reported_cases, generation_time = generation_time,
#'                            delays = list(incubation_period, reporting_delay), 
#'                            stan_args = list(warmup = 200,
#'                             control = list(adapt_delta = 0.95, max_treedepth = 15),
#'                             cores = ifelse(interactive(), 4, 1)),
#'                             verbose = interactive())
#'
#' plot(def)
#' 
#' # run model using backcalculation
#' backcalc <- estimate_infections(reported_cases, generation_time = generation_time,
#'                                 delays = list(incubation_period, reporting_delay),
#'                                 stan_args = list(warmup = 200, 
#'                                                  cores = ifelse(interactive(), 4, 1),
#'                                                  control = list(adapt_delta = 0.9)),
#'                                 rt_prior = list(), verbose = interactive())
#'
#' plot(backcalc)
#' 
#' # run the model with default settings using the future backend 
#' ## (combine with a call to future::plan to make this parallel).
#' def_future <- estimate_infections(reported_cases, generation_time = generation_time,
#'                                   delays = list(incubation_period, reporting_delay),
#'                                   stan_args = list(warmup = 200, 
#'                                                    control = list(adapt_delta = 0.9),
#'                                                    cores = ifelse(interactive(), 4, 1)),
#'                                   verbose = interactive(), future = TRUE)
#' 
#' plot(def_future)                        
#'                            
#' # run model with Rt fixed into the future using the latest estimate
#' fixed_rt <- estimate_infections(reported_cases, generation_time = generation_time,
#'                                 delays = list(incubation_period, reporting_delay),
#'                                 stan_args = list(warmup = 200, 
#'                                                  control = list(adapt_delta = 0.9),
#'                                                  cores = ifelse(interactive(), 4, 1)),
#'                                 future_rt = "latest", verbose = interactive())
#'
#' plot(fixed_rt)
#'
#' # run the model with default settings on a later snapshot of 
#' # data (use burn_in here to remove the first week of estimates that may
#' # be impacted by this most).
#' snapshot_cases <- EpiNow2::example_confirmed[80:130]
#' snapshot <- estimate_infections(snapshot_cases, generation_time = generation_time,
#'                                 delays = list(incubation_period, reporting_delay),
#'                                 stan_args = list(warmup = 200, 
#'                                                  control = list(adapt_delta = 0.9),
#'                                                  cores = ifelse(interactive(), 4, 1)),
#'                                 burn_in = 7, verbose = interactive())
#'
#' plot(snapshot) 
#' 
#' # run model with stationary Rt assumption (likely to provide biased real-time estimates)
#' stat <- estimate_infections(reported_cases, generation_time = generation_time,
#'                             delays = list(incubation_period, reporting_delay),
#'                             stan_args = list(warmup = 200, cores = ifelse(interactive(), 4, 1),
#'                                              control = list(adapt_delta = 0.9)),
#'                             stationary = TRUE, verbose = interactive())
#'
#' plot(stat)
#'        
#' # run model with fixed Rt assumption 
#' fixed <- estimate_infections(reported_cases, generation_time = generation_time,
#'                              delays = list(incubation_period, reporting_delay),
#'                              stan_args = list(warmup = 200, cores = ifelse(interactive(), 4, 1),
#'                                               control = list(adapt_delta = 0.9)),
#'                              gp = list(), verbose = interactive())
#'
#' plot(fixed)
#' 
#' # run model with no delays 
#' no_delay <- estimate_infections(reported_cases, generation_time = generation_time,
#'                                 stan_args = list(warmup = 200,
#'                                                  cores = ifelse(interactive(), 4, 1),
#'                                                  control = list(adapt_delta = 0.9)),
#'                                 verbose = interactive())
#'
#' plot(no_delay)    
#'      
#' # add a dummy breakpoint (used only when optionally estimating breakpoints)
#' reported_cases_bp <- data.table::copy(reported_cases)[,
#'               breakpoint := data.table::fifelse(date == as.Date("2020-03-16"), 1, 0)]              
#' # run model with breakpoints but otherwise static Rt
#' # This formulation may increase the apparent effect of the breakpoint but needs to be tested using
#' # model fit criteria (i.e LFO), comparison to models with non-static Rt and models with partially
#' # constrained non-static Rt.           
#' bkp <- estimate_infections(reported_cases_bp, generation_time = generation_time,
#'                             delays = list(incubation_period, reporting_delay),
#'                             stan_args = list(warmup = 200, 
#'                                              cores = ifelse(interactive(), 4, 1),
#'                                              control = list(adapt_delta = 0.9)),
#'                             gp = list(), verbose = interactive())                                                         
#'
#' plot(bkp)
#' # breakpoint effect
#' fbkp$summarised[variable == "breakpoints"]
#' }                                
estimate_infections <- function(reported_cases, 
                                model = NULL, 
                                samples = 1000,
                                stan_args = NULL,
                                method = "exact", 
                                family = "negbin", 
                                generation_time, 
                                CrIs = c(0.2, 0.5, 0.9),
                                delays = list(),
                                horizon = 7,
                                gp = list(basis_prop = 0.3, boundary_scale = 2, 
                                          lengthscale_alpha = 4.5, lengthscale_beta = 21.5,
                                          alpha_sd = 0.1),
                                rt_prior = list(mean = 1, sd = 0.5),
                                week_effect = TRUE, 
                                use_breakpoints = TRUE, 
                                stationary = FALSE, 
                                future_rt = "project",
                                burn_in = 0, 
                                prior_smoothing_window = 7, 
                                future = FALSE, 
                                max_execution_time = Inf, 
                                return_fit = FALSE,
                                id = "estimate_infections",
                                verbose = FALSE){
   
  # store dirty reported case data
  dirty_reported_cases <- data.table::copy(reported_cases)
  # set fall back rt prior and trigger switches
  if (length(rt_prior) == 0) {
    estimate_rt <- FALSE
    rt_prior <- list(mean = 1, sd = 1)
  }else{
    estimate_rt <- TRUE
  }
  
  # If no GP default to stationary setup with no assumed non-parametric change
  # add in placeholder parameters
  if (length(gp) == 0) {
    fixed <- TRUE
    stationary <- TRUE
    gp = list(basis_prop = 1, boundary_scale = 1,
              lengthscale_alpha = 1, lengthscale_beta = 1,
              alpha_sd = 0.1)
  }else{
    fixed <- FALSE
  }

  # Check verbose settings and set logger to match---------------------------
  if (verbose) {
    futile.logger::flog.threshold(futile.logger::DEBUG,
                                  name = "EpiNow2.epinow.estimate_infections")
  }

  # Check breakpoints -------------------------------------------------------
  if (is.null(reported_cases$breakpoint)) {
    reported_cases$breakpoint <- 0
    use_breakpoints <- FALSE
  }
  
  if (use_breakpoints) {
   break_no <- sum(reported_cases$breakpoint, na.rm = TRUE)
   if (break_no == 0) {
     futile.logger::flog.warn("Breakpoint estimation was specified but no breakpoints were detected.",
                              name = "EpiNow2.epinow.estimate_infections")
   }
  }else{
    break_no <- 0
  }
 
  # Organise delays ---------------------------------------------------------
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
  # Create mean shifted reported cases as prior ------------------------------
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
                           horizon = horizon,
                           no_delays = no_delays,
                           mean_shift = mean_shift,
                           generation_time = generation_time,
                           rt_prior = rt_prior, 
                           estimate_rt = estimate_rt,
                           week_effect = week_effect, 
                           stationary = stationary,
                           fixed = fixed,
                           break_no = break_no, 
                           future_rt = future_rt, 
                           gp = gp,
                           family = family,
                           delays = delays)

  # Set up default settings -------------------------------------------------
  args <- create_stan_args(model, data = data, samples = samples, 
                           stan_args = stan_args,
                           init = create_initial_conditions(data, delays, rt_prior, 
                                                            generation_time, mean_shift),
                           method = method, 
                           verbose = verbose)
  
  # Fit model ---------------------------------------------------------------
  if (method == "exact") {
    fit <- fit_model_with_nuts(args,
                               future = future,
                               max_execution_time = max_execution_time,
                               verbose = verbose,
                               id = id)
  }else if (method == "approximate"){
    fit <- fit_model_with_vb(args,
                             verbose = verbose,
                             id = id)
  }
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
                           start_date = start_date,
                           CrIs = CrIs)
  
  ## Join stan fit if required
  if (return_fit) {
    format_out$fit <- fit
  }
  format_out$observations <- dirty_reported_cases
  class(format_out) <- c("estimate_infections", class(format_out))
  return(format_out)
}


#' Fit a Stan Model using the NUTs sampler
#'
#' @param args List of stan arguments
#' @param future Logical, defaults to `FALSE`. Should `future` be used to run stan chains in parallel.
#' @param max_execution_time Numeric, defaults to Inf. What is the maximum execution time per chain in seconds. 
#'     Results will still be returned as long as at least 2 chains complete successfully within the timelimit. 
#' @param id A character string used to assign logging information on error. Used by `regional_epinow` 
#' to assign errors to regions. Alter the default to run with error catching.
#' @param verbose Logical, defaults to `FALSE`. Should verbose progress information be returned.
#' @importFrom futile.logger flog.debug flog.info flog.error
#' @importFrom R.utils withTimeout
#' @importFrom future.apply future_lapply
#' @importFrom purrr compact
#' @importFrom rstan sflist2stanfit sampling
#' @importFrom rlang abort cnd_muffle
#' @return A stan model object
fit_model_with_nuts <- function(args, future = FALSE, max_execution_time = Inf, 
                                id = "stan", verbose = FALSE) {
  if (verbose) {
    futile.logger::flog.debug(paste0("%s: Running in exact mode for ", ceiling(args$iter - args$warmup) * args$chains," samples (across ", args$chains,
                                     " chains each with a warm up of ", args$warmup, " iterations each) and ",
                                     args$data$t," time steps of which ", args$data$horizon, " are a forecast"), 
                              id, name = "EpiNow2.epinow.estimate_infections.fit")
  }
  
  
  if (exists("stuck_chains", args)) {
    stuck_chains <- args$stuck_chains
    args$stuck_chains <- NULL
  }else{
    stuck_chains <- 0
  }
  
  fit_chain <- function(chain, stan_args, max_time, catch = FALSE) {
    stan_args$chain_id <- chain
    if (catch) {
      fit <- tryCatch(withCallingHandlers(
        R.utils::withTimeout(do.call(rstan::sampling, stan_args), 
                             timeout = max_time,
                             onTimeout = "silent"),
        warning = function(w) {
          futile.logger::flog.warn("%s (chain: %s): %s - %s", id, chain, w$message, toString(w$call),
                                   name = "EpiNow2.epinow.estimate_infections.fit")
          rlang::cnd_muffle(w)
        }),
        error = function(e) {
            error_text <- sprintf("%s (chain: %s): %s - %s", id, chain, e$message, toString(e$call))
            futile.logger::flog.error(error_text,
                                      name = "EpiNow2.epinow.estimate_infections.fit")
            return(NULL)
        })
    }else{
      fit <- R.utils::withTimeout(do.call(rstan::sampling, stan_args), 
                                  timeout = max_time,
                                  onTimeout = "silent")
    }
    
    if (is.null(fit) || length(names(fit)) == 0) {
      return(NULL)
    }else{
      return(fit)
    }
  }
  
  if(!future) {
    fit <- fit_chain(1, stan_args = args, max_time = max_execution_time,
                     catch = !id %in% c("estimate_infections", "epinow"))
    if (stuck_chains > 0) {fit <- NULL}
    if (is.null(fit)) {
      rlang::abort("model fitting was timed out - try increasing the max_execution_time")
    }
  }else{
    chains <- args$chains
    args$chains <- 1
    args$cores <- 1
    fits <- future.apply::future_lapply(1:chains, fit_chain, 
                                       stan_args = args, 
                                       max_time = max_execution_time,
                                       catch = TRUE,
                                       future.seed = TRUE)
    if (stuck_chains > 0) {fits[1:stuck_chains] <- NULL}
    fit <- purrr::compact(fits)
    if (length(fit) == 0) {
      fit <- NULL
      if (is.null(fit)) {
        rlang::abort("all chains failed - try inspecting the output for errors or increasing the max_execution_time")
      }
    }else{
      failed_chains <- chains - length(fit)
      if (failed_chains > 0) {
        futile.logger::flog.warn("%s: %s chains failed or were timed out.", id, failed_chains, 
                                 name = "EpiNow2.epinow.estimate_infections.fit")
        if ((chains - failed_chains) < 2) {
          rlang::abort("model fitting failed as too few chains were returned to assess convergence (2 or more required)")
        }
      }
      fit <- rstan::sflist2stanfit(fit)
    }
  }
  return(fit)
}

#' Fit a Stan Model using Variational Inference
#'
#' @inheritParams fit_model_with_nuts
#' @importFrom futile.logger flog.debug flog.info flog.error
#' @importFrom purrr safely
#' @importFrom rstan vb
#' @importFrom rlang abort
#' @return A stan model object
fit_model_with_vb <- function(args, future = FALSE, id = "stan", verbose = FALSE) {
  if (verbose) {
    futile.logger::flog.debug(paste0("%s: Running in approximate mode for ", args$iter, " iterations (with ", args$trials, " attempts). Extracting ",
                                     args$output_samples, " approximate posterior samples for ", args$data$t," time steps of which ",
                                     args$data$horizon, " are a forecast"),
                              id, name = "EpiNow2.epinow.estimate_infections.fit")
  }
  
  if (exists("trials", args)) {
    trials <- args$trials
    args$trials <- NULL
  }else{
    trials <- 1
  }
  
  fit_vb <- function(stan_args) {
    fit <-  do.call(rstan::vb, stan_args)
    
    if (length(names(fit)) == 0) {
      return(NULL)
    }else{
      return(fit)
    }
    return(fit)
  }
  safe_vb <- purrr::safely(fit_vb)
  
  fit <- NULL
  current_trials <- 0
  
  while (current_trials <= trials & is.null(fit)) {
    fit <- safe_vb(args)
    
    error <- fit[[2]]
    fit <- fit[[1]]
    current_trials <- current_trials + 1
  }
  
  if (is.null(fit)) {
    if (is.null(fit)) {
      futile.logger::flog.error("%s: Fitting failed - try increasing stan_args$trials or inspecting the model input",
                                id, name = "EpiNow2.epinow.estimate_infections.fit")
      rlang::abort("Variational Inference failed due to: ", error)
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
#' @inheritParams calc_summary_measures
#' @importFrom data.table fifelse rbindlist 
#' @importFrom lubridate days
#' @return A list of samples and summarised posterior parameter estimates
format_fit <- function(posterior_samples, horizon, shift, burn_in, start_date,
                       CrIs){
  format_out <- list()
  # bind all samples together
  format_out$samples <- data.table::rbindlist(posterior_samples, fill = TRUE, idcol = "variable")
  
  if (is.null(format_out$samples$strat)) {
    format_out$samples <- format_out$samples[, strat := NA]
  }
  # add type based on horizon
  format_out$samples <- format_out$samples[,
                                           type := data.table::fifelse(date > (max(date, na.rm = TRUE) - horizon), 
                                                                       "forecast", 
                                                                       data.table::fifelse(date > (max(date, na.rm = TRUE) - horizon - shift),
                                                                                           "estimate based on partial data",                    
                                                                                           "estimate"))]
  
  # remove burn in period if specified
  if (burn_in > 0) {
    format_out$samples <- format_out$samples[is.na(date) | date >= (start_date + lubridate::days(burn_in))]
  }
  
  # summarise samples
  format_out$summarised <- calc_summary_measures(format_out$samples,
                                                 summarise_by = c("date", "variable", "strat", "type"),
                                                 order_by = c("variable", "date"),
                                                 CrIs = CrIs)
  return(format_out)
}

