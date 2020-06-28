#' Estimate Infections, the Time-Varying Reproduction Number and the Rate of Growth
#'
#' @description This function uses a non-parametric approach to reconstruct cases by date of infection from reported 
#' cases. It can optionally then estimate the time-varying reproduction number and the rate of growth.
#' @param reported_cases A data frame of confirmed cases (confirm) by date (date).
#' @param family A character string indicating the reporting model to use. Defaults to negative 
#' binomial ("negbin") with poisson ("poisson") also supported.
#' @param generation_time A list containing the mean, standard deviation of the mean (mean_sd), 
#' standard deviation (sd), standard deviation of the standard deviation and the maximum allowed value for the
#' generation time (assuming a gamma distribution).
#' @param incubation_period  A list containing the mean, standard deviation of the mean (mean_sd), 
#' standard deviation (sd), standard deviation of the standard deviation and the maximum allowed value for the
#' incubation period (assuming a lognormal distribution with all parameters excepting the max allowed value 
#' on the log scale).
#' @param reporting_delay A list containing the mean, standard deviation of the mean (mean_sd), 
#' standard deviation (sd), standard deviation of the standard deviation and the maximum allowed value for the
#' reporting delay (assuming a lognormal distribution with all parameters excepting the max allowed value 
#' on the log scale).
#' @param rt_prior A list contain the mean and standard deviation (sd) of the gamma distributed prior for
#' Rt. By default this is assumed to be mean 1 with a standard deviation of 1.
#' @param prior_smoothing_window Numeric defaults to 7. The number of days over which to take a rolling average
#' for the prior based on reported cases.
#' @param horizon Numeric, defaults to 14. Number of days into the future to forecast.
#' @param model A compiled stan model. By default uses the internal package model.
#' @param cores Numeric, defaults to 2. The number of cores to use when fitting the stan model.
#' @param chains Numeric, defaults to 2. The number of MCMC chains to use.
#' @param estimate_rt Logical, defaults TRUE. Should Rt be estimated when imputing infections.
#' @param adapt_delta Numeric, defaults to 0.99. See ?rstan::sampling.
#' @param max_treedepth Numeric, defaults to 15. See ?rstan::sampling.
#' @param return_fit Logical, defaults to FALSE. Should the fitted stan model be returned.
#' @param verbose Logical, defaults to FALSE. Should verbose progress messages be printed.
#' @export
#' @importFrom rstan sampling extract 
#' @importFrom data.table data.table copy merge.data.table as.data.table setorder rbindlist setDTthreads melt .N setDT
#' @importFrom purrr transpose map_dbl
#' @importFrom lubridate wday
#' @importFrom truncnorm rtruncnorm
#' @importFrom HDInterval hdi
#' @examples
#' \dontrun{
#' ## Get example case counts and format
#' reported_cases <- NCoVUtils::get_ecdc_cases(countries = "Russia")
#' reported_cases <- NCoVUtils::format_ecdc_data(reported_cases)
#' reported_cases <- data.table::as.data.table(reported_cases)[, confirm := cases][, cases := NULL][1:90]
#'  
#' ## Set up example generation time
#' generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
#'                         mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
#'                         sd = EpiNow2::covid_generation_times[1, ]$sd,
#'                         sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
#'                         max = 30)
#' ## Set                   
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
#' ## Run model
#' out <- EpiNow2::estimate_infections(reported_cases, family = "negbin",
#'                                     generation_time = generation_time,
#'                                     incubation_period = incubation_period,
#'                                     reporting_delay = reporting_delay,
#'                                     samples = 1000, warmup = 500,
#'                                     rt_prior = list(mean = 1, sd = 1),
#'                                     cores = 4, chains = 4,
#'                                     estimate_rt = TRUE,
#'                                     verbose = TRUE, return_fit = TRUE)
#'
#' out   
#' }                                
estimate_infections <- function(reported_cases, family = "negbin",
                                incubation_period, reporting_delay,
                                generation_time, rt_prior,
                                prior_smoothing_window = 7,
                                horizon = 14,
                                model, cores = 1, chains = 2,
                                samples = 1000, warmup = 1000,
                                estimate_rt = TRUE, adapt_delta = 0.99,
                                max_treedepth = 15, return_fit = FALSE,
                                verbose = FALSE){
  
  suppressMessages(data.table::setDTthreads(threads = cores))
  
  reported_cases <- data.table::setDT(reported_cases)
  
  # Add prior for R if missing ---------------------------------
  
  if (missing(rt_prior)) {
    rt_prior <- list(mean = 1, sd = 1)
  }
  
  # Make sure there are no missing dates and order cases --------------------
  reported_cases_grid <- data.table::copy(reported_cases)[, .(date = seq(min(date), max(date) + horizon, by = "days"))]
  
  reported_cases <- data.table::merge.data.table(
    reported_cases , reported_cases_grid, 
    by = c("date"), all.y = TRUE)
  
  reported_cases <- reported_cases[is.na(confirm), confirm := 0 ][,.(date = date, confirm)]
  reported_cases <- data.table::setorder(reported_cases, date)
  
  ## Filter out 0 reported cases from the beginning of the data
  reported_cases <- reported_cases[order(date)][,
                                 cum_cases := cumsum(confirm)][cum_cases > 0][, cum_cases := NULL]
  
  # Estimate the mean delay -----------------------------------------------
  
  mean_shift <- as.integer(exp(incubation_period$mean) + exp(reporting_delay$mean))
  
  # Add the mean delay and incubation period on as 0 case days ------------
  
  reported_cases <- data.table::rbindlist(list(
    data.table::data.table(date = seq(min(reported_cases$date) - mean_shift - prior_smoothing_window, 
                                      min(reported_cases$date) - 1, by = "days"),
                           confirm = 0),
    reported_cases
  ))  
  
  # Calculate smoothed prior cases ------------------------------------------
  
  shifted_reported_cases <- data.table::copy(reported_cases)[,
                    confirm := data.table::shift(confirm, n = mean_shift,
                                                 type = "lead", 
                                                 fill = NA)][,
                    confirm := data.table::frollmean(confirm, n = prior_smoothing_window, 
                                                     align = "right", fill = 0)][,
                                                     confirm := data.table::fifelse(confirm == 0, 1e-3, confirm)]
  
  ## Forecast trend on reported cases using the last week of data
  final_week <- data.table::data.table(confirm = shifted_reported_cases[1:(.N - horizon - mean_shift)][max(1, .N - 6):.N]$confirm)[,
                                            t := 1:.N]
  lm_model <- lm(log(confirm) ~ t, data = final_week)
  
  
  ## Estimate unreported future infections using a log linear model
  shifted_reported_cases <- shifted_reported_cases[, t := 1:.N][, 
                                                     t := t - (.N - horizon - mean_shift - 6)][,
                                                     confirm := data.table::fifelse(t >= 7, 
                                                                                    exp(lm_model$coefficients[1] +
                                                                                          lm_model$coefficients[2] * t),
                                                                                    confirm)][, t := NULL]
  ##Drop median generation interval initial values
  shifted_reported_cases <- shifted_reported_cases[-(1:prior_smoothing_window)]
  reported_cases <- reported_cases[-(1:prior_smoothing_window)]
  
  # Add week day info -------------------------------------------------------
  
  reported_cases <- reported_cases[, day_of_week := lubridate::wday(date, week_start = 1)]
  
  # Define stan model parameters --------------------------------------------
  
  data <- list(
    day_of_week = reported_cases$day_of_week,
    cases = reported_cases[1:(.N - horizon)]$confirm,
    shifted_cases = shifted_reported_cases$confirm,
    t = length(reported_cases$date),
    rt = length(reported_cases$date) - mean_shift,
    horizon = horizon,
    inc_mean_mean = incubation_period$mean,
    inc_mean_sd = incubation_period$mean_sd,
    inc_sd_mean = incubation_period$sd,
    inc_sd_sd = incubation_period$sd_sd,
    max_inc = incubation_period$max,
    rep_mean_mean = reporting_delay$mean,
    rep_mean_sd = reporting_delay$mean_sd,
    rep_sd_mean = reporting_delay$sd,
    rep_sd_sd = reporting_delay$sd_sd,
    max_rep = reporting_delay$max,
    gt_mean_mean = generation_time$mean,
    gt_mean_sd = generation_time$mean_sd,
    gt_sd_mean = generation_time$sd,
    gt_sd_sd = generation_time$sd_sd,
    max_gt = generation_time$max,
    r_mean = rt_prior$mean,
    r_sd = rt_prior$sd,
    estimate_r = ifelse(estimate_rt, 1, 0)
  )  
  
  ## Set model to poisson or negative binomial
  if (family %in% "poisson") {
    data$model_type <- 0
  }else if (family %in% "negbin"){
    data$model_type <- 1
  }
  
  # Set up initial conditions fn --------------------------------------------
  
  init_fun <- function(){out <- list(
    eta = rnorm(data$t, mean = 0, sd = 1),
    rho = rlnorm(1, 1.609438, 0.5),
    alpha =  truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = 1),
    inc_mean = truncnorm::rtruncnorm(1, a = 0, mean = incubation_period$mean, sd = incubation_period$mean_sd),
    inc_sd = truncnorm::rtruncnorm(1, a = 0, mean = incubation_period$sd, sd = incubation_period$sd_sd),
    rep_mean = truncnorm::rtruncnorm(1, a = 0, mean = reporting_delay$mean, sd = reporting_delay$mean_sd),
    rep_sd = truncnorm::rtruncnorm(1, a = 0, mean = reporting_delay$sd,  sd = reporting_delay$sd_sd))
  
  if (data$model_type == 1) {
    out$rep_phi <- array(rexp(1, 1))
  }
  
  if (estimate_rt) {
    out$R <- array(rgamma(n = 1, shape = (rt_prior$mean / rt_prior$sd)^2, 
                          scale = (rt_prior$sd^2) / rt_prior$mean))
    out$R_eta <- rnorm(data$rt, mean = 0, sd = 1)
    out$R_rho <- array(rlnorm(1, 1.609438, 0.5))
    out$R_alpha <-  array(truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = 1))
    out$gt_mean <- array(truncnorm::rtruncnorm(1, a = 1, mean = generation_time$mean,  
                                               sd = generation_time$mean_sd))
    out$gt_sd <-  array(truncnorm::rtruncnorm(1, a = 0, mean = generation_time$sd,
                                              sd = generation_time$sd_sd))
    
    if (data$model_type == 1) {
      out$inf_phi <- array(rexp(1, 1))
    }
  }
  
  return(out)
  }
  
  # Load and run the stan model ---------------------------------------------
  
  if (missing(model)) {
    model <- stanmodels$estimate_infections
  }
  
  if (verbose) {
    message(paste0("Running for ", samples + warmup," samples and ", data$t," time steps of which ", horizon, " are a forecast"))
  }
  
  fit <-
    rstan::sampling(model,
                    data = data,
                    chains = chains,
                    init = init_fun,
                    iter = samples + warmup, 
                    warmup = warmup,
                    cores = cores,
                    control = list(adapt_delta = adapt_delta,
                                   max_treedepth = max_treedepth),
                    refresh = ifelse(verbose, 50, 0))
  
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
  out$infections <- extract_parameter("imputed_infections", 
                                      samples,
                                      reported_cases$date)
  
  out$reported_cases <- extract_parameter("imputed_reports", 
                                          samples, 
                                          reported_cases$date)
  
  if (estimate_rt) {
    out$R <- extract_parameter("R", 
                               samples,
                               reported_cases$date[-(1:mean_shift)])
    
    out$growth_rate <- extract_parameter("r", 
                                         samples,
                                         reported_cases$date[-(1:mean_shift)])
    
    out$reported_cases_rt <- extract_parameter("imputed_branch_reports", 
                                               samples, 
                                               reported_cases$date[-(1:mean_shift)])
  }
  
    ## Add prior infections
    out$prior_infections <- shifted_reported_cases[, .(parameter = "prior_infections", time = 1:.N, 
                                                       date, value = confirm, sample = 1)]
    
    out$noise <- extract_parameter("noise", 
                                   samples,
                                   reported_cases$date)
    
    out$day_of_week <- extract_parameter("day_of_week_eff", 
                                         samples,
                                         1:7)
    
    char_day_of_week <- data.table::data.table(wday = c("Monday", "Tuesday", "Wednesday",
                                                        "Thursday", "Friday", "Saturday",
                                                        "Sunday"),
                                               time = 1:7)
    out$day_of_week <- out$day_of_week[char_day_of_week, on = "time"][, 
                                       strat := wday][,`:=`(time = NULL, date = NULL, wday = NULL)]
    
    extract_static_parameter <- function(param) {
      data.table::data.table(
        parameter = param,
        sample = 1:length(samples[[param]]),
        value = samples[[param]])
    }
    
    out$inc_mean <- extract_static_parameter("inc_mean")
    
    out$inc_sd <- extract_static_parameter("inc_sd")
    
    out$rep_mean <- extract_static_parameter("rep_mean")
    
    out$rep_sd <- extract_static_parameter("rep_sd")
    
    if (estimate_rt) {
      out$gt_mean <- extract_static_parameter("gt_mean")
      out$gt_mean <- out$gt_mean[, value := value.V1][, value.V1 := NULL]
      
      out$gt_sd <- extract_static_parameter("gt_sd")
      out$gt_sd <- out$gt_sd[, value := value.V1][, value.V1 := NULL]
    }
  

# Format output -----------------------------------------------------------
    
 format_out <- list()
  
 ## Bind all samples together
 format_out$samples <- data.table::rbindlist(out, fill = TRUE, idcol = "variable")
 
 ## Add type based on horizon
 format_out$samples <- format_out$samples[,
          type := data.table::fifelse(date > (max(date, na.rm = TRUE) - horizon), 
                                      "forecast", 
                                      data.table::fifelse(date > (max(date, na.rm = TRUE) - horizon - mean_shift),
                                      "estimate based on partial data",                    
                                      "estimate"))]
 
 ## Summarise samples
 format_out$summarised <- data.table::copy(format_out$samples)[, .(
   bottom  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.9)), ~ .[[1]])),
   top = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.9)), ~ .[[2]])),
   lower  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.5)), ~ .[[1]])),
   upper = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.5)), ~ .[[2]])),
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