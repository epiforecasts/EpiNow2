#' Report case counts by date of report                        
#' @param case_estimates A data.table of case estimates with the following variables: date, sample, cases
#' @param case_forecast A data.table of case forecasts with the following variables: date, sample, cases. If not supplied the
#' default is not incoperate forecasts.
#' @param reporting_effect A `data.table` giving the weekly reporting effect with the following variables:
#' `sample` (must be the same as in `nowcast`), `effect` (numeric scaling factor for each weekday), `day`
#' (numeric 1 - 7 (1 = Monday and 7 = Sunday)). If not supplied then no weekly reporting effect is assumed.
#' @export
#' @return A list of `data.table`s. The first entry contains the following variables `sample`, `date` and `cases` with the second
#' being summarised across samples.
#' @inheritParams estimate_infections
#' @inheritParams adjust_infection_to_report
#' @importFrom data.table data.table rbindlist setorder
#' @importFrom future.apply future_lapply
#' @importFrom HDInterval hdi
#' @examples 
#' \dontrun{
#' ## Define example cases
#' cases <- data.table::as.data.table(EpiSoon::example_obs_cases) 
#' 
#' cases <- cases[, `:=`(confirm = as.integer(cases))]
#' 
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
#' out <- EpiNow2::estimate_infections(cases, family = "negbin",
#'                                     generation_time = generation_time,
#'                                     incubation_period = incubation_period,
#'                                     reporting_delay = reporting_delay,
#'                                     samples = 1000, warmup = 500,
#'                                     estimate_rt =  FALSE, verbose = TRUE)
#'                             
#'                      
#' reported_cases <- report_cases(case_estimates = out$samples[variable == "infections"][, 
#'                                                             cases := value][, value := NULL],
#'                                reporting_delay = reporting_delay,
#'                                incubation_period =  incubation_period, 
#'                                type = "sample")
#'                                
#' print(reported_cases)
#' }
report_cases <- function(case_estimates,
                         case_forecast = NULL, 
                         reporting_delay,
                         incubation_period,
                         type = "sample",
                         reporting_effect) {
  
  samples <- length(unique(case_estimates$sample))
  
  ##Sample report delay and incubation period
  ## Define a single report delay distribution
  delay_defs <- EpiNow2::lognorm_dist_def(mean = reporting_delay$mean,
                                         mean_sd = reporting_delay$mean_sd,
                                         sd = reporting_delay$sd,
                                         sd_sd = reporting_delay$sd_sd,
                                         max_value = reporting_delay$max,
                                         samples = samples)

  ## Define a single incubation period
  incubation_defs <- EpiNow2::lognorm_dist_def(mean = incubation_period$mean,
                                               mean_sd = incubation_period$mean_sd,
                                               sd = incubation_period$sd,
                                               sd_sd = incubation_period$sd_sd,
                                               max_value = incubation_period$max,
                                               samples = samples)
  ## Add a null reporting effect if missing
  if (missing(reporting_effect)) {
    reporting_effect <- data.table::data.table(
      sample = list(1:nrow(delay_defs)),
      effect = rep(1, 7),
      day = 1:7
    )
    
    reporting_effect <- reporting_effect[, .(sample = unlist(sample)), by = .(effect, day)]
  }
  
  ## Filter and sum nowcast to use only upscaled cases by date of infection
  infections <- data.table::copy(case_estimates)
  
  ## Add in case forecast if present
  if (!is.null(case_forecast)) {
    infections <- data.table::rbindlist(list(
      infections,
      case_forecast[, .(date, sample, cases = as.integer(cases))]
    ), use.names = TRUE)
  }
  
  ## For each sample map to report date
  report <- future.apply::future_lapply(1:max(infections$sample), 
                     function(id) {EpiNow2::adjust_infection_to_report(infections[sample == id], 
                                                          delay_def = delay_defs[id,],
                                                          incubation_def = incubation_defs[id, ],
                                                          type = type,
                                                          reporting_effect = reporting_effect[sample == id, ]$effect)})

  report <- data.table::rbindlist(report, idcol = "sample")
    
  out <- list()
  
  ## Bind all samples together
  out$samples <- report
  
  ## Summarise samples
  out$summarised <- report[, .(
    bottom  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.9)), ~ .[[1]])),
    top = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.9)), ~ .[[2]])),
    lower  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.5)), ~ .[[1]])),
    upper = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.5)), ~ .[[2]])),
    median = as.numeric(median(cases, na.rm = TRUE)),
    mean = as.numeric(mean(cases, na.rm = TRUE)),
    sd = as.numeric(sd(cases, na.rm = TRUE))), by = .(date)]
  
  ## Order summarised samples
  data.table::setorder(out$summarised, date) 
  
  return(out)
}


#' Provide Summary Statistics for Estimated Infections and Rt
#' @param summarised_estimates A data.table of summarised estimates containing the following variables:
#'  variable, median, bottom, and top. It should contain the following estimates: R, infections, and r 
#'  (rate of growth).
#' @param rt_samples A data.table containing Rt samples with the following variables: sample and value.
#' @return A data.table containing formatted and numeric summary measures
#' @export
#' @importFrom data.table data.table
#' @importFrom purrr map
report_summary <- function(summarised_estimates,
                           rt_samples) { 
  
  ## Extract values of interest
  summarised_estimates <- summarised_estimates[, .(variable, point = median,
                                                   lower = bottom, upper = top,
                                                   mid_lower = lower, min_upper = upper)]
  ## Extract latest R estimate
  R_latest <- summarised_estimates[variable == "R"][, variable := NULL][,
                                   purrr::map(.SD, ~ round(., 1))]
   
  ## Estimate probability of control
  prob_control <- rt_samples[, .(prob_control = sum(value <= 1) / .N)]$prob_control
  prob_control <- signif(prob_control, 2)
  
  ##Extract current cases
  current_cases <- summarised_estimates[variable == "infections"][, variable := NULL]
  

  ## Get individual estimates
  r_latest <- summarised_estimates[variable == "growth_rate"][, variable := NULL][,
                                  purrr::map(.SD, ~ round(., 2))]
  
  doubling_time <- function(r) {
    round(log(2) * 1 / r, 2)
  }

  doubling_time_latest <- summarised_estimates[variable == "growth_rate"][,
                                .(point = doubling_time(point),
                                  lower = doubling_time(upper),
                                  upper = doubling_time(lower))]
  

  ## Regional summary
 summary <- data.table::data.table(
    measure = c("New confirmed cases by infection date",
                "Expected change in daily cases",
                "Effective reproduction no.",
                "Rate of growth",
                "Doubling/halving time (days)"),
    estimate = c(make_conf(current_cases),
                 as.character(EpiNow2::map_prob_change(prob_control)),
                 make_conf(R_latest, digits = 1),
                 make_conf(r_latest, digits = 2),
                 make_conf(doubling_time_latest, digits = 1)),
    numeric_estimate = list(current_cases,
                         prob_control,
                         R_latest,
                         r_latest,
                         doubling_time_latest)
  )

  return(summary) 
}