#' Report Case Nowcast Estimates
#'
#' @description Returns a summarised nowcast as well as saving key information to the
#' results folder.
#' @param target Character string indicting the data type to use as the "nowcast".
#' @param target_folder Character string indicating the folder into which to save results. 
#'  Also used to extract previously generated results. 
#' @return NULL
#' @export
#' @importFrom data.table rbindlist
#' @importFrom purrr pmap
#' @inheritParams sample_approx_dist
report_nowcast <- function(nowcast, cases, 
                            target, target_folder) {
  
  ## Combine nowcast with observed cases by onset and report
  reported_cases <- cases[import_status %in% "local",
                          .(median = sum(confirm), 
                            type = "Observed by report date",
                            confidence = 1), by = "date"]
  
  
  ## Count cumulative cases
  all_cases <- data.table::rbindlist(list(summarised_cast, 
                                          reported_cases), fill = TRUE)
  
  ## Save combined data
  saveRDS(all_cases,  paste0(target_folder, "/summarised_nowcast.rds"))
  
  ## Extract latest cases
  current_cases <- all_cases[type %in% "nowcast"][
    date == max(date)][, .(date, range = purrr::pmap(
      list(mean, bottom, top),
      function(mean, bottom, top) {
        list(point = mean,
             lower = bottom, 
             upper = top,
             mid_lower = lower,
             mid_upper = upper)
      }))]
  
  
  latest_date <- current_cases$date
  
  saveRDS(latest_date,  paste0(target_folder, "/latest_date.rds"))
  
  current_cases <- current_cases$range
  
  saveRDS(current_cases,  paste0(target_folder, "/current_cases.rds"))
  
  return(invisible(NULL))
}


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
#' reported_cases <- report_cases(case_estimates = out$samples[variable == "infections"][, cases := value][, value := NULL],
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
      case_forecast[, .(sample, date, cases = as.integer(cases))]
    ))
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

#' Report Effective Reproduction Number Estimates
#' @inheritParams report_nowcast
#' @return NULL
#' @export
report_reff <- function(target_folder) {
  
  ## Get summarised nowcast 
  summarised_nowcast <- readRDS(paste0(target_folder, "/summarised_nowcast.rds"))
  
  ## Pull out R estimates
  reff_estimates <- readRDS(paste0(target_folder, "/summarised_reff.rds"))
  bigr_estimates <- reff_estimates[rt_type %in% "nowcast"]
  
  ## Data.table of confidence estimates
  case_confidence <- summarised_nowcast[, .(type, confidence, date)]
  
  ## Join confidence onto R estimates
  bigr_estimates <- case_confidence[bigr_estimates, on = c("type", "date")][
    !is.na(confidence)]
  
  saveRDS(bigr_estimates,
          paste0(target_folder, "/bigr_estimates.rds"))
  
  # Pull out and plot big R -------------------------------------------------
  
  extract_bigr_values <- function(max_var, sel_var) {
    
    out <- EpiNow2::pull_max_var(bigr_estimates, max_var,
                                sel_var, type_selected = "nowcast")
    
    return(out)
  }
  
  ## Pull summary measures
  R_max_estimate <- extract_bigr_values("median", "R0_range")
  
  
  saveRDS(R_max_estimate,
          paste0(target_folder, "/bigr_eff_max_estimate.rds"))
  
  R_latest <- extract_bigr_values("date", "R0_range")
  
  saveRDS(R_latest,
          paste0(target_folder, "/bigr_eff_latest.rds"))
  
  ## Pull out probability of control
  prob_control <- extract_bigr_values("date", "prob_control")
  prob_control <-  signif(prob_control, 2)
  
  saveRDS(prob_control,
          paste0(target_folder, "/prob_control_latest.rds"))
  
  return(invisible(NULL))
}

#' Report Rate of Growth Estimates
#' @inheritParams report_nowcast
#' @return NULL
#' @export
#' @importFrom data.table copy rbindlist as.data.table dcast
#' @importFrom purrr map
report_littler <- function(target_folder) {
  
  
  ## Data.table of confidence estimates
  summarised_nowcast <- readRDS(paste0(target_folder, "/summarised_nowcast.rds"))
  case_confidence <- summarised_nowcast[, .(type, confidence, date)]
  case_confidence <- case_confidence[type %in% "nowcast"]
  
  ## Merge in case confidence
  littler_estimates <- readRDS(paste0(target_folder, "/summarised_littler.rds"))
  littler_estimates$time_varying_r[[1]] <- 
    case_confidence[littler_estimates$time_varying_r[[1]], on = "date"][
      !is.na(confidence)][, type := NULL]
  
  saveRDS(littler_estimates,
          paste0(target_folder, "/rate_spread_estimates.rds"))
  
  ## get overall estimates
  report_overall <- data.table::copy(littler_estimates)[,
                     .(report_overall = purrr::map(overall_little_r,
                                                   ~ purrr::map_dfr(., function(estimate) {
                                                     paste0(signif(estimate$mean, 2), " (",
                                                            signif(estimate$bottom, 2), " -- ",
                                                            signif(estimate$top, 2),")")})), type)][,
                     .(data.table::as.data.table(type), data.table::rbindlist(report_overall))]
                                                                                                                                  
  
  report_overall <- report_overall[, .(Data = type,
                                       `Rate of growth` = little_r,
                                       `Doubling/halving time (days)` = doubling_time,
                                       `Adjusted R-squared` = goodness_of_fit)]
  
  saveRDS(report_overall,
          paste0(target_folder, "/rate_spread_overall_summary.rds"))
  
  clean_double <- function(var, type) {
    var <- signif(var, 2)
    return(var)
  }
  
  ## get latest estimates
  report_latest <-  littler_estimates[, .(type,
                                          latest = purrr::map(time_varying_r, function(estimate) {
                                            estimate <- estimate[date == max(date)]
                                            
                                            estimate$bottom <- clean_double(estimate$bottom, 
                                                                            type = estimate$vars[1])
                                            estimate$top <- clean_double(estimate$top, 
                                                                         type = estimate$vars[1])
                                            estimate$mean <- clean_double(estimate$mean,
                                                                          type = estimate$vars[1])
                                            
                                            out <- data.table::data.table(
                                              vars = estimate$var,
                                              range = paste0(estimate$mean, " (",
                                                             estimate$bottom, " -- ", estimate$top,
                                                             ")")
                                            )
                                            
                                            return(out)
                                          }))] 
  
  report_latest <- report_latest[, .(data.table::as.data.table(type),
                                     data.table::rbindlist(latest))][,
                                   .(type, vars, range)]
  
  report_latest <- data.table::dcast(report_latest, type ~ vars, value.var = "range")
  
  report_latest <- report_latest[, .(Data = type,
                                     `Rate of growth` = little_r,
                                     `Doubling/halving time (days)` = doubling_time,
                                     `Adjusted R-squared` = goodness_of_fit)]
  
  saveRDS(report_latest,
          paste0(target_folder, "/rate_spread_latest_summary.rds"))
  
  
  ## Get individual estimates
  rate_spread_latest <- report_latest[Data == "nowcast"]$`Rate of growth`
  
  
  saveRDS(rate_spread_latest,
          paste0(target_folder, "/rate_spread_latest.rds"))
  
  doubling_time_latest <- report_latest[Data == "nowcast"]$`Doubling/halving time (days)`
  
  saveRDS(doubling_time_latest,
          paste0(target_folder, "/doubling_time_latest.rds"))
  
  adjusted_r_latest <- report_latest[Data == "nowcast"]$`Adjusted R-squared`
  
  saveRDS(adjusted_r_latest,
          paste0(target_folder, "/adjusted_r_latest.rds"))
  
  
  ## Tidy time-varying little R
  tidy_littler <- littler_estimates[type %in% "nowcast"][,
                        .(data.table::as.data.table(type),
                          data.table::rbindlist(time_varying_r))][,
                        var := factor(var, levels = c("little_r", "doubling_time", "goodness_of_fit"),
                        labels = c("Rate of growth", "Doubling/halving time (days)", "Adjusted R-squared"))]
  
  saveRDS(tidy_littler,
          paste0(target_folder, "/time_varying_littler.rds"))
  
  return(invisible(NULL))
}


#' Provide Summary Statistics on an Rt Pipeline
#'
#' @return NULL
#' @export
#' @inheritParams report_nowcast
#' @importFrom data.table data.table
report_summary <- function(target_folder) { 
  
  current_cases <- readRDS(paste0(target_folder, "/current_cases.rds"))
  prob_control <- readRDS(paste0(target_folder, "/prob_control_latest.rds"))
  R_latest <- readRDS(paste0(target_folder, "/bigr_eff_latest.rds"))
  doubling_time_latest <- readRDS(paste0(target_folder, "/doubling_time_latest.rds"))
  
  ## Regional summary
  region_summary <- data.table::data.table(
    measure = c("New confirmed cases by infection date",
                "Expected change in daily cases",
                "Effective reproduction no.",
                "Doubling/halving time (days)"),
    estimate = c(EpiNow2::make_conf(current_cases),
                 as.character(EpiNow2::map_prob_change(prob_control)),
                 EpiNow2::make_conf(R_latest, digits = 1),
                 doubling_time_latest
    )
  )
  
  saveRDS(region_summary, paste0(target_folder, '/region_summary.rds'))
  
  return(invisible(NULL)) 
}