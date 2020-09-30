#' Real-time Rt Estimation, Forecasting and Reporting
#'
#' @description Estimate Rt and cases by date of infection, forecast into the future, transform to date 
#' of report and then save summary measures and plots.
#' @param target_date Date, defaults to maximum found in the data if not specified.
#' @param target_folder Character string specifying where to save results (will create if not present).
#' @param return_estimates Logical, defaults to TRUE. Should estimates be returned.
#' @return A list of output from estimate_infections, forecast_infections,  report_cases, and report_summary.
#' @export
#' @inheritParams estimate_infections
#' @inheritParams forecast_infections
#' @importFrom data.table setDT
#' @importFrom lubridate days
#' @importFrom futile.logger flog.fatal
#' @examples
#' \donttest{
#' ## Construct example distributions
#' generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#' incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#' reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1), max_value = 30)
#' 
#' ## Example case data
#' reported_cases <- EpiNow2::example_confirmed[1:40] 
#' 
#' ## Report Rt along with forecasts
#' out <- epinow(reported_cases = reported_cases, generation_time = generation_time,
#'               delays = list(incubation_period, reporting_delay),
#'               stan_args = list(warmup = 200, cores = ifelse(interactive(), 4, 1)))
#' 
#' out
#' 
#' ## For optional forecasting
#' if(requireNamespace("EpiSoon")){
#'    if(requireNamespace("forecastHybrid")){
#'
#'    ## Report Rt along with forecasts
#'    out <- epinow(reported_cases = cases, samples = 1000, 
#'                  generation_time = generation_time,
#'                  delays = list(incubation_period, reporting_delay),
#'                  forecast_model = function(y, ...){
#'                    EpiSoon::forecastHybrid_model(
#'                      y = y[max(1, length(y) - 21):length(y)],
#'                      model_params = list(models = "aefz", weights = "equal"),
#'                      forecast_params = list(PI.combination = "mean"), ...)},
#'                   stan_args = list(warmup = 200, cores = ifelse(interactive(), 4, 1)))
#' 
#' out
#'    }
#' }
#'
#' }
#'
epinow <- function(reported_cases, model, samples = 1000, stan_args, 
                   method = "exact", family = "negbin",
                   generation_time, delays,
                   gp = list(basis_prop = 0.3, boundary_scale = 2,
                             lengthscale_mean = 0, lengthscale_sd = 2),
                   rt_prior = list(mean = 1, sd = 1), horizon = 7,
                   estimate_rt = TRUE, estimate_week_eff = TRUE, estimate_breakpoints = FALSE,
                   burn_in = 0, stationary = FALSE, fixed = FALSE, fixed_future_rt = FALSE,
                   future = FALSE, max_execution_time = Inf, prior_smoothing_window = 7,
                   return_fit = FALSE, forecast_model, ensemble_type = "mean",
                   return_estimates = TRUE, target_folder, target_date, verbose = FALSE) {

  if (!return_estimates & missing(target_folder)) {
    futile.logger::flog.fatal("Either return estimates or save to a target folder")
    stop("Either return estimates or save to a target folder")
  }

 # Check arguments ---------------------------------------------------------

  if (missing(delays)) {
    delays <- list()
  }
  
  if (missing(stan_args)) {
    stan_args <- NULL
  }
 # Convert input to DT -----------------------------------------------------
  suppressMessages(data.table::setDTthreads(threads = 1))
  reported_cases <- data.table::setDT(reported_cases)

  # Set up folders ----------------------------------------------------------

  if (missing(target_date)) {
    target_date <- max(reported_cases$date)
  }

  if (missing(target_folder)) {
    target_folder <- NULL
  }


  if (!is.null(target_folder)) {
    latest_folder <- file.path(target_folder, "latest")
    target_folder <- file.path(target_folder, target_date)

    if (!dir.exists(target_folder)) {
      dir.create(target_folder, recursive = TRUE)
    }
  }
  # Make sure the horizon is as specified from the target date --------------

  if (horizon != 0) {
    horizon <- horizon + as.numeric(as.Date(target_date) - max(reported_cases$date))
  }


  # Save input data ---------------------------------------------------------

  if (!is.null(target_folder)) {
    latest_date <- reported_cases[confirm > 0][date == max(date)]$date

    saveRDS(latest_date, paste0(target_folder, "/latest_date.rds"))
    saveRDS(reported_cases, paste0(target_folder, "/reported_cases.rds"))
  }

  # Estimate infections and Reproduction no ---------------------------------

  if (missing(model)) {
    model <- NULL
  }

  estimates <- estimate_infections(reported_cases = reported_cases, model = model, samples = samples, stan_args = stan_args,
                                   method = method, family = family,
                                   generation_time = generation_time, delays = delays, horizon = horizon,
                                   gp = gp, rt_prior = rt_prior,
                                   estimate_rt = estimate_rt, estimate_week_eff = estimate_week_eff, estimate_breakpoints = estimate_breakpoints,
                                   stationary = stationary, fixed = fixed, fixed_future_rt = fixed_future_rt,
                                   burn_in = burn_in, prior_smoothing_window = prior_smoothing_window, future = future,
                                   max_execution_time = max_execution_time, return_fit = return_fit, verbose = verbose)

  # Report estimates --------------------------------------------------------
  if (!is.null(target_folder)) {
    saveRDS(estimates$samples, paste0(target_folder, "/estimate_samples.rds"))
    saveRDS(estimates$summarised, paste0(target_folder, "/summarised_estimates.rds"))

    if (return_fit) {
      saveRDS(estimates$fit, paste0(target_folder, "/model_fit.rds"))
    }
  }
  # Forecast infections and reproduction number -----------------------------
  if (!missing(forecast_model)) {
    forecast <- forecast_infections(infections = estimates$summarised[variable == "infections"][type != "forecast"][, type := NULL],
                                    rts = estimates$summarised[variable == "R"][type != "forecast"][, type := NULL],
                                    gt_mean = estimates$summarised[variable == "gt_mean"]$mean,
                                    gt_sd = estimates$summarised[variable == "gt_sd"]$mean,
                                    gt_max = generation_time$max,
                                    forecast_model = forecast_model,
                                    ensemble_type = ensemble_type,
                                    horizon = horizon,
                                    samples = samples)
  }
  # Report cases ------------------------------------------------------------
  if (!missing(forecast_model) & !is.null(target_folder)) {
    saveRDS(forecast$samples, paste0(target_folder, "/forecast_samples.rds"))
    saveRDS(forecast$summarised, paste0(target_folder, "/summarised_forecast.rds"))
  }
  # Report forcasts ---------------------------------------------------------

  if (missing(forecast_model)) {
    estimated_reported_cases <- list()
    estimated_reported_cases$samples <- estimates$samples[variable == "reported_cases"][,
      .(date, sample, cases = value, type = "gp_rt")]
    estimated_reported_cases$summarised <- estimates$summarised[variable == "reported_cases"][,
      type := "gp_rt"][, variable := NULL][, strat := NULL]
  }else {
    report_cases_with_forecast <- function(model) {
      reported_cases <- report_cases(case_estimates = estimates$samples[variable == "infections"][type != "forecast"][,
        .(date, sample, cases = value)],
                                     case_forecast = forecast$samples[type == "case" &
                                                                        forecast_type == model][,
                                       .(date, sample, cases = value)],
                                     delays = delays,
                                     type = "sample")
      return(reported_cases)
    }

    reported_cases_rt <- report_cases_with_forecast(model = "rt")
    reported_cases_cases <- report_cases_with_forecast(model = "case")
    reported_cases_ensemble <- report_cases_with_forecast(model = "ensemble")

    estimated_reported_cases <- list()

    estimated_reported_cases$samples <- data.table::rbindlist(list(
      reported_cases_rt$samples[, type := "rt"],
      reported_cases_cases$samples[, type := "case"],
      reported_cases_ensemble$samples[, type := "ensemble"],
      estimates$samples[variable == "reported_cases"][,
        .(date, sample, cases = value, type = "gp_rt")]
    ), use.names = TRUE)

    estimated_reported_cases$summarised <- data.table::rbindlist(list(
      reported_cases_rt$summarised[, type := "rt"],
      reported_cases_cases$summarised[, type := "case"],
      reported_cases_ensemble$summarised[, type := "ensemble"],
      estimates$summarised[variable == "reported_cases"][, type := "gp_rt"][,
        variable := NULL][, strat := NULL]
    ), use.names = TRUE)
  }

  if (!is.null(target_folder)) {
    saveRDS(estimated_reported_cases$samples, paste0(target_folder, "/estimated_reported_cases_samples.rds"))
    saveRDS(estimated_reported_cases$summarised, paste0(target_folder, "/summarised_estimated_reported_cases.rds"))
  }

  # # Report estimates --------------------------------------------------------

  summary <- report_summary(
    summarised_estimates = estimates$summarised[!is.na(date)][type != "forecast"][date == max(date)],
    rt_samples = estimates$samples[variable == "R"][type != "forecast"][date == max(date), .(sample, value)])


  if (!is.null(target_folder)) {
    saveRDS(summary, paste0(target_folder, "/summary.rds"))
  }

  #  # Plot --------------------------------------------------------------------

  plots <- report_plots(summarised_estimates = estimates$summarised,
                        reported = reported_cases, target_folder = target_folder)

  # Copy all results to latest folder ---------------------------------------
  if (!is.null(target_folder)) {
    ## Save all results to a latest folder as well
    suppressWarnings(
      if (dir.exists(latest_folder)) {
        unlink(latest_folder)
      })

    suppressWarnings(
      dir.create(latest_folder)
    )

    suppressWarnings(
      file.copy(file.path(target_folder, "."),
                latest_folder, recursive = TRUE)
    )
  }

  if (return_estimates) {
    out <- list()
    out$estimates <- estimates

    if (!missing(forecast_model)) {
      out$forecast <- forecast
    }

    out$estimated_reported_cases <- estimated_reported_cases
    out$summary <- summary
    out$plots <- plots
    return(out)
  }else {
    return(invisible(NULL))
  }
}


#' Real-time Rt Estimation, Forecasting and Reporting by Region
#'
#' @description Estimates Rt by region. See the documentation for `epinow` for further information.
#' @param reported_cases A data frame of confirmed cases (confirm) by date (date), and region (`region`).
#' @param non_zero_points Numeric, the minimum number of time points with non-zero cases in a region required for
#' that region to be evaluated. Defaults to 2.
#' @param summary Logical, should summary measures be calculated.
#' @param all_regions_summary Logical, defaults to `TRUE`. Should summary plots for all regions be returned
#' rather than just regions of interest.
#' @param return_timings Logical, defaults to FALSE. If not returning estimates can be used to request timing data is returned.
#' @param ... Pass additional arguments to `epinow`
#' @inheritParams epinow
#' @inheritParams regional_summary
#' @return A list of output stratified at the top level into regional output and across region output summary output
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom data.table as.data.table setDT copy setorder
#' @importFrom purrr safely map compact keep
#' @importFrom futile.logger flog.info flog.warn flog.trace
#' @importFrom R.utils withTimeout
#' @importFrom rlang cnd_muffle
#' @examples
#'  \donttest{
#' ## Construct example distributions
#' generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#' incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#'                    
#' reporting_delay <- list(mean = log(10), mean_sd = log(2),
#'                         sd = log(2), sd_sd = log(1.1), max = 30)
#'                         
#' ## Uses example case vector
#' cases <- EpiNow2::example_confirmed[1:40]
#' 
#' cases <- data.table::rbindlist(list(
#'   data.table::copy(cases)[, region := "testland"],
#'   cases[, region := "realland"]))
#'   
#' ## Run basic nowcasting pipeline
#' ## Here we reduce the accuracy of the GP approximation in order to reduce runtime
#' out <- regional_epinow(reported_cases = cases,  generation_time = generation_time,
#'                        delays = list(incubation_period, reporting_delay),
#'                        stan_args = list(warmup = 200, cores = ifelse(interactive(), 4, 1)))
#'}
regional_epinow <- function(reported_cases, target_folder, target_date,
                            non_zero_points = 2, summary = TRUE, summary_dir,
                            region_scale = "Region", all_regions_summary = TRUE,
                            return_estimates = TRUE, max_plot = 10,
                            return_timings = FALSE, ...) {

  ## Set input to data.table

  if (missing(target_date)) {
    target_date <- as.character(max(reported_cases$date))
  }

  if (missing(target_folder)) {
    target_folder <- NULL
  }

  futile.logger::flog.info("Reporting estimates using data up to: %s", target_date)
  
  ## Clean regions
  reported_cases <- clean_regions(reported_cases, non_zero_points)
  regions <- unique(reported_cases$region)

  ## Function to run the pipeline in a region
  safe_run_region <- purrr::safely(run_region)

  ## Run regions (make parallel using future::plan)
  futile.logger::flog.trace("calling future apply to process each region through the run_region function")
  regional_out <- future.apply::future_lapply(regions, safe_run_region,
                                              reported_cases = reported_cases,
                                              target_folder = target_folder, 
                                              target_date = target_date,
                                              return_estimates = return_estimates,
                                              ...,
                                              future.scheduling = Inf)

  out <- process_regions(regional_out, regions)
  regional_out <- out$all
  sucessful_regional_out <- out$successful
  
  # only attempt the summary if there are at least some results
  if (summary && length(sucessful_regional_out) > 0) {
    if (missing(summary_dir)) {
      summary_dir <- NULL
    }
    safe_summary <- purrr::safely(regional_summary)

    futile.logger::flog.info("Producing summary")

    summary_out <- safe_summary(regional_output = sucessful_regional_out,
                                summary_dir = summary_dir,
                                reported_cases = reported_cases,
                                region_scale = region_scale,
                                all_regions = all_regions_summary,
                                max_plot = max_plot)

    if (!is.null(summary_out[[2]])) {
      futile.logger::flog.info("Errors caught whilst generating summary statistics: ")
      futile.logger::flog.info(toString(summary_out[[2]]))
    }

    summary_out <- summary_out[[1]]
  }

  if (return_estimates) {
    out <- list()
    out$regional <- regional_out

    if (summary) {
      out$summary <- summary_out
    }

    return(out)
  }else if (return_timings) {
    out <- purrr::map(regional_out, ~.$timing)
    return(out)
  }else {
    return(invisible(NULL))
  }
}

#' Clean Regions
#'
#' @inheritParams regional_epinow
#' @importFrom data.table copy setDT
#' @importFrom futile.logger flog.info
#' @return A dataframe of cleaned regional data
clean_regions <- function(reported_cases, non_zero_points) {
  reported_cases <- data.table::setDT(reported_cases)
  ## Check for regions more than required time points with cases
  eval_regions <- data.table::copy(reported_cases)[, .(confirm = confirm > 0), by = c("region", "date")][,
                                                     .(confirm = sum(confirm, na.rm = TRUE)), by = "region"][confirm >= non_zero_points]$region
  
  eval_regions <- unique(eval_regions)
  orig_regions <- length(unique(reported_cases$region))
  if (length(eval_regions) > 30){
    futile.logger::flog.info("Producing estimates for: %s regions",
                             length(eval_regions))
    futile.logger::flog.info("Regions excluded: %s regions",
                             length(orig_regions))
  }else{
    futile.logger::flog.info("Producing estimates for: %s",
                             paste(eval_regions, collapse = ", "))
    futile.logger::flog.info("Regions excluded: %s",
                             paste(orig_regions, collapse = ", "))
  }

  
  ## Exclude zero regions
  reported_cases <- reported_cases[!is.na(region)][region %in% eval_regions]
  
  return(reported_cases)
}



#' Run epinow with Regional Processing Code
#' @param target_region Character string indicating the region being evaluated
#' @inheritParams regional_epinow
#' @importFrom data.table setDTthreads
#' @importFrom futile.logger flog.trace flog.warn
#' @return A list of processed output as produced by `process_region`
run_region <- function(target_region,
                       reported_cases,
                       target_folder,
                       target_date,
                       return_estimates,
                       ...) {
  futile.logger::flog.info("Initialising estimates for: %s", target_region)
  
  data.table::setDTthreads(threads = 1)
  
  if (!is.null(target_folder)) {
    target_folder <- file.path(target_folder, target_region)
  }
  
  futile.logger::flog.trace("filtering data for target region %s", target_region)
  regional_cases <- reported_cases[region %in% target_region][, region := NULL]
  
  futile.logger::flog.trace("calling epinow2::epinow to process data for %s", target_region)
  timing <- system.time(
    out <- tryCatch(
      withCallingHandlers(EpiNow2::epinow(
        reported_cases = regional_cases,
        target_folder = target_folder,
        target_date = target_date,
        return_estimates = TRUE,
        ...),
        warning = function(w) {
          futile.logger::flog.warn("%s: %s - %s", target_region, w$message, toString(w$call))
          rlang::cnd_muffle(w)
        }
      ),
      TimeoutException = function(ex) {
        futile.logger::flog.warn("region %s timed out", target_region)
        return(list("timing" = Inf))
      }
    )
  )
  out <- process_region(out, return_estimates, target_region, timing)
  return(out)
}

#' Process regional estimate
#'
#' @param out List of output returned by `epinow`
#' @param timing Output from `Sys.time` 
#' @inheritParams regional_epinow
#' @inheritParams run_region
#' @importFrom futile.logger flog.info
#' @return A list of processed output
process_region <- function(out, return_estimates, target_region, timing) {
  
  if (exists("estimates", out) & !return_estimates) {
    out$estimates$samples <- NULL
  }
  if (exists("forecast", out) & !return_estimates) {
    out$forecast$samples <- NULL
  }
  if (exists("estimated_reported_cases", out) & !return_estimates) {
    out$estimated_reported_cases$samples <- NULL
  }
  if (exists("plots", out) & !return_estimates) {
    out$estimated_reported_cases$plots <- NULL
  }
  if (!exists("timing", out)) { # only exists if it failed and is Inf
    out$timing = timing['elapsed']
  }
  if (exists("summary", out)) { # if it failed a warning would have been output above
    futile.logger::flog.info("Completed estimates for: %s", target_region)
  }
  
  return(out)
}


#' Process all Region Estimates
#'
#' @param regional_out A list of output from multiple runs of `regional_epinow`
#' @param regions A character vector identifying the regions that have been run
#' @importFrom purrr keep map compact
#' @importFrom futile.logger flog.trace flog.info
#' @return A list of all regional estimates and successful regional estimates
process_regions <- function(regional_out, regions) {
  
  # names on regional_out
  names(regional_out) <- regions
  problems <- purrr::keep(regional_out, ~!is.null(.$error))
  
  futile.logger::flog.trace("%s runtime errors caught", length(problems))
  for (location in names(problems)) {
    # output timeout / error
    futile.logger::flog.info("Runtime error in %s : %s - %s", location,
                             problems[[location]]$error$message, 
                             toString(problems[[location]]$error$call))
  }
  
  regional_out <- purrr::map(regional_out, ~.$result)
  sucessful_regional_out <- purrr::keep(purrr::compact(regional_out), ~ is.finite(.$timing))
  return(list(all = regional_out, successful = sucessful_regional_out))
}
