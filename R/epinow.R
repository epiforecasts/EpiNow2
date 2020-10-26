#' Real-time Rt Estimation, Forecasting and Reporting
#'
#' @description This function wraps the functionality of `estimate_infections` and `forecast_infections` in order
#' to estimate Rt and cases by date of infection, forecast into these infections into the future. It also contains 
#' additional functionality to convert forecasts to date of report and produce summary output useful for reporting 
#' results and interpreting them.
#' @param output A character vector of optional output to return. Supported options are samples ("samples"), 
#' plots ("plots"), the run time ("timing"), copying the dated folder into a latest folder (if `target_folder` is not null
#'  - set using "latest"), and the stan fit ("fit"). The default is to return samples and plots alongside summarised estimates
#' and summary statistics. This argument uses partial matching so for example passing "sam" will lead to samples
#' being reported.
#' @param return_output Logical, defaults to FALSE. Should output be returned, this automatically updates to TRUE 
#' if no directory for saving is specified. 
#' @param forecast_args A list of arguments to pass to `forecast_infections`. Unless at a minimum a `forecast_model` is passed 
#' tin his list then `forecast_infections` will be bypassed. 
#' @param id A character string used to assign logging information on error. Used by `regional_epinow`
#' to assign `epinow` errors to regions.
#' @param ... Additional arguments passed to `estimate_infections`. See that functions documentation for options.
#' @return A list of output from estimate_infections, forecast_infections,  report_cases, and report_summary.
#' @export
#' @inheritParams setup_target_folder
#' @inheritParams estimate_infections
#' @inheritParams forecast_infections
#' @inheritParams setup_default_logging
#' @importFrom data.table setDT
#' @importFrom lubridate days
#' @importFrom futile.logger flog.fatal flog.warn flog.error flog.debug ftry
#' @importFrom rlang trace_back
#' @examples
#' \donttest{
#' # construct example distributions
#' generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#' incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#' reporting_delay <- bootstrapped_dist_fit(rlnorm(100, log(6), 1), max_value = 30)
#' 
#' # example case data
#' reported_cases <- EpiNow2::example_confirmed[1:40] 
#' 
#' # estimate Rt and nowcast/forecast cases by date of infection
#' out <- epinow(reported_cases = reported_cases, generation_time = generation_time,
#'               delays = list(incubation_period, reporting_delay),
#'               stan_args = list(cores = ifelse(interactive(), 4, 1),
#'               control = list(adapt_delta = 0.95)), verbose = interactive())
#' out
#' 
#' # optional forecasting using EpiSoon plug-in
#' if(requireNamespace("EpiSoon")){
#'    if(requireNamespace("forecastHybrid")){
#'    # report Rt along with forecasts
#'    out <- epinow(reported_cases = reported_cases, samples = 200,
#'                  generation_time = generation_time, 
#'                  delays = list(incubation_period, reporting_delay),
#'                  forecast_args = list(
#'                      forecast_model = function(y, ...){
#'                      EpiSoon::forecastHybrid_model(
#'                           y = y[max(1, length(y) - 21):length(y)],
#'                           model_params = list(models = "aefz", weights = "equal"),
#'                           forecast_params = list(PI.combination = "mean"), ...)}
#'                           ),
#'                  stan_args = list(warmup = 200, cores = ifelse(interactive(), 4, 1)),
#'                  verbose = interactive())
#'     out
#'    }
#' }
#' }
#'
epinow <- function(reported_cases, samples = 1000, horizon = 7, 
                   generation_time, delays = list(),
                   CrIs = c(0.2, 0.5, 0.9),
                   return_output = FALSE, output = c("samples", "plots", "latest"), 
                   target_folder = NULL, target_date, 
                   forecast_args = NULL, logs = tempdir(),
                   id = "epinow", verbose = FALSE,
                   ...) {

  if (is.null(target_folder)) {
    return_output <- TRUE
  }
  
  if (is.null(CrIs) | length(CrIs) == 0 | !is.numeric(CrIs)) {
    futile.logger::flog.fatal("At least one credible interval must be specified",
                              name = "EpiNow2.epinow")
    stop("At least one credible interval must be specified")
  }
  
  # check verbose settings and set logger to match---------------------------
  if (verbose) {
    futile.logger::flog.threshold(futile.logger::DEBUG,
                                  name = "EpiNow2.epinow")
  }
  # target data -------------------------------------------------------------
  if (missing(target_date)) {
    target_date <- max(reported_cases$date, na.rm = TRUE)
  }

  # setup logging -----------------------------------------------------------
  setup_default_logging(logs = logs, 
                        target_date = target_date,
                        mirror_epinow = TRUE,
                        mirror_epinow_fit = verbose)
  
  # setup input -------------------------------------------------------------
  output <- match_output_arguments(output, 
                                   supported_args = c("plots", "samples", 
                                                      "fit", "timing", 
                                                      "latest"),
                                   logger = "EpiNow2.epinow",
                                   level = "debug")
  
  # set up folders ----------------------------------------------------------
  target_folders <- setup_target_folder(target_folder, target_date)
  target_folder <- target_folders$date
  latest_folder <- target_folders$latest
  
  # specify internal functions
  epinow_internal <- function() {
    # check verbose settings and set logger to match---------------------------
    if (verbose) {
      futile.logger::flog.threshold(futile.logger::DEBUG,
                                    name = "EpiNow2.epinow")
    }
    
    # convert input to DT -----------------------------------------------------
    reported_cases <- setup_dt(reported_cases)
    
    # save input data ---------------------------------------------------------
    save_input(reported_cases, target_folder)
    
    # make sure the horizon is as specified from the target date --------------
    horizon <- update_horizon(horizon, target_date, reported_cases)
    
    # estimate infections and Reproduction no ---------------------------------
    estimates <- estimate_infections(reported_cases = reported_cases, 
                                     generation_time = generation_time,
                                     CrIs = CrIs,
                                     delays = delays,
                                     samples = samples,
                                     horizon = horizon,
                                     return_fit = output["fit"],
                                     verbose = verbose,
                                     ...)
    
    save_estimate_infections(estimates, target_folder, 
                             samples = output["samples"],
                             return_fit = output["fit"])
    
    # forecast infections and reproduction number -----------------------------
    if (!is.null(forecast_args)) {
      forecast <- do.call(forecast_infections, 
                          c(list(infections = estimates$summarised[variable == "infections"][type != "forecast"][, type := NULL],
                                 rts = estimates$summarised[variable == "R"][type != "forecast"][, type := NULL],
                                 gt_mean = estimates$summarised[variable == "gt_mean"]$mean,
                                 gt_sd = estimates$summarised[variable == "gt_sd"]$mean,
                                 gt_max = generation_time$max,
                                 horizon = horizon,
                                 samples = samples,
                                 CrIs = CrIs),
                            forecast_args))
      
      save_forecast_infections(forecast, target_folder, samples = output["samples"])
    }else{
      forecast <- NULL
    }
    # report forecasts ---------------------------------------------------------
    estimated_reported_cases <- estimates_by_report_date(estimates,
                                                         forecast, 
                                                         delays = delays,
                                                         target_folder = target_folder,
                                                         samples = output["samples"],
                                                         CrIs = CrIs)
    
    # report estimates --------------------------------------------------------
    summary <- report_summary(
      summarised_estimates = estimates$summarised[!is.na(date)][type != "forecast"][date == max(date)],
      rt_samples = estimates$samples[variable == "R"][type != "forecast"][date == max(date), .(sample, value)],
      target_folder = target_folder)
    
    # plot --------------------------------------------------------------------
    if (output["plots"]) {
      plots <- report_plots(summarised_estimates = estimates$summarised,
                            reported = reported_cases, 
                            target_folder = target_folder)
    }else{
      plots <- NULL
    }
    
    if (return_output) {
      out <- construct_output(estimates, 
                              forecast,
                              estimated_reported_cases,
                              plots = plots,
                              summary,
                              samples = output["samples"])
      return(out)
    }else{
      return(invisible(NULL))
    }
  }
  
  # start processing with system timing and error catching
  timing <- system.time({
    out <-  futile.logger::ftry(
      withCallingHandlers(epinow_internal(),
        warning = function(w) {
          futile.logger::flog.warn(capture.output(rlang::trace_back()),
                                    name = "EpiNow2.epinow")
        },
        error = function(e) {
          futile.logger::flog.error(capture.output(rlang::trace_back()),
                                    name = "EpiNow2.epinow")
        }))
  })
  
  # log timing if specified
  if (output["timing"]) {
    if (return_output) {
      out$timing <- timing['elapsed']
    }
    if (!is.null(target_folder)) {
      saveRDS(timing['elapsed'], paste0(target_folder, "/runtime.rds"))
    }
  }
  
  # copy all results to latest folder
  if (output["latest"]) {
    copy_results_to_latest(target_folder, latest_folder)
  }

  # return output
  if (return_output) {
    return(out)
  }else{
    return(invisible(NULL))
  }
}


