#' Real-time Rt Estimation, Forecasting and Reporting
#'
#' @description This function wraps the functionality of `estimate_infections` and `forecast_infections` in order
#' to estimate Rt and cases by date of infection, forecast into these infections into the future. It also contains 
#' additional functionality to convert forecasts to date of report and produce summary output useful for reporting 
#' results and interpreting them.
#' @param output A character vector of optional output to return. Supported options are samples ("samples"), 
#' plots ("plots"), and the stan fit ("fit"). The default is to return samples and plots alongside summarised estimates
#' and summary statistics. This argument uses partial matching so for example passing "sam" will lead to samples
#' being reported.
#' @param return_output Logical, defaults to TRUE. Should output be returned. This must either be true or a
#' `target_folder` must be specified in order to enable output saving to disk.
#' @param forecast_args A list of arguments to pass to `forecast_infections`. Unless at a minumum a `forecast_model` is passed 
#' tin his list then `forecast_infections` will be bypassed. 
#' @param ... Additional arguments passed to `estimate_infections`. See that functions documentation for options.
#' @return A list of output from estimate_infections, forecast_infections,  report_cases, and report_summary.
#' @export
#' @inheritParams setup_target_folder
#' @inheritParams estimate_infections
#' @inheritParams forecast_infections
#' @importFrom data.table setDT
#' @importFrom lubridate days
#' @importFrom futile.logger flog.fatal
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
#' ## Report Rt along with forecasts
#' out <- epinow(reported_cases = reported_cases, generation_time = generation_time,
#'               delays = list(incubation_period, reporting_delay), 
#'               stan_args = list(cores = ifelse(interactive(), 4, 1)))
#' out
#' 
#' # optional forecasting
#' if(requireNamespace("EpiSoon")){
#'    if(requireNamespace("forecastHybrid")){
#'    # report Rt along with forecasts
#'    out <- epinow(reported_cases = reported_cases, samples = 200, verbose = TRUE,
#'                  generation_time = generation_time, 
#'                  delays = list(incubation_period, reporting_delay),
#'                  forecast_args = list(
#'                      forecast_model = function(y, ...){
#'                      EpiSoon::forecastHybrid_model(
#'                           y = y[max(1, length(y) - 21):length(y)],
#'                           model_params = list(models = "aefz", weights = "equal"),
#'                           forecast_params = list(PI.combination = "mean"), ...)}
#'                           ),
#'                  stan_args = list(warmup = 200, cores = ifelse(interactive(), 4, 1)))
#'     out
#'    }
#' }
#'
#' }
#'
epinow <- function(reported_cases, samples = 1000, horizon = 7, 
                   generation_time, delays = list(),
                   return_output = TRUE, output = c("samples", "plots"), 
                   target_folder = NULL, target_date, 
                   forecast_args = NULL, verbose = FALSE,
                   ...) {

  if (!return_output & is.null(target_folder)) {
    futile.logger::flog.fatal("Either return output or save to a target folder",
                              name = "EpiNow2.epinow")
    stop("Either return output or save to a target folder")
  }
  
  # Check verbose settings and set logger to match---------------------------
  if (verbose) {
    futile.logger::flog.threshold(futile.logger::DEBUG,
                                  name = "EpiNow2.epinow")
  }
  
 # Setup input -------------------------------------------------------------
 output <- match_output_arguments(output, supported_args = c("plots", "samples", "fit"),
                                  logger = "EpiNow2.epinow",
                                  level = "debug")
  
 # convert input to DT -----------------------------------------------------
 reported_cases <- setup_dt(reported_cases)
  
 # target data -------------------------------------------------------------
 if (missing(target_date)) {
   target_date <- max(reported_cases$date)
 }
 
 # set up folders ----------------------------------------------------------
 target_folders <- setup_target_folder(target_folder, target_date)
 target_folder <- target_folders$date
 latest_folder <- target_folders$latest
 
 # save input data ---------------------------------------------------------
 save_input(reported_cases, target_folder)
 
 # make sure the horizon is as specified from the target date --------------
 horizon <- update_horizon(horizon, target_date, reported_cases)

 # estimate infections and Reproduction no ---------------------------------
 estimates <- estimate_infections(reported_cases = reported_cases, 
                                  generation_time = generation_time,
                                  delays = delays,
                                  samples = samples,
                                  horizon = horizon,
                                  estimate_rt = TRUE,
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
                              samples = samples),
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
                                                      samples = output["samples"])
 
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
  # copy all results to latest folder ---------------------------------------
  copy_results_to_latest(target_folder, latest_folder)

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


