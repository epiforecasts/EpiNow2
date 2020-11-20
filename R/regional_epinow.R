#' Real-time Rt Estimation, Forecasting and Reporting by Region
#'
#' @description \lifecycle{maturing}
#' Efficiently runs `epinow()` across multiple regions in an efficient manner and conducts basic data checks and 
#' cleaning such as removing regions with fewer than `non_zero_points` as these are unlikely to produce reasonable 
#' results whilst consuming significant resources. See the documentation for `epinow` for further information.
#' 
#' By default all arguments supporting input from `_opts` functions are shared across regions (including delays, 
#' truncation, Rt settings, stan settings, and gaussian process settings). Region specific settings are supported 
#' by passing a named list of `_opts` calls (with an entry per region) to the relevant argument. A helper function
#' (`opts_list`) is available to facilitate building this list.  
#' 
#' Regions can be estimated in parallel using the `{future}` package (see `setup_future`). The progress of producing
#' estimates across multiple regions is tracked using the `progressr` package. Modify this behaviour using
#' progressr::handlers and enable it in batch by setting `R_PROGRESSR_ENABLE=TRUE` as an environment variable. 
#' @param reported_cases A data frame of confirmed cases (confirm) by date (date), and region (`region`).
#' @param non_zero_points Numeric, the minimum number of time points with non-zero cases in a region required for
#' that region to be evaluated. Defaults to 7.
#' @param output A character vector of optional output to return. Supported options are the individual regional estimates
#' ("regions"),  samples ("samples"), plots ("plots"), copying the individual region dated folder into 
#' a latest folder (if `target_folder` is not null, set using "latest"), the stan fit of the underlying model ("fit"), and an 
#' overall summary across regions ("summary"). The default is to return samples and plots alongside summarised estimates and 
#' summary statistics. If `target_folder` is not NULL then the default is also to copy all results into a latest folder.
#' @param summary_args A list of arguments passed to `regional_summary`. See the `regional_summary` documentation for details.
#' @param verbose Logical defaults to FALSE. Outputs verbose progress messages to the console from `epinow`.
#' @param ... Pass additional arguments to `epinow`. See the documentation for `epinow` for details.
#' @inheritParams epinow
#' @inheritParams regional_summary
#' @return A list of output stratified at the top level into regional output and across region output summary output
#' @export
#' @seealso epinow estimate_infections forecast_infections setup_future regional_summary
#' @importFrom future.apply future_lapply
#' @importFrom data.table as.data.table setDT copy setorder
#' @importFrom purrr safely map compact keep
#' @importFrom futile.logger flog.info flog.warn flog.trace
#' @importFrom R.utils withTimeout
#' @importFrom rlang cnd_muffle
#' @importFrom progressr with_progress progressor
#' @examples
#'  \donttest{
#' #set number of cores to use
#' options(mc.cores = ifelse(interactive(), 4, 1))
#' 
#' # construct example distributions
#' generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#' incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#' reporting_delay <- list(mean = convert_to_logmean(3,1),
#'                         mean_sd = 0.1,
#'                         sd = convert_to_logsd(3,1),
#'                         sd_sd = 0.1, max = 15)
#'                         
#' # uses example case vector
#' cases <- EpiNow2::example_confirmed[1:60]
#' cases <- data.table::rbindlist(list(
#'   data.table::copy(cases)[, region := "testland"],
#'   cases[, region := "realland"]))
#'                 
#' # run epinow across multiple regions and generate summaries
#' # samples and warmup have been reduced for this example
#' def <- regional_epinow(reported_cases = cases, 
#'                        generation_time = generation_time,
#'                        delays = delay_opts(incubation_period, reporting_delay),
#'                        rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
#'                        stan = stan_opts(samples = 100, warmup = 200),
#'                        verbose = interactive())
#'                        
#' # apply a different rt method per region
#' # (here a gaussian process and a weekly random walk)
#' gp <- opts_list(gp_opts(), cases)
#' gp <- update_list(gp, list(realland = NULL))
#' rt <- opts_list(rt_opts(), cases, realland = rt_opts(rw = 7))
#' region_rt <- regional_epinow(reported_cases = cases, 
#'                              generation_time = generation_time,
#'                              delays = delay_opts(incubation_period, reporting_delay),
#'                              rt = rt, gp = gp,
#'                              stan = stan_opts(samples = 100, warmup = 200),
#'                              verbose = interactive())
#'}
regional_epinow <- function(reported_cases, 
                            generation_time, 
                            delays = delay_opts(),
                            truncation = trunc_opts(),
                            rt = rt_opts(),
                            backcalc = backcalc_opts(),
                            gp = gp_opts(),
                            obs = obs_opts(),
                            stan = stan_opts(),
                            horizon = 7,
                            CrIs = c(0.2, 0.5, 0.9),
                            target_folder = NULL, 
                            target_date,
                            non_zero_points = 2, 
                            output = c("regions", "summary", "samples", 
                                       "plots", "latest"),
                            return_output = FALSE,
                            summary_args = list(), 
                            verbose = FALSE,
                            logs = tempdir(), ...) {
  # supported output
  output <- match_output_arguments(output, 
                                   supported_args = c("plots", "samples", "fit",
                                                      "regions", "summary",
                                                      "timing", "latest"),
                                   logger = "EpiNow2")
  # make timing compulsory 
  output["timing"] <- TRUE
  if (missing(target_date)) {
    target_date <- as.character(max(reported_cases$date))
  }
  
  # setup logging -----------------------------------------------------------
  setup_default_logging(logs = logs, target_date = target_date,
                        mirror_epinow = verbose)
  
  futile.logger::flog.info("Reporting estimates using data up to: %s", target_date)
  if (is.null(target_folder)) {
    futile.logger::flog.info("No target directory specified so returning output")
    return_output <- TRUE
  }else{
    futile.logger::flog.info("Saving estimates to : %s", target_folder)
  } 
  
  # clean regions
  reported_cases <- clean_regions(reported_cases, non_zero_points)
  regions <- unique(reported_cases$region)
  
  # run regions (make parallel using future::plan)
  futile.logger::flog.trace("calling future apply to process each region through the run_region function")
  futile.logger::flog.info("Showing progress using progressr.")
  
  progressr::with_progress({
    progress_fn <- progressr::progressor(along = regions)
    regional_out <- future.apply::future_lapply(regions, run_region,
                                                generation_time = generation_time, 
                                                delays = delays,
                                                truncation = truncation,
                                                rt = rt,
                                                backcalc = backcalc,
                                                gp = gp,
                                                obs = obs,
                                                stan = stan,
                                                horizon = horizon,
                                                CrIs = CrIs,
                                                reported_cases = reported_cases,
                                                target_folder = target_folder, 
                                                target_date = target_date,
                                                output = output,
                                                return_output = output["summary"] | return_output,
                                                complete_logger = ifelse(length(regions) > 10, 
                                                                         "EpiNow2.epinow",
                                                                         "EpiNow2"),
                                                progress_fn = progress_fn,
                                                verbose = verbose,
                                                ...,
                                                future.scheduling = Inf,
                                                future.seed = TRUE)
  })
  
  out <- process_regions(regional_out, regions)
  regional_out <- out$all
  sucessful_regional_out <- out$successful
  
  if (return_output) {
    out <- list()
    if (output["regions"]) {
      out$regional <- regional_out
    }
  }
  
  # only attempt the summary if there are at least some results
  if (output["summary"] && length(sucessful_regional_out) > 0) {
    safe_summary <- purrr::safely(regional_summary)
    futile.logger::flog.info("Producing summary")
    summary_out <- do.call(safe_summary,
                           c(list(regional_output = sucessful_regional_out,
                                  reported_cases = reported_cases,
                                  return_output = return_output),
                             summary_args))
    
    if (!is.null(summary_out[[2]])) {
      futile.logger::flog.info("Errors caught whilst generating summary statistics: ")
      futile.logger::flog.info(toString(summary_out[[2]]))
    }
    summary_out <- summary_out[[1]]
    if (return_output) {
      out$summary <- summary_out
    }
  }
  
  if (output["timing"]) {
    safe_runtimes <- purrr::safely(regional_runtimes)
    timings <- safe_runtimes(regional_out,
                             target_folder = target_folder,
                             target_date = target_date,
                             return_output = return_output)[[1]]
    if (return_output) {
      out$timings <- timings
    }
  }
  
  if (return_output) {
    return(out)
  }else{
    return(invisible(NULL))
  }
}

#' Clean Regions
#'
#' @description \lifecycle{stable}
#' Removes regions with insufficient time points, and provides logging information on the 
#' input.
#' @seealso regional_epinow
#' @inheritParams regional_epinow
#' @importFrom data.table copy setDT
#' @importFrom futile.logger flog.info
#' @return A dataframe of cleaned regional data
clean_regions <- function(reported_cases, non_zero_points) {
  reported_cases <- data.table::setDT(reported_cases)
  # check for regions more than required time points with cases
  eval_regions <- data.table::copy(reported_cases)[, .(confirm = confirm > 0), by = c("region", "date")][,
                                                     .(confirm = sum(confirm, na.rm = TRUE)), by = "region"][confirm >= non_zero_points]$region
  eval_regions <- unique(eval_regions)
  orig_regions <- setdiff(unique(reported_cases$region), eval_regions)
  if (length(eval_regions) > 30){
    futile.logger::flog.info("Producing estimates for: %s regions",
                             length(eval_regions))
    message <- ifelse(length(orig_regions) == 0, 0, 
                      length(orig_regions))
    futile.logger::flog.info("Regions excluded: %s regions",
                             message)
  }else{
    futile.logger::flog.info("Producing estimates for: %s",
                             paste(eval_regions, collapse = ", "))
    message <- ifelse(length(orig_regions) == 0, "none", 
                      paste(orig_regions, collapse = ", "))
    futile.logger::flog.info("Regions excluded: %s",
                             message)
  }
  # exclude zero regions
  reported_cases <- reported_cases[!is.na(region)][region %in% eval_regions]
  return(reported_cases)
}

#' Run epinow with Regional Processing Code
#' 
#' @description \lifecycle{maturing}
#' Internal function that handles calling `epinow`. Future work will extend this 
#' function to better handle `stan` logs and allow the user to modify settings between
#' regions.
#' @param target_region Character string indicating the region being evaluated
#' @param progress_fn Function as returned by `progressr::progressor`. Allows the use of a 
#' progress bar. 
#' @param complete_logger Character string indicating the logger to output
#' the completion of estimation to.
#' @inheritParams regional_epinow
#' @importFrom data.table setDTthreads
#' @importFrom futile.logger flog.trace flog.warn
#' @importFrom purrr quietly 
#' @seealso regional_epinow
#' @return A list of processed output as produced by `process_region`
run_region <- function(target_region,
                       generation_time, 
                       delays,
                       truncation,
                       rt,
                       backcalc,
                       gp,
                       obs,
                       stan,
                       horizon,
                       CrIs,
                       reported_cases,
                       target_folder,
                       target_date,
                       return_output,
                       output,
                       complete_logger,
                       verbose,
                       progress_fn,
                       ...) {
  futile.logger::flog.info("Initialising estimates for: %s", target_region, 
                           name = "EpiNow2.epinow")
  data.table::setDTthreads(threads = 1)
  
  if (!is.null(target_folder)) {
    target_folder <- file.path(target_folder, target_region)
  }
  futile.logger::flog.trace("filtering data for target region %s", target_region,
                            name = "EpiNow2.epinow")
  regional_cases <- reported_cases[region %in% target_region][, region := NULL]
  
  futile.logger::flog.trace("calling epinow2::epinow to process data for %s", target_region,
                            name = "EpiNow2.epinow")

  out <- epinow(
    generation_time = generation_time, 
    delays = filter_opts(delays, target_region),
    truncation = filter_opts(truncation, target_region),
    rt = filter_opts(rt, target_region),
    backcalc = filter_opts(backcalc, target_region),
    gp = filter_opts(gp, target_region),
    obs = filter_opts(obs, target_region),
    stan = filter_opts(stan, target_region),
    horizon = horizon,
    CrIs = CrIs,
    reported_cases = regional_cases,
    target_folder = target_folder,
    target_date = target_date,
    return_output = TRUE,
    output = names(output[output]),
    logs = NULL,
    verbose = verbose,
    id = target_region,
    ...)
  
  out <- process_region(out, target_region, timing,
                        return_output,
                        return_timing = output["timing"],
                        complete_logger)
  
  if (!missing(progress_fn)) {
    progress_fn(sprintf("Region: %s", target_region))
  }
  return(out)
}

#' Process regional estimate
#'
#' @description \lifecycle{maturing}
#' Internal function that removes output that is not required, and returns logging 
#' information. 
#' @param out List of output returned by `epinow`
#' @param timing Output from `Sys.time` 
#' @param return_timing Logical, should runtime be returned
#' @inheritParams regional_epinow
#' @inheritParams run_region
#' @seealso regional_epinow
#' @importFrom futile.logger flog.info
#' @return A list of processed output
process_region <- function(out, target_region, timing, 
                           return_output = TRUE, return_timing = TRUE,
                           complete_logger = "EpiNow2.epinow") {
  
  if (!is.null(out[["estimates"]]) & !return_output) {
    out$estimates$samples <- NULL
  }
  if (!is.null(out[["forecast"]]) & !return_output) {
    out$forecast$samples <- NULL
  }
  if (!is.null(out[["estimated_reported_cases"]]) & !return_output) {
    out$estimated_reported_cases$samples <- NULL
  }
  if (!is.null(out[["plots"]]) & !return_output) {
    out$estimated_reported_cases$plots <- NULL
  }
  
  if (!is.null(out[["summary"]])) { # if it failed a warning would have been output above
    futile.logger::flog.info("Completed estimates for: %s", target_region, 
                             name = complete_logger)
  }
  return(out)
}

#' Process all Region Estimates
#'
#' @description \lifecycle{stable}
#' Internal function that processes the output from multiple `epinow` runs, adds 
#' summary logging information.
#' @param regional_out A list of output from multiple runs of `regional_epinow`
#' @param regions A character vector identifying the regions that have been run
#' @importFrom purrr keep map compact
#' @importFrom futile.logger flog.trace flog.info
#' @seealso regional_epinow epinow
#' @return A list of all regional estimates and successful regional estimates
process_regions <- function(regional_out, regions) {
  # names on regional_out
  names(regional_out) <- regions
  problems <- purrr::keep(regional_out, ~!is.null(.$error))
  futile.logger::flog.info("Completed regional estimates")
  futile.logger::flog.info("Regions with estimates: %s", (length(regions) - length(problems)))
  futile.logger::flog.info("Regions with runtime errors: %s", length(problems))
  for (location in names(problems)) {
    # output timeout / error
    futile.logger::flog.info("Runtime error in %s : %s", location,
                             problems[[location]]$error, 
                             name = "EpiNow2.epinow")
  } 
  sucessful_regional_out <- purrr::keep(purrr::compact(regional_out), ~ is.finite(.$timing))
  return(list(all = regional_out, successful = sucessful_regional_out))
}
