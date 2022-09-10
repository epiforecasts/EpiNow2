#' Real-time Rt Estimation, Forecasting and Reporting
#'
#' @description `r lifecycle::badge("maturing")`
#' This function wraps the functionality of `estimate_infections()` and `forecast_infections()` in order
#' to estimate Rt and cases by date of infection, forecast into these infections into the future. It also contains
#' additional functionality to convert forecasts to date of report and produce summary output useful for reporting
#' results and interpreting them. See [here](https://gist.github.com/seabbs/163d0f195892cde685c70473e1f5e867) for an
#' example of using `epinow` to estimate Rt for Covid-19 in a country from the ECDC data source.
#' @param output A character vector of optional output to return. Supported options are samples ("samples"),
#' plots ("plots"), the run time ("timing"), copying the dated folder into a latest folder (if `target_folder` is not null,
#' set using "latest"), and the stan fit ("fit"). The default is to return all options. This argument uses partial matching
#' so for example passing "sam" will lead to samples being reported.
#' @param return_output Logical, defaults to FALSE. Should output be returned, this automatically updates to TRUE
#' if no directory for saving is specified.
#' @param plot_args A list of optional arguments passed to `plot.epinow()`.
#' @return A list of output from estimate_infections, forecast_infections,  report_cases, and report_summary.
#' @export
#' @seealso estimate_infections simulate_infections forecast_infections regional_epinow
#' @inheritParams setup_target_folder
#' @inheritParams estimate_infections
#' @inheritParams setup_default_logging
#' @importFrom data.table setDT
#' @importFrom lubridate days
#' @importFrom futile.logger flog.fatal flog.warn flog.error flog.debug ftry
#' @importFrom rlang cnd_muffle
#' @examples
#' \donttest{
#' # set number of cores to use
#' options(mc.cores = ifelse(interactive(), 4, 1))
#' # construct example distributions
#' generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#' incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#' reporting_delay <- list(
#'   mean = convert_to_logmean(2, 1),
#'   mean_sd = 0.1,
#'   sd = convert_to_logsd(2, 1),
#'   sd_sd = 0.1,
#'   max = 10
#' )
#'
#' # example case data
#' reported_cases <- example_confirmed[1:40]
#'
#' # estimate Rt and nowcast/forecast cases by date of infection
#' out <- epinow(
#'   reported_cases = reported_cases, generation_time = generation_time,
#'   rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
#'   delays = delay_opts(incubation_period, reporting_delay)
#' )
#' # summary of the latest estimates
#' summary(out)
#' # plot estimates
#' plot(out)
#'
#' # summary of R estimates
#' summary(out, type = "parameters", params = "R")
#' }
epinow <- function(reported_cases,
                   generation_time = NULL,
                   delays = delay_opts(),
                   truncation = trunc_opts(),
                   rt = rt_opts(),
                   backcalc = backcalc_opts(),
                   gp = gp_opts(),
                   obs = obs_opts(),
                   stan = stan_opts(),
                   horizon = 7,
                   CrIs = c(0.2, 0.5, 0.9),
                   zero_threshold = 50,
                   return_output = FALSE,
                   output = c("samples", "plots", "latest", "fit", "timing"),
                   plot_args = list(),
                   target_folder = NULL, target_date,
                   logs = tempdir(), id = "epinow", verbose = interactive()) {
  if (is.null(target_folder)) {
    return_output <- TRUE
  }

  if (is.null(CrIs) | length(CrIs) == 0 | !is.numeric(CrIs)) {
    futile.logger::flog.fatal("At least one credible interval must be specified",
      name = "EpiNow2.epinow"
    )
    stop("At least one credible interval must be specified")
  }

  # check verbose settings and set logger to match---------------------------
  if (verbose) {
    futile.logger::flog.threshold(futile.logger::DEBUG,
      name = "EpiNow2.epinow"
    )
  }
  # target data -------------------------------------------------------------
  if (missing(target_date)) {
    target_date <- max(reported_cases$date, na.rm = TRUE)
  }

  # setup logging -----------------------------------------------------------
  setup_default_logging(
    logs = logs,
    target_date = target_date,
    mirror_epinow = TRUE
  )

  # setup input -------------------------------------------------------------
  output <- match_output_arguments(output,
    supported_args = c(
      "plots", "samples",
      "fit", "timing",
      "latest"
    ),
    logger = "EpiNow2.epinow",
    level = "debug"
  )

  # set up folders ----------------------------------------------------------
  target_folders <- setup_target_folder(target_folder, target_date)
  target_folder <- target_folders$date
  latest_folder <- target_folders$latest

  # specify internal functions
  epinow_internal <- function() {
    # check verbose settings and set logger to match---------------------------
    if (verbose) {
      futile.logger::flog.threshold(futile.logger::DEBUG,
        name = "EpiNow2.epinow"
      )
    }

    # convert input to DT -----------------------------------------------------
    reported_cases <- setup_dt(reported_cases)

    # save input data ---------------------------------------------------------
    save_input(reported_cases, target_folder)

    # make sure the horizon is as specified from the target date --------------
    horizon <- update_horizon(horizon, target_date, reported_cases)

    # estimate infections and Reproduction no ---------------------------------
    estimates <- estimate_infections(
      reported_cases = reported_cases,
      generation_time = generation_time,
      delays = delays,
      truncation = truncation,
      rt = rt,
      backcalc = backcalc,
      gp = gp,
      obs = obs,
      stan = stan,
      CrIs = CrIs,
      zero_threshold = zero_threshold,
      horizon = horizon,
      verbose = verbose,
      id = id
    )

    if (!output["fit"]) {
      estimates$fit <- NULL
      estimates$args <- NULL
    }

    save_estimate_infections(estimates, target_folder,
      samples = output["samples"],
      return_fit = output["fit"]
    )

    # report forecasts ---------------------------------------------------------
    estimated_reported_cases <- estimates_by_report_date(estimates,
      target_folder = target_folder,
      samples = output["samples"],
      CrIs = CrIs
    )

    # report estimates --------------------------------------------------------
    summary <- summary.estimate_infections(estimates,
      return_numeric = TRUE,
      target_folder = target_folder
    )

    # plot --------------------------------------------------------------------
    if (output["plots"]) {
      plots <- do.call(plot.estimate_infections, c(
        list(
          x = estimates,
          type = "all",
          target_folder = target_folder
        ),
        plot_args
        )
      )
    } else {
      plots <- NULL
    }

    if (return_output) {
      out <- construct_output(estimates,
        estimated_reported_cases,
        plots = plots,
        summary,
        samples = output["samples"]
      )
      return(out)
    } else {
      return(invisible(NULL))
    }
  }

  # start processing with system timing and error catching
  start_time <- Sys.time()
  out <- tryCatch(withCallingHandlers(
    epinow_internal(),
    warning = function(w) {
      futile.logger::flog.warn("%s: %s - %s", id, w$message, toString(w$call),
        name = "EpiNow2.epinow"
      )
      rlang::cnd_muffle(w)
    }
  ),
  error = function(e) {
    if (id %in% "epinow") {
      stop(e)
    } else {
      error_text <- sprintf("%s: %s - %s", id, e$message, toString(e$call))
      futile.logger::flog.error(error_text,
        name = "EpiNow2.epinow"
      )
      rlang::cnd_muffle(e)
      return(list(error = error_text))
    }
  }
  )
  end_time <- Sys.time()
  if (!is.null(out$error)) {
    out$trace <- rlang::trace_back()
  }

  if (!is.null(target_folder) & !is.null(out$error)) {
    saveRDS(out$error, paste0(target_folder, "/error.rds"))
    saveRDS(out$trace, paste0(target_folder, "/trace.rds"))
  }

  # log timing if specified
  if (output["timing"]) {
    out$timing <- round(as.numeric(end_time - start_time), 1)
    if (!is.null(target_folder)) {
      saveRDS(out$timing, paste0(target_folder, "/runtime.rds"))
    }
  }

  # copy all results to latest folder
  if (output["latest"]) {
    copy_results_to_latest(target_folder, latest_folder)
  }

  # return output
  if (return_output) {
    class(out) <- c("epinow", class(out))
    return(out)
  } else {
    return(invisible(NULL))
  }
}
