#' Real-time Rt Estimation, Forecasting and Reporting
#'
#' @description `r lifecycle::badge("stable")`
#' This function wraps the functionality of [estimate_infections()] in order
#' to estimate Rt and cases by date of infection and forecast these infections
#' into the future. In addition to the functionality of
#' [estimate_infections()] it produces additional summary output useful for
#' reporting results and interpreting them as well as error catching and
#' reporting, making it particularly useful for production use e.g. running at
#' set intervals on a dedicated server.
#'
#' @param output A character vector of optional output to return. Supported
#' options are samples ("samples"), plots ("plots"), the run time ("timing"),
#' copying the dated folder into a latest folder (if `target_folder` is not
#' null, set using "latest"), and the stan fit ("fit"). The default is to
#' return all options.
#'
#' @param return_output Logical, defaults to FALSE. Should output be returned,
#' this automatically updates to TRUE if no directory for saving is specified.
#'
#' @param plot_args A list of optional arguments passed to [plot.epinow()].
#'
#' @return A list of output from estimate_infections with additional elements
#'   summarising results and reporting errors if they have occurred.
#' @export
#' @seealso [estimate_infections()] [forecast_infections()] [regional_epinow()]
#' @inheritParams setup_target_folder
#' @inheritParams estimate_infections
#' @inheritParams setup_default_logging
#' @importFrom data.table setDT
#' @importFrom lubridate days
#' @importFrom futile.logger flog.fatal flog.warn flog.error flog.debug ftry
#' @importFrom rlang cnd_muffle
#' @importFrom checkmate assert_string assert_path_for_output
#' assert_date assert_logical
#' @importFrom R.utils isDirectory
#' @examples
#' \donttest{
#' # set number of cores to use
#' old_opts <- options()
#' options(mc.cores = ifelse(interactive(), 4, 1))
#'
#' # set an example generation time. In practice this should use an estimate
#' # from the literature or be estimated from data
#' generation_time <- Gamma(
#'   shape = Normal(1.3, 0.3),
#'   rate = Normal(0.37, 0.09),
#'   max = 14
#' )
#' # set an example incubation period. In practice this should use an estimate
#' # from the literature or be estimated from data
#' incubation_period <- LogNormal(
#'    meanlog = Normal(1.6, 0.06),
#'    sdlog = Normal(0.4, 0.07),
#'    max = 14
#' )
#' # set an example reporting delay. In practice this should use an estimate
#' # from the literature or be estimated from data
#' reporting_delay <- LogNormal(mean = 2, sd = 1, max = 10)
#'
#' # example case data
#' reported_cases <- example_confirmed[1:40]
#'
#' # estimate Rt and nowcast/forecast cases by date of infection
#' out <- epinow(
#'   data = reported_cases,
#'   generation_time = generation_time_opts(generation_time),
#'   rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
#'   delays = delay_opts(incubation_period + reporting_delay)
#' )
#' # summary of the latest estimates
#' summary(out)
#' # plot estimates
#' plot(out)
#'
#' # summary of R estimates
#' summary(out, type = "parameters", params = "R")
#'
#' options(old_opts)
#' }
# nolint start: cyclocomp_linter
epinow <- function(data,
                   generation_time = generation_time_opts(),
                   delays = delay_opts(),
                   truncation = trunc_opts(),
                   rt = rt_opts(),
                   backcalc = backcalc_opts(),
                   gp = gp_opts(),
                   obs = obs_opts(),
                   stan = stan_opts(),
                   horizon = 7,
                   CrIs = c(0.2, 0.5, 0.9),
                   filter_leading_zeros = TRUE,
                   zero_threshold = Inf,
                   return_output = FALSE,
                   output = c("samples", "plots", "latest", "fit", "timing"),
                   plot_args = list(),
                   target_folder = NULL, target_date,
                   logs = tempdir(), id = "epinow", verbose = interactive(),
                   reported_cases) {
  if (!missing(reported_cases)) {
    lifecycle::deprecate_stop(
      "1.5.0",
      "epinow(reported_cases)",
      "epinow(data)"
    )
  }
  # Check inputs
  assert_logical(return_output)
  stopifnot("target_folder is not a directory" =
              !is.null(target_folder) || isDirectory(target_folder)
            )
  if (!missing(target_date)) {
    assert_string(target_date)
  }
  assert_string(id)
  assert_logical(verbose)

  if (is.null(target_folder)) {
    return_output <- TRUE
  }

  if (is.null(CrIs) || length(CrIs) == 0 || !is.numeric(CrIs)) {
    futile.logger::flog.fatal(
      "At least one credible interval must be specified",
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
    target_date <- max(data$date, na.rm = TRUE)
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
    reported_cases <- setup_dt(data)

    # save input data ---------------------------------------------------------
    save_input(reported_cases, target_folder)

    # make sure the horizon is as specified from the target date --------------
    horizon <- update_horizon(horizon, target_date, reported_cases)

    # estimate infections and Reproduction no ---------------------------------
    estimates <- estimate_infections(
      data = reported_cases,
      generation_time = generation_time,
      delays = delays,
      truncation = truncation,
      rt = rt,
      backcalc = backcalc,
      gp = gp,
      obs = obs,
      stan = stan,
      CrIs = CrIs,
      filter_leading_zeros = filter_leading_zeros,
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
      ))
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
  out <- tryCatch(
    withCallingHandlers(
      epinow_internal(),
      warning = function(w) {
        futile.logger::flog.warn("%s: %s - %s", id, w$message, toString(w$call),
          name = "EpiNow2.epinow"
        )
        rlang::cnd_muffle(w)
      }
    ),
    error = function(e) {
      if (id == "epinow") {
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

  if (!is.null(target_folder) && !is.null(out$error)) {
    saveRDS(out$error, file.path(target_folder, "error.rds"))
    saveRDS(out$trace, file.path(target_folder, "trace.rds"))
  }

  # log timing if specified
  if (output["timing"]) {
    out$timing <- difftime(end_time, start_time)
    if (!is.null(target_folder)) {
      saveRDS(out$timing, file.path(target_folder, "runtime.rds"))
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
# nolint end: cyclocomp_linter
