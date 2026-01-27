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
#' null, set using "latest"), the stan fit ("fit"), and the full
#' [estimate_infections()] return object ("estimate_infections"). The default
#' is to return all options.
#'
#' @param return_output Logical, defaults to FALSE. Should output be returned,
#' this automatically updates to TRUE if no directory for saving is specified.
#'
#' @param plot_args A list of optional arguments passed to
#' [plot.estimate_infections()].
#'
#' @return An `<epinow>` object (inheriting from `<estimate_infections>`)
#' containing:
#'
#' - `fit`: The stan fit object.
#' - `args`: A list of arguments used for fitting (stan data).
#' - `observations`: The input data (`<data.frame>`).
#' - `timing`: The run time (if `output` includes "timing").
#' @export
#' @seealso [get_samples()] [get_predictions()] [get_delays()]
#' [estimate_infections()] [forecast_infections()] [regional_epinow()]
#' @inheritParams calc_CrIs
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
#'   meanlog = Normal(1.6, 0.06),
#'   sdlog = Normal(0.4, 0.07),
#'   max = 14
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
#'   generation_time = gt_opts(generation_time),
#'   rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.1)),
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
                   generation_time = gt_opts(),
                   delays = delay_opts(),
                   truncation = trunc_opts(),
                   rt = rt_opts(),
                   backcalc = backcalc_opts(),
                   gp = gp_opts(),
                   obs = obs_opts(),
                   forecast = forecast_opts(),
                   stan = stan_opts(),
                   CrIs = c(0.2, 0.5, 0.9),
                   return_output = is.null(target_folder),
                   output = c(
                     "samples", "plots", "latest", "fit", "timing",
                     "estimate_infections"
                   ),
                   plot_args = list(),
                   target_folder = NULL, target_date,
                   logs = tempdir(), id = "epinow", verbose = interactive(),
                   filter_leading_zeros = TRUE, zero_threshold = Inf, horizon
                   ) {
  if (!missing(filter_leading_zeros)) {
    lifecycle::deprecate_stop(
      "1.7.0",
      "estimate_infections(filter_leading_zeros)",
      "filter_leading_zeros()"
    )
  }
  if (!missing(zero_threshold)) {
    lifecycle::deprecate_stop(
      "1.7.0",
      "estimate_infections(zero_threshold)",
      "apply_zero_threshold()"
    )
  }
  if (!missing(horizon)) {
    lifecycle::deprecate_stop(
      "1.7.0",
      "epinow(horizon)",
      "epinow(forecast)",
      details = "The `horizon` argument passed to `epinow()` will
        override any `horizon` argument passed via `forecast_opts()`."
    )
  }
  # Check inputs
  assert_logical(return_output)
  stopifnot(
    "target_folder is not a directory" =
      !is.null(target_folder) || isDirectory(target_folder)
  )
  if (!missing(target_date)) {
    assert_string(target_date)
  }
  assert_string(id)
  assert_logical(verbose)

  if (is.null(forecast)) {
    forecast <- forecast_opts(horizon = 0)
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
      "latest", "estimate_infections"
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
    horizon <- update_horizon(forecast$horizon, target_date, reported_cases)

    estimates <- estimate_infections(
      data = reported_cases,
      generation_time = generation_time,
      delays = delays,
      truncation = truncation,
      rt = rt,
      backcalc = backcalc,
      gp = gp,
      obs = obs,
      forecast = forecast,
      stan = stan,
      verbose = verbose,
      id = id
    )

    save_estimate_infections(estimates, target_folder,
      samples = output["samples"],
      return_fit = output["fit"],
      CrIs = CrIs
    )

    # report forecasts ---------------------------------------------------------
    estimated_reported_cases <- estimates_by_report_date(estimates,
      target_folder = target_folder,
      samples = output["samples"],
      CrIs = CrIs
    )

    # report estimates --------------------------------------------------------
    summary <- summary(estimates,
      return_numeric = TRUE,
      target_folder = target_folder,
      CrIs = CrIs
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
      return(estimates)
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

#' Internal function for backward-compatible element access
#'
#' @description Helper function that provides backward compatibility for
#' deprecated element names in epinow objects.
#'
#' @param x An `epinow` object
#' @param name The name of the element to extract
#' @return The requested element
#' @keywords internal
#' @noRd
epinow_compat_extract <- function(x, name) {
  switch(name,
    estimates = {
      lifecycle::deprecate_warn(
        "1.8.0",
        I("epinow()$estimates"),
        details = paste(
          "Use `get_samples()` for samples,",
          "`summary(x, type = 'parameters')` for summarised estimates."
        )
      )
      list(
        samples = get_samples(x),
        summarised = summary(x, type = "parameters"),
        fit = x$fit,
        args = x$args,
        observations = x$observations
      )
    },
    estimated_reported_cases = {
      lifecycle::deprecate_warn(
        "1.8.0",
        I("epinow()$estimated_reported_cases"),
        "estimates_by_report_date()"
      )
      estimates_by_report_date(x)
    },
    summary = {
      lifecycle::deprecate_warn(
        "1.8.0",
        I("epinow()$summary"),
        "summary()"
      )
      latest_date <- max(x$observations$date, na.rm = TRUE)
      summarised <- summary(x, type = "parameters")
      summarised <- summarised[date == latest_date]
      rt_samples <- get_samples(x)[variable == "R" & date == latest_date]
      report_summary(summarised, rt_samples, return_numeric = TRUE)
    },
    plots = {
      lifecycle::deprecate_warn(
        "1.8.0",
        I("epinow()$plots"),
        "plot()"
      )
      plot(x, type = "all")
    },
    estimate_infections = {
      lifecycle::deprecate_warn(
        "1.8.0",
        I("epinow()$estimate_infections"),
        details = paste(
          "The epinow object now inherits from estimate_infections.",
          "Use the object directly."
        )
      )
      out <- x
      class(out) <- setdiff(class(out), "epinow")
      out
    },
    NULL
  )
}

#' Extract elements from epinow objects with deprecated warnings
#'
#' @description `r lifecycle::badge("deprecated")`
#' Provides backward compatibility for the old return structure. The previous
#' structure with \code{estimates}, \code{estimated_reported_cases},
#' \code{summary}, \code{plots}, and \code{estimate_infections} elements is
#' deprecated. Use the standard S3 methods
#' instead:
#' \itemize{
#'   \item \code{estimates$samples} - use \code{get_samples(object)}
#'   \item \code{estimates$summarised} - use \code{summary(object,
#'     type = "parameters")}
#'   \item \code{estimated_reported_cases} - use
#'     \code{estimates_by_report_date(object)}
#'   \item \code{summary} - use \code{summary(object)}
#'   \item \code{plots} - use \code{plot(object)}
#'   \item \code{estimate_infections} - use the object directly (it now
#'     inherits from \code{estimate_infections})
#' }
#'
#' @param x An \code{epinow} object
#' @param name The name of the element to extract
#' @return The requested element with a deprecation warning for deprecated
#'   elements
#' @keywords internal
#' @export
#' @method $ epinow
`$.epinow` <- function(x, name) {
  deprecated_names <- c(
    "estimates", "estimated_reported_cases", "summary", "plots",
    "estimate_infections"
  )
  result <- epinow_compat_extract(x, name)
  if (!is.null(result) || name %in% deprecated_names) {
    return(result)
  }
  .subset2(x, name)
}

#' Extract elements from epinow objects with bracket notation
#'
#' @description `r lifecycle::badge("deprecated")`
#' Provides backward compatibility for bracket-based access to deprecated
#' elements. See [$.epinow] for details on the deprecation.
#'
#' @param x An `epinow` object
#' @param i The name or index of the element to extract
#' @return The requested element with a deprecation warning for deprecated
#'   elements
#' @keywords internal
#' @export
#' @method [[ epinow
`[[.epinow` <- function(x, i) {
  deprecated_names <- c(
    "estimates", "estimated_reported_cases", "summary", "plots",
    "estimate_infections"
  )
  result <- epinow_compat_extract(x, i)
  if (!is.null(result) || i %in% deprecated_names) {
    return(result)
  }
  .subset2(x, i)
}
