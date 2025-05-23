#' Setup Logging
#'
#' @description `r lifecycle::badge("questioning")`
#' Sets up `{futile.logger}` logging, which is integrated into `{EpiNow2}`.
#' See the documentation for `{futile.logger}` for full details. By default
#' `{EpiNow2}` prints all logs at the "INFO" level and returns them to the
#' console. Usage of logging is currently being explored as the current
#' setup cannot log stan errors or progress.
#'
#' @param threshold Character string indicating the logging level see
#' (?futile.logger for details of the available options). Defaults to "INFO".
#'
#' @param file Character string indicating the path to save logs to. By default
#' logs will be written to the console.
#'
#' @param mirror_to_console Logical, defaults to `FALSE`. If saving logs to a
#' file should they also be duplicated in the console.
#'
#' @param name Character string defaulting to EpiNow2. This indicates the name
#' of the logger to setup. The default logger for EpiNow2 is called EpiNow2.
#' Nested options include: Epinow2.epinow which controls all logging for
#' [epinow()] and nested functions, EpiNow2.epinow.estimate_infections
#' (logging in [estimate_infections()], and
#' EpiNow2.epinow.estimate_infections.fit (logging in fitting functions).
#'
#' @importFrom futile.logger flog.threshold flog.appender appender.tee
#' @importFrom futile.logger appender.file flog.info
#' @importFrom cli cli_inform col_blue
#' @return Nothing
#' @export
setup_logging <- function(threshold = "INFO", file = NULL,
                          mirror_to_console = FALSE, name = "EpiNow2") {
  if (is.null(name)) {
    name <- "ROOT"
  }
  cli_inform(
    "Logging threshold set at {threshold} for the name logger"
  )
  futile.logger::flog.threshold(threshold, name = name)

  if (!is.null(file)) {
    if (mirror_to_console) {
      cli_inform(
        "Writing {col_blue(name)} logs to the console and: {.file {file}}."
      )
      futile.logger::flog.appender(
        futile.logger::appender.tee(file),
        name = name
      )
    } else {
      cli_inform(
        "Writing {col_blue(name)} logs to: {.file {file}}."
      )
      futile.logger::flog.appender(
        futile.logger::appender.file(file),
        name = name
      )
    }
  } else {
    cli_inform(
      "Writing {col_blue(name)} logs to the console."
    )
    futile.logger::flog.appender(futile.logger::appender.console(), name = name)
  }
  return(invisible(NULL))
}

#' Setup Default Logging
#'
#' @description `r lifecycle::badge("questioning")`
#' Sets up default logging. Usage of logging is currently being explored as the
#' current setup cannot log stan errors or progress.
#'
#' @param logs Character path indicating the target folder in which to store log
#' information. Defaults to the temporary directory if not specified. Default
#' logging can be disabled if `logs` is set to NULL. If specifying a custom
#' logging setup then the code for [setup_default_logging()] and the
#' [setup_logging()] function are a sensible place to start.
#'
#' @param mirror_epinow Logical, defaults to FALSE. Should internal logging be
#' returned from [epinow()] to the console.
#'
#' @inheritParams setup_target_folder
#' @return No return value, called for side effects
#' @export
#' @importFrom purrr walk
#' @examples
#' setup_default_logging()
setup_default_logging <- function(logs = tempdir(check = TRUE),
                                  mirror_epinow = FALSE,
                                  target_date = NULL) {
  if (!is.null(logs)) {
    if (is.null(target_date)) {
      target_date <- "latest"
    }
    # make log paths
    log_path <- list()
    log_path$regional_epinow <- file.path(logs, "regional-epinow")
    log_path$epinow <- file.path(logs, "epinow")

    purrr::walk(log_path, function(path) {
      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
      }
    })

    # set up logs
    setup_logging("INFO",
      file = paste0(log_path$regional_epinow, "/", target_date, ".log"),
      mirror_to_console = TRUE
    )
    setup_logging("INFO",
      file = paste0(log_path$epinow, "/", target_date, ".log"),
      mirror_to_console = mirror_epinow,
      name = "EpiNow2.epinow"
    )
  }
  return(invisible(NULL))
}

#' Set up Future Backend
#' @description `r lifecycle::badge("stable")`
#' A utility function that aims to streamline the set up
#' of the required future backend with sensible defaults for most users of
#' [regional_epinow()]. More advanced users are recommended to setup their own
#' `{future}` backend based on their available resources. Running this requires
#' the `{future}` package to be installed.
#'
#' @param strategies A vector length 1 to 2 of strategies to pass to
#' [future::plan()]. Nesting of parallelisation is from the top level down.
#' The default is to set up nesting parallelisation with both using
#' [future::multisession()] ([future::multicore()] will likely be a faster
#' option on supported platforms). For single level parallelisation use a
#' single strategy or [future::plan()] directly. See [future::plan()] for
#' options.
#'
#' @param min_cores_per_worker Numeric, the minimum number of cores per worker.
#' Defaults to 4 which assumes 4 MCMC chains are in use per region.
#'
#' @inheritParams regional_epinow
#' @importFrom futile.logger flog.error flog.info flog.debug
#' @importFrom cli cli_abort
#' @export
#' @return Numeric number of cores to use per worker. If greater than 1 pass to
#' `stan_args = list(cores = "output from setup future")` or use
#' `future = TRUE`. If only a single strategy is used then nothing is returned.
setup_future <- function(data,
                         strategies = c("multisession", "multisession"),
                         min_cores_per_worker = 4) {
  if (!requireNamespace("future", quietly = TRUE)) {
    futile.logger::flog.error(
      "The future package is required for parallelisation"
    )
    cli_abort(
      c(
        "!" = "The future package is required for parallelisation."
      )
    )
  }
  if (length(strategies) > 2 || length(strategies) == 0) {
    futile.logger::flog.error("1 or 2 strategies should be used")
    cli_abort(
      c(
        "!" = "{.var strategies} must either be of length 1 or 2."
      )
    )
  }
  if (is.null(data$region)) {
    futile.logger::flog.error("Reported cases must contain a region")
    cli_abort(
      c(
        "!" = "Exactly 2 strategies should be used."
      )
    )
  }
  if (length(strategies) == 1) {
    workers <- future::availableCores()
    futile.logger::flog.info(
      "Using %s workers with 1 core per worker",
      workers
    )
    future::plan(strategies,
      workers = workers,
      gc = TRUE, earlySignal = TRUE
    )
    cores_per_worker <- 1
    return(invisible(NULL))
  } else {
    jobs <- length(unique(data$region))
    workers <- min(
      ceiling(future::availableCores() / min_cores_per_worker), jobs
    )
    cores_per_worker <- max(1, round(future::availableCores() / workers, 0))

    futile.logger::flog.info(
      "Using %s workers with %s cores per worker",
      workers, cores_per_worker
    )

    future::plan(list(
      future::tweak(strategies[1],
        workers = workers,
        gc = TRUE, earlySignal = TRUE
      ),
      future::tweak(strategies[2], workers = cores_per_worker)
    ))
    return(cores_per_worker)
  }
}

#' Convert to Data Table
#' @description `r lifecycle::badge("stable")`
#' Convenience function that sets the number of `{data.table}` cores to 1 and
#' maps input to be a `{data.table}`
#' @inheritParams estimate_infections
#' @return A data table
#' @keywords internal
setup_dt <- function(data) {
  suppressMessages(data.table::setDTthreads(threads = 1))
  data.table::setDT(data)
}

#' Setup Target Folder for Saving
#'
#' @description `r lifecycle::badge("stable")`
#' Sets up a folders for saving results
#' @param target_date Date, defaults to maximum found in the data if not
#' specified.
#'
#' @param target_folder Character string specifying where to save results (will
#' create if not present).
#'
#' @return A list containing the path to the dated folder and the latest folder
#' @keywords internal
setup_target_folder <- function(target_folder = NULL, target_date) {
  if (!is.null(target_folder)) {
    latest_folder <- file.path(target_folder, "latest")
    target_folder <- file.path(target_folder, target_date)
    if (!dir.exists(target_folder)) {
      dir.create(target_folder, recursive = TRUE)
    }
    return(list(date = target_folder, latest = latest_folder))
  } else {
    return(invisible(NULL))
  }
}
