#' Get Folders with Results
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param results_dir A character string giving the directory in which results
#'  are stored (as produced by [regional_epinow()]).
#'
#' @return A named character vector containing the results to plot.
#' @keywords internal
get_regions <- function(results_dir) {
  # regions to include - based on folder names
  regions <- list.dirs(results_dir,
    recursive = FALSE,
    full.names = FALSE
  )

  # put into alphabetical order
  regions <- regions[regions != "runtimes.csv"]
  regions <- sort(regions)
  names(regions) <- regions
  regions
}

#' Get a Single Raw Result
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param file Character string giving the result files name.
#'
#' @param region Character string giving the region of interest.
#'
#' @param date Target date (in the format `"yyyy-mm-dd`).
#'
#' @param result_dir Character string giving the location of the target
#' directory.
#'
#' @return An R object read in from the targeted `.rds` file
#' @keywords internal
get_raw_result <- function(file, region, date,
                           result_dir) {
  file_path <- file.path(result_dir, region, date, file)
  readRDS(file_path)
}
#' Get Combined Regional Results
#'
#' @description `r lifecycle::badge("stable")`
#' Summarises results across regions either from input or from disk. See the
#' examples for details.
#'
#' @param regional_output A list of output as produced by [regional_epinow()]
#' and stored in the `regional` list.
#'
#' @param results_dir A character string indicating the folder containing the
#' `{EpiNow2}` results to extract.
#'
#' @param date A Character string (in the format "yyyy-mm-dd") indicating the
#' date to extract data for. Defaults to "latest" which finds the latest
#' results available.
#'
#' @param samples Logical, defaults to `TRUE`. Should samples be returned.
#'
#' @param forecast Logical, defaults to `FALSE`. Should forecast results be
#' returned.
#'
#' @return A list of estimates, forecasts and estimated cases by date of report.
#' @export
#' @importFrom purrr map safely
#' @importFrom data.table rbindlist
#' @examples
#' # get example multiregion estimates
#' regional_out <- readRDS(system.file(
#'   package = "EpiNow2", "extdata", "example_regional_epinow.rds"
#' ))
#'
#' # from output
#' results <- get_regional_results(regional_out$regional, samples = FALSE)
get_regional_results <- function(regional_output,
                                 results_dir, date,
                                 samples = TRUE,
                                 forecast = FALSE) {
  if (missing(regional_output)) {
    regional_output <- NULL
  }

  if (is.null(regional_output)) {
    # assign to latest likely date if not given
    if (missing(date)) {
      date <- "latest"
    }
    # find all regions
    regions <- get_regions(results_dir)

    load_data <- purrr::safely(get_raw_result) # nolint

    # get estimates
    get_estimates_file <- function(samples_path, summarised_path) {
      out <- list()

      if (samples) {
        samples <- purrr::map(
          regions, ~ load_data(
            samples_path, .,
            result_dir = results_dir,
            date = date
          )[[1]]
        )
        samples <- data.table::rbindlist(samples, idcol = "region", fill = TRUE)
        out$samples <- samples
      }
      # get incidence values and combine
      summarised <- purrr::map(
        regions, ~ load_data(
          summarised_path,
          .,
          result_dir = results_dir,
          date = date
        )[[1]]
      )
      summarised <- data.table::rbindlist(
        summarised,
        idcol = "region", fill = TRUE
      )
      out$summarised <- summarised
      out
    }
    out <- list()
    out$estimates <- get_estimates_file(
      samples_path = "estimate_samples.rds",
      summarised_path = "summarised_estimates.rds"
    )

    if (forecast) {
      out$estimated_reported_cases <- get_estimates_file(
        samples_path = "estimated_reported_cases_samples.rds",
        summarised_path = "summarised_estimated_reported_cases.rds"
      )
    }
  } else {
    out <- list()
    estimates_out <- list()

    if (samples) {
      samp <- purrr::map(regional_output, get_samples)
      samp <- data.table::rbindlist(samp, idcol = "region", fill = TRUE)
      estimates_out$samples <- samp
    }
    summarised <- purrr::map(
      regional_output, summary, type = "parameters"
    )
    summarised <- data.table::rbindlist(
      summarised,
      idcol = "region", fill = TRUE
    )
    estimates_out$summarised <- summarised
    out$estimates <- estimates_out

    if (forecast) {
      erc_out <- list()
      erc_data <- purrr::map(regional_output, estimates_by_report_date)
      if (samples) {
        samp <- purrr::map(erc_data, ~ .$samples)
        samp <- data.table::rbindlist(samp, idcol = "region", fill = TRUE)
        erc_out$samples <- samp
      }
      summarised <- purrr::map(erc_data, ~ .$summarised)
      summarised <- data.table::rbindlist(
        summarised,
        idcol = "region", fill = TRUE
      )
      erc_out$summarised <- summarised
      out$estimated_reported_cases <- erc_out
    }
  }
  out
}

#' Get Regions with Most Reported Cases
#'
#' @description `r lifecycle::badge("stable")`
#' Extract a vector of regions with the most reported cases in a set time
#' window.
#'
#' @param time_window Numeric, number of days to include from latest date in
#' data. Defaults to 7 days.
#'
#' @param no_regions Numeric, number of regions to return. Defaults to 6.
#'
#' @inheritParams regional_epinow
#'
#' @return A character vector of regions with the highest reported cases
#'
#' @importFrom data.table copy setorderv
#' @importFrom lubridate days
#' @keywords internal
get_regions_with_most_reports <- function(data,
                                          time_window = 7,
                                          no_regions = 6) {
  most_reports <- data.table::copy(data)
  most_reports <-
    most_reports[,
      .SD[date >= (max(date, na.rm = TRUE) - lubridate::days(time_window))],
      by = "region"
    ]
  most_reports <- most_reports[,
    .(confirm = sum(confirm, na.rm = TRUE)),
    by = "region"
  ]
  most_reports <- data.table::setorderv(
    most_reports,
    cols = "confirm", order = -1
  )
  most_reports[1:no_regions][!is.na(region)]$region
}

##' Estimate seeding time from delays and generation time
##'
##' The seeding time is set to the mean of the specified delays, constrained
##' to be at least the maximum generation time
##' @inheritParams estimate_infections
##' @return An integer seeding time
##' @keywords internal
get_seeding_time <- function(delays, generation_time, rt = rt_opts()) {
  # Estimate the mean delay -----------------------------------------------
  seeding_time <- sum(mean(delays, ignore_uncertainty = TRUE))
  if (!is.null(rt)) {
    ## make sure we have at least max(generation_time) seeding time
    seeding_time <- max(seeding_time, sum(max(generation_time)))
  }
  max(round(seeding_time), 1)
}

#' Get posterior samples from a fitted model
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts posterior samples from a fitted model, combining all parameters
#' into a single data.table with dates and metadata.
#'
#' @param object A fitted model object (e.g., from `estimate_infections()`)
#' @param ... Additional arguments (currently unused)
#'
#' @return A `data.table` with columns: date, variable, strat, sample, time,
#'   value, type. Contains all posterior samples for all parameters.
#'
#' @export
#' @examples
#' \dontrun{
#' # After fitting a model
#' samples <- get_samples(fit)
#' # Filter to specific parameters
#' R_samples <- samples[variable == "R"]
#' }
get_samples <- function(object, ...) {
  UseMethod("get_samples")
}

#' @rdname get_samples
#' @export
get_samples.estimate_infections <- function(object, ...) {
  raw_samples <- extract_samples(object$fit)

  for (arg_name in names(object$args)) {
    if (!(arg_name %in% names(raw_samples))) {
      raw_samples[[arg_name]] <- object$args[[arg_name]]
    }
  }

  format_samples_with_dates(
    raw_samples = raw_samples,
    args = object$args,
    observations = object$observations
  )
}

#' @rdname get_samples
#' @export
get_samples.forecast_infections <- function(object, ...) {
  data.table::copy(object$samples)
}

#' @rdname get_samples
#' @export
get_samples.estimate_secondary <- function(object, ...) {
  # Extract raw posterior samples from the fit
  raw_samples <- extract_samples(object$fit)

  # Extract parameters (delays and params)
  samples_list <- list(
    extract_delays(raw_samples),
    extract_parameters(raw_samples)
  )

  # Extract time-varying generated quantities
  # Get dates for time-indexed parameters (post burn-in)
  burn_in <- object$args$burn_in
  dates <- object$observations[(burn_in + 1):.N]$date

  # Extract sim_secondary (generated quantity, post burn-in) if available
  sim_secondary_samples <- extract_latent_state(
    "sim_secondary", raw_samples, dates
  )
  if (!is.null(sim_secondary_samples)) {
    samples_list <- c(samples_list, list(sim_secondary_samples))
  }

  # Combine all samples
  samples <- data.table::rbindlist(samples_list, fill = TRUE)

  # Rename 'parameter' column to 'variable' for consistency if needed
  if ("parameter" %in% names(samples)) {
    data.table::setnames(samples, "parameter", "variable")
  }

  # Add placeholder columns for consistency with estimate_infections format
  # Only add if not already present
  if (!"date" %in% names(samples)) samples[, date := as.Date(NA)]
  if (!"strat" %in% names(samples)) samples[, strat := NA_character_]
  if (!"time" %in% names(samples)) samples[, time := NA_integer_]
  if (!"type" %in% names(samples)) samples[, type := NA_character_]

  # Reorder columns
  data.table::setcolorder(
    samples,
    c("date", "variable", "strat", "sample", "time", "value", "type")
  )

  samples[]
}

#' @rdname get_samples
#' @export
get_samples.forecast_secondary <- function(object, ...) {
  data.table::copy(object$samples)
}

#' Get predictions from a fitted model
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts predictions from a fitted model, combining observations with model
#' estimates. For `estimate_infections()` returns predicted reported cases, for
#' `estimate_secondary()` returns predicted secondary observations.
#'
#' @param object A fitted model object (e.g., from `estimate_infections()` or
#'   `estimate_secondary()`)
#' @param CrIs Numeric vector of credible intervals to return. Defaults to
#'   c(0.2, 0.5, 0.9).
#' @param ... Additional arguments (currently unused)
#'
#' @return A `data.table` with columns including date, observations, and summary
#'   statistics (mean, sd, credible intervals) for the model predictions.
#'
#' @export
#' @examples
#' \dontrun{
#' # After fitting a model
#' predictions <- get_predictions(fit)
#' }
get_predictions <- function(object, ...) {
  UseMethod("get_predictions")
}

#' @rdname get_predictions
#' @export
get_predictions.estimate_infections <- function(object,
                                                CrIs = c(0.2, 0.5, 0.9),
                                                ...) {
  # Get samples for reported cases
  samples <- get_samples(object)
  reported_samples <- samples[variable == "reported_cases"]

  # Calculate summary measures
  predictions <- calc_summary_measures(
    reported_samples,
    summarise_by = "date",
    order_by = "date",
    CrIs = CrIs
  )

  # Merge with observations
  predictions <- data.table::merge.data.table(
    object$observations[, .(date, confirm)],
    predictions,
    by = "date",
    all = TRUE
  )

  predictions
}

#' @rdname get_predictions
#' @export
get_predictions.estimate_secondary <- function(object,
                                               CrIs = c(0.2, 0.5, 0.9),
                                               ...) {
  # Get samples for simulated secondary observations
  samples <- get_samples(object)
  sim_secondary_samples <- samples[variable == "sim_secondary"]

  # Calculate summary measures
  predictions <- calc_summary_measures(
    sim_secondary_samples,
    summarise_by = "date",
    order_by = "date",
    CrIs = CrIs
  )

  # Merge with observations
  predictions <- data.table::merge.data.table(
    object$observations, predictions,
    all = TRUE, by = "date"
  )

  predictions
}

#' @rdname get_predictions
#' @export
get_predictions.forecast_infections <- function(object, ...) {
  data.table::copy(object$predictions)
}

#' @rdname get_predictions
#' @export
get_predictions.forecast_secondary <- function(object, ...) {
  data.table::copy(object$predictions)
}
