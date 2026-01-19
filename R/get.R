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

  # Extract parameters (delays and params) - variable column added by extract_*
  delay_samples <- extract_delays(raw_samples, args = object$args)
  param_samples <- extract_parameters(raw_samples, args = object$args)
  samples_list <- list(delay_samples, param_samples)

  # Extract time-varying generated quantities
  # Get dates for time-indexed parameters (post burn-in)
  burn_in <- object$args$burn_in
  dates <- object$observations[(burn_in + 1):.N]$date

  # Extract sim_secondary (generated quantity, post burn-in) if available
  sim_secondary_samples <- extract_latent_state(
    "sim_secondary", raw_samples, dates
  )
  if (!is.null(sim_secondary_samples)) {
    sim_secondary_samples[, variable := "sim_secondary"]
    samples_list <- c(samples_list, list(sim_secondary_samples))
  }

  # Combine all samples
  samples <- data.table::rbindlist(samples_list, fill = TRUE)

  # Add placeholder columns for consistency with estimate_infections format
  if (!"date" %in% names(samples)) samples[, date := as.Date(NA)]
  if (!"strat" %in% names(samples)) samples[, strat := NA_character_]
  if (!"time" %in% names(samples)) samples[, time := NA_integer_]
  if (!"type" %in% names(samples)) samples[, type := NA_character_]

  # Reorder columns to match estimate_infections format
  data.table::setcolorder(
    samples,
    c("variable", "parameter", "time", "date", "sample", "value", "strat",
      "type")
  )

  samples[]
}

#' @rdname get_samples
#' @export
get_samples.forecast_secondary <- function(object, ...) {
  data.table::copy(object$samples)
}

#' @rdname get_samples
#' @export
get_samples.estimate_truncation <- function(object, ...) {
  raw_samples <- extract_samples(object$fit)

  # Extract delay parameters (truncation distribution)
  # variable column is added by extract_delays()
  samples <- extract_delays(raw_samples, args = object$args)

  samples[]
}

#' Get predictions from a fitted model
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts predictions from a fitted model, combining observations with model
#' estimates. For `estimate_infections()` returns predicted reported cases, for
#' `estimate_secondary()` returns predicted secondary observations. For
#' `estimate_truncation()` returns reconstructed observations adjusted for
#' truncation.
#'
#' @param object A fitted model object (e.g., from `estimate_infections()`,
#'   `estimate_secondary()`, or `estimate_truncation()`)
#' @param CrIs Numeric vector of credible intervals to return. Defaults to
#'   c(0.2, 0.5, 0.9).
#' @param ... Additional arguments (currently unused)
#'
#' @return A `data.table` with columns including date and summary statistics
#'   (mean, sd, credible intervals) for the model predictions.
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

#' @rdname get_predictions
#' @export
get_predictions.estimate_truncation <- function(object,
                                                CrIs = c(0.2, 0.5, 0.9),
                                                ...) {
  # Extract reconstructed observations from fit
  recon_obs <- extract_stan_param(object$fit, "recon_obs",
    CrIs = CrIs,
    var_names = TRUE
  )
  recon_obs <- recon_obs[, id := variable][, variable := NULL]
  obs_sets <- object$args$obs_sets
  # Assign dataset index using modulo: rows cycle through 1..obs_sets
  # e.g., with obs_sets=3: row 1->1, row 2->2, row 3->3, row 4->1, ...
  recon_obs <- recon_obs[, dataset := seq_len(.N)][
    ,
    dataset := dataset %% obs_sets
  ][
    dataset == 0, dataset := obs_sets
  ]

  # Process input observations to get dates
  dirty_obs <- purrr::map(object$observations, data.table::as.data.table)
  earliest_date <- max(
    as.Date(
      purrr::map_chr(dirty_obs, function(x) x[, as.character(min(date))])
    )
  )
  dirty_obs <- purrr::map(dirty_obs, function(x) x[date >= earliest_date])
  nrow_obs <- order(purrr::map_dbl(dirty_obs, nrow))
  dirty_obs <- dirty_obs[nrow_obs]

  # Get truncation max from args
  trunc_max <- object$args$delay_max[1]

  # Link predictions to dates
  link_preds <- function(index) {
    target_obs <- dirty_obs[[index]][, idx := .N - 0:(.N - 1)]
    target_obs <- target_obs[idx < trunc_max]
    estimates <- recon_obs[dataset == index][, c("id", "dataset") := NULL]
    # Convert to integer: reconstructed observations are counts, so integer
    # representation is appropriate. CrI columns are also converted for
    # consistency since fractional counts are not meaningful.
    estimates <- estimates[, lapply(.SD, as.integer)]
    estimates <- estimates[, idx := .N - 0:(.N - 1)]
    if (!is.null(estimates$n_eff)) {
      estimates[, "n_eff" := NULL]
    }
    if (!is.null(estimates$Rhat)) {
      estimates[, "Rhat" := NULL]
    }

    # Merge predictions with date only (no observations)
    result <- data.table::merge.data.table(
      target_obs[, .(date, idx)],
      estimates,
      by = "idx", all.x = TRUE
    )
    result[, report_date := max(target_obs$date)]
    result[order(date)][, idx := NULL]
  }

  predictions <- purrr::map(seq_len(obs_sets), link_preds)
  data.table::rbindlist(predictions)
}

#' Get delay distributions from a fitted model
#'
#' @description `r lifecycle::badge("experimental")`
#' Extracts the delay distributions used in a fitted model. For
#' `estimate_infections()` this includes generation time, reporting delays,
#' and truncation delays. For `estimate_secondary()` this includes any
#' delays specified in the model. For `estimate_truncation()` this returns
#' the estimated truncation distribution.
#'
#' @param object A fitted model object (e.g., from `estimate_infections()`,
#'   `estimate_secondary()`, or `estimate_truncation()`)
#' @param ... Additional arguments passed to methods
#'
#' @return A named list of `dist_spec` objects representing the delay
#'   distributions. The list may be empty if no delays were specified.
#'   Use `names()` to see available delays.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all delays from a fitted model
#' delays <- get_delays(fit)
#' names(delays)
#' # Access specific delay
#' delays$generation_time
#' }
get_delays <- function(object, ...) {
  UseMethod("get_delays")
}

#' Reconstruct a dist_spec from stored stan data and posterior
#'
#' @param object A fitted model object containing fit and args
#' @param delay_name The name of the delay (e.g., "generation_time")
#' @return A dist_spec object, or NULL if the delay doesn't exist
#' @keywords internal
reconstruct_delay <- function(object, delay_name) {
  stan_data <- object$args

  # Get the delay ID for this named delay
  delay_id <- stan_data[[paste0("delay_id_", delay_name)]]
  if (is.null(delay_id) || delay_id == 0) {
    return(NULL)
  }

  types_groups <- stan_data$delay_types_groups
  if (is.null(types_groups)) {
    return(NULL)
  }

  # Extract posterior if parameters were estimated
  posterior <- NULL
  if (stan_data$delay_params_length > 0 && !is.null(object$fit)) {
    posterior <- extract_stan_param(object$fit, params = "delay_params")
  }

  # Get indices for this delay type
  delay_indices <- seq(types_groups[delay_id], types_groups[delay_id + 1] - 1)
  types_p <- stan_data$delay_types_p[delay_indices]

  # Reconstruct each delay component
  delay_list <- lapply(seq_along(delay_indices), function(i) {
    idx <- delay_indices[i]
    type_id <- stan_data$delay_types_id[idx]

    if (types_p[i] == 1) {
      reconstruct_parametric(stan_data, type_id, posterior)
    } else {
      reconstruct_nonparametric(stan_data, type_id)
    }
  })

  if (length(delay_list) == 1) delay_list[[1]] else do.call(c, delay_list)
}

#' Reconstruct a parametric delay distribution
#'
#' Helper function to reconstruct a single parametric delay component from
#' Stan data and posterior samples.
#'
#' @param stan_data List of Stan data containing delay specification
#' @param param_id Integer index into the parametric delay arrays
#' @param posterior Data frame with posterior mean and sd for delay_params,
#'   or NULL if not estimated
#' @return A `dist_spec` object representing the delay distribution
#' @keywords internal
reconstruct_parametric <- function(stan_data, param_id, posterior) {
  dist_type <- dist_spec_distributions()[stan_data$delay_dist[param_id] + 1]
  dist_max <- stan_data$delay_max[param_id]

  # Get parameter indices and values
  param_idx <- seq(
    stan_data$delay_params_groups[param_id],
    stan_data$delay_params_groups[param_id + 1] - 1
  )
  prior_mean <- stan_data$delay_params_mean[param_idx]
  prior_sd <- stan_data$delay_params_sd[param_idx]

  # Build parameters: posterior if estimated, fixed or prior otherwise
  # NA handling: if prior_sd[j] is NA, then prior_sd[j] > 0 evaluates to NA,
  # and NA && !is.na(NA) -> NA && FALSE -> FALSE, so estimated = FALSE.
  # This correctly treats NA prior_sd as a fixed (non-estimated) parameter.
  param_names <- natural_params(dist_type)
  parameters <- lapply(seq_along(prior_mean), function(j) {
    estimated <- prior_sd[j] > 0 && !is.na(prior_sd[j])
    if (estimated && !is.null(posterior)) {
      Normal(round(posterior$mean[param_idx[j]], 3),
             round(posterior$sd[param_idx[j]], 3))
    } else if (prior_sd[j] == 0 || is.na(prior_sd[j])) {
      prior_mean[j]
    } else {
      Normal(prior_mean[j], prior_sd[j])
    }
  })
  names(parameters) <- param_names

  new_dist_spec(params = parameters, max = dist_max, distribution = dist_type)
}

#' Reconstruct a nonparametric delay distribution
#'
#' Helper function to reconstruct a single nonparametric delay component from
#' Stan data. Nonparametric delays are stored as probability mass functions.
#'
#' @param stan_data List of Stan data containing delay specification
#' @param np_id Integer index into the nonparametric delay PMF arrays
#' @return A `dist_spec` object representing the nonparametric delay
#' @keywords internal
reconstruct_nonparametric <- function(stan_data, np_id) {
  pmf_idx <- seq(
    stan_data$delay_np_pmf_groups[np_id],
    stan_data$delay_np_pmf_groups[np_id + 1] - 1
  )
  NonParametric(pmf = stan_data$delay_np_pmf[pmf_idx])
}

#' @rdname get_delays
#' @export
get_delays.epinowfit <- function(object, ...) {
  stan_data <- object$args

  # Find all delay_id_* variables in stan_data with non-zero IDs
  id_vars <- grep("^delay_id_", names(stan_data), value = TRUE)
  available_names <- sub("^delay_id_", "", id_vars)
  available_names <- available_names[vapply(
    id_vars, function(v) !is.null(stan_data[[v]]) && stan_data[[v]] > 0,
    logical(1)
  )]

  # Return named list of all available delays (with posterior if estimated)
  delays <- lapply(available_names, function(n) reconstruct_delay(object, n))
  names(delays) <- available_names
  delays
}

#' @rdname get_delays
#' @export
get_delays.estimate_truncation <- function(object, ...) {
  list(truncation = reconstruct_delay(object, "truncation"))
}

#' Get a single delay distribution from a fitted model
#'
#' @description `r lifecycle::badge("experimental")`
#' Convenience function to extract a single delay distribution by name.
#' This is equivalent to `get_delays(object)[[type]]` but provides a
#' cleaner interface.
#'
#' @param object A fitted model object (e.g., from `estimate_infections()`,
#'   `estimate_secondary()`, or `estimate_truncation()`)
#' @param type Character string specifying the delay type to extract.
#'   Common values include `"generation_time"`, `"reporting"`, and
#'   `"truncation"`. Use `names(get_delays(object))` to see available types.
#' @param ... Additional arguments passed to methods
#'
#' @return A `dist_spec` object representing the requested delay distribution,
#'   or `NULL` if the specified type is not found.
#'
#' @seealso [get_delays()] to retrieve all delay distributions as a list
#' @export
#' @examples
#' \dontrun{
#' # Get truncation delay from estimate_truncation
#' trunc_dist <- get_delay(fit, "truncation")
#'
#' # Get generation time from estimate_infections
#' gt <- get_delay(fit, "generation_time")
#' }
get_delay <- function(object, type, ...) {
  UseMethod("get_delay")
}

#' @rdname get_delay
#' @export
get_delay.default <- function(object, type, ...) {
  delays <- get_delays(object, ...)
  delays[[type]]
}
