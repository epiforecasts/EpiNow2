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

#' @rdname get_samples
#' @export
get_samples.estimate_truncation <- function(object, ...) {
  raw_samples <- extract_samples(object$fit)

  samples_list <- list()

  # Extract delay parameters (truncation distribution)
  samples_list$delay_params <- extract_delays(raw_samples)

  # Extract reconstructed observations
  recon_obs <- raw_samples[["recon_obs"]]
  obs_sets <- object$args$obs_sets
  n_time <- object$args$t
  n_samples <- dim(recon_obs)[1]

  recon_dt <- data.table::data.table(
    sample = rep(seq_len(n_samples), each = n_time * obs_sets),
    time = rep(rep(seq_len(n_time), obs_sets), n_samples),
    dataset = rep(rep(seq_len(obs_sets), each = n_time), n_samples),
    value = as.vector(aperm(recon_obs, c(2, 3, 1)))
  )
  recon_dt[, variable := "recon_obs"]
  samples_list$recon_obs <- recon_dt

  # Combine all samples
  samples <- data.table::rbindlist(samples_list, fill = TRUE)

  # Rename 'parameter' column to 'variable' for consistency if needed
  if ("parameter" %in% names(samples)) {
    data.table::setnames(samples, "parameter", "variable")
  }

  # Add placeholder columns for consistency
  if (!"date" %in% names(samples)) samples[, date := as.Date(NA)]
  if (!"strat" %in% names(samples)) samples[, strat := NA_character_]
  if (!"time" %in% names(samples)) samples[, time := NA_integer_]
  if (!"type" %in% names(samples)) samples[, type := NA_character_]

  samples[]
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

  # Link predictions to dates and observations
  link_preds <- function(index) {
    target_obs <- dirty_obs[[index]][, idx := .N - 0:(.N - 1)]
    target_obs <- target_obs[idx < trunc_max]
    estimates <- recon_obs[dataset == index][, c("id", "dataset") := NULL]
    estimates <- estimates[, lapply(.SD, as.integer)]
    estimates <- estimates[, idx := .N - 0:(.N - 1)]
    if (!is.null(estimates$n_eff)) {
      estimates[, "n_eff" := NULL]
    }
    if (!is.null(estimates$Rhat)) {
      estimates[, "Rhat" := NULL]
    }

    # Merge predictions with date and observed confirm
    result <- data.table::merge.data.table(
      target_obs[, .(date, confirm, idx)],
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
  fit <- object$fit

  # Get the delay ID for this named delay
  id_var <- paste0("delay_id_", delay_name)
  delay_id <- stan_data[[id_var]]

  # Return NULL if delay doesn't exist (ID is 0 or NULL)
  if (is.null(delay_id) || delay_id == 0) {
    return(NULL)
  }

  # Get delay type groups to find indices for this delay
  types_groups <- stan_data$delay_types_groups
  if (is.null(types_groups)) {
    return(NULL)
  }

  # Extract posterior estimates if any parameters were estimated
  posterior <- NULL
  if (stan_data$delay_params_length > 0 && !is.null(fit)) {
    posterior <- extract_stan_param(fit, params = "delay_params")
  }

  # Get start and end indices for this delay type
  start_idx <- types_groups[delay_id]
  end_idx <- types_groups[delay_id + 1] - 1
  delay_indices <- seq(start_idx, end_idx)

  # Determine if parametric or nonparametric
  types_p <- stan_data$delay_types_p[delay_indices]

  # Each delay in the range could be parametric or nonparametric
  delay_list <- lapply(seq_along(delay_indices), function(i) {
    idx <- delay_indices[i]
    is_parametric <- types_p[i] == 1

    if (is_parametric) {
      # Get the parametric delay ID
      param_id <- stan_data$delay_types_id[idx]

      # Get distribution type
      dist_type <- dist_types()[stan_data$delay_dist[param_id] + 1]

      # Get parameter indices
      param_start <- stan_data$delay_params_groups[param_id]
      param_end <- stan_data$delay_params_groups[param_id + 1] - 1
      param_indices <- seq(param_start, param_end)

      # Get prior values
      params_prior_mean <- stan_data$delay_params_mean[param_indices]
      params_prior_sd <- stan_data$delay_params_sd[param_indices]

      # Get max delay
      dist_max <- stan_data$delay_max[param_id]

      # Create parameters - use posterior if estimated, prior/fixed otherwise
      param_names <- natural_params(dist_type)
      parameters <- lapply(seq_along(params_prior_mean), function(j) {
        param_idx <- param_indices[j]
        was_estimated <- params_prior_sd[j] > 0 && !is.na(params_prior_sd[j])

        if (was_estimated && !is.null(posterior)) {
          # Use posterior estimates
          post_mean <- round(posterior$mean[param_idx], 3)
          post_sd <- round(posterior$sd[param_idx], 3)
          Normal(post_mean, post_sd)
        } else if (params_prior_sd[j] == 0 || is.na(params_prior_sd[j])) {
          # Fixed value
          params_prior_mean[j]
        } else {
          # Prior (no fit available)
          Normal(params_prior_mean[j], params_prior_sd[j])
        }
      })
      names(parameters) <- param_names

      new_dist_spec(
        params = parameters,
        max = dist_max,
        distribution = dist_type
      )
    } else {
      # Nonparametric delay
      np_id <- stan_data$delay_types_id[idx]

      # Get PMF indices
      pmf_start <- stan_data$delay_np_pmf_groups[np_id]
      pmf_end <- stan_data$delay_np_pmf_groups[np_id + 1] - 1
      pmf <- stan_data$delay_np_pmf[seq(pmf_start, pmf_end)]

      NonParametric(pmf = pmf)
    }
  })

  # If single delay, return it directly; otherwise combine
  if (length(delay_list) == 1) {
    delay_list[[1]]
  } else {
    do.call(c, delay_list)
  }
}

#' @rdname get_delays
#' @export
get_delays.estimate_infections <- function(object, ...) {
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
get_delays.estimate_secondary <- function(object, ...) {
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
  # Extract estimated delay parameters from the posterior
  delay_params <- extract_stan_param(object$fit, params = "delay_params")
  params_mean <- round(delay_params$mean, 3)
  params_sd <- round(delay_params$sd, 3)

  # Get distribution info from Stan data
  dist_type <- dist_types()[object$args$delay_dist[1] + 1]
  dist_max <- object$args$delay_max[1]

  # Create Normal distributions for each parameter
  parameters <- purrr::map(seq_along(params_mean), function(id) {
    Normal(params_mean[id], params_sd[id])
  })
  names(parameters) <- natural_params(dist_type)

  # Create and return the dist_spec in a named list
  trunc_dist <- new_dist_spec(
    params = parameters,
    max = dist_max,
    distribution = dist_type
  )

  list(truncation = trunc_dist)
}
