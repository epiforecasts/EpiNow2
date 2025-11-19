#' Format Posterior Samples
#'
#' @description `r lifecycle::badge("stable")`
#' Summaries posterior samples and adds additional custom variables.
#'
#' @param posterior_samples A list of posterior samples as returned by
#' [format_samples_with_dates()].
#'
#' @param horizon Numeric, forecast horizon.
#'
#' @param shift Numeric, the shift to apply to estimates.
#'
#' @param burn_in Deprecated; this functionality is no longer available.
#'
#' @param start_date Deprecated; this functionality is no longer available.
#'
#' @inheritParams calc_summary_measures
#' @importFrom data.table fcase rbindlist
#' @importFrom lubridate days
#' @importFrom futile.logger flog.info
#' @return A list of samples and summarised posterior parameter estimates.
#' @keywords internal
format_fit <- function(posterior_samples, horizon, shift, burn_in, start_date,
                       CrIs) {
  if (!missing(burn_in)) {
    lifecycle::deprecate_stop(
      "1.8.0",
      "format_fit(burn_in)",
      detail = "This functionality is no longer available."
    )

  }
  if (!missing(start_date)) {
    lifecycle::deprecate_stop(
      "1.8.0",
      "format_fit(start_date)",
      detail = "This functionality is no longer available."
    )
  }
  format_out <- list()
  # bind all samples together
  format_out$samples <- data.table::rbindlist(
    posterior_samples,
    fill = TRUE, idcol = "variable"
  )

  if (is.null(format_out$samples$strat)) {
    format_out$samples <- format_out$samples[, strat := NA]
  }
  # add type based on horizon
  format_out$samples <- format_out$samples[
    ,
    type := data.table::fcase(
      date > (max(date, na.rm = TRUE) - horizon),
      "forecast",
      date > (max(date, na.rm = TRUE) - horizon - shift),
      "estimate based on partial data",
      is.na(date), NA_character_,
      default = "estimate"
    )
  ]

  # summarise samples
  format_out$summarised <- calc_summary_measures(format_out$samples,
    summarise_by = c("date", "variable", "strat", "type"),
    order_by = c("variable", "date"),
    CrIs = CrIs
  )
  format_out
}

#' Format Simulation Output from Stan
#'
#' @description `r lifecycle::badge("deprecated")`
#' This function has been deprecated. Use [format_samples_with_dates()] instead,
#' which provides a unified interface for both estimation and simulation outputs.
#'
#' @param data A list of the data supplied to the simulation.
#'
#' @param reported_dates A vector of dates to report estimates for.
#'
#' @param imputed_dates A vector of dates to report imputed reports for.
#'
#' @param reported_inf_dates A vector of dates to report infection estimates
#' for.
#'
#' @param drop_length_1 Logical; drop dimensions of length 1 in arrays extracted
#' from the stan fit. Used in simulations where there's only 1 realization.
#'
#' @param merge Deprecated parameter that was never implemented.
#'
#' @inheritParams extract_samples
#' @return A list of `<data.frame>`'s each containing the simulated trajectories
#' of each parameter.
#' @importFrom rstan extract
#' @importFrom data.table data.table
#' @keywords internal
format_simulation_output <- function(stan_fit, data, reported_dates,
                                     imputed_dates, reported_inf_dates,
                                     drop_length_1 = FALSE, merge = FALSE) {
  lifecycle::deprecate_warn(
    "1.8.0",
    "format_simulation_output()",
    "format_samples_with_dates()"
  )

  # Extract samples and apply drop_length_1
  raw_samples <- extract_samples(stan_fit)
  if (drop_length_1) {
    raw_samples <- lapply(raw_samples, function(x) {
      if (length(dim(x)) > 1 && dim(x)[1] == 1) dim(x) <- dim(x)[-1]
      x
    })
  }

  # Copy data fields into samples
  for (data_name in names(data)) {
    if (!(data_name %in% names(raw_samples))) {
      raw_samples[[data_name]] <- data[[data_name]]
    }
  }

  # Construct observations and args for format_samples_with_dates
  # Use the full date range from reported_inf_dates
  observations <- data.table(date = reported_inf_dates)

  # Calculate seeding_time from the date vectors
  seeding_time <- length(reported_inf_dates) - length(reported_dates)

  # Add imputed_times - these are the indices of imputed dates within reported dates
  data$imputed_times <- match(imputed_dates, reported_dates)
  data$seeding_time <- seeding_time

  # Call unified formatting function
  combined <- format_samples_with_dates(
    raw_samples = raw_samples,
    args = data,
    observations = observations
  )

  # Split back into list format for backward compatibility
  out <- split(combined, by = "variable", keep.by = FALSE)

  return(out)
}

#' Format raw Stan samples with dates and metadata
#'
#' @description Internal helper that extracts Stan parameters, adds dates to
#'   time-varying parameters, and combines into a single long-format data.table.
#'
#' @param raw_samples Raw samples from extract_samples()
#' @param args Model arguments (from object$args)
#' @param observations Observation data with dates
#'
#' @return A `data.table` in long format with dates and metadata
#' @keywords internal
format_samples_with_dates <- function(raw_samples, args, observations) {
  dates <- observations$date
  reported_dates <- dates[-(1:args$seeding_time)]

  # Extract each parameter into a data.table
  out <- list()

  # Infections (for estimate_infections)
  infections <- extract_parameter("infections", raw_samples, dates)
  if (!is.null(infections)) {
    out$infections <- infections[date >= min(reported_dates)]
  }

  # Reported cases (for estimate_infections)
  out$reported_cases <- extract_parameter(
    "imputed_reports", raw_samples, reported_dates[args$imputed_times]
  )

  # R (reproduction number) - try R first, then gen_R
  out$R <- extract_parameter("R", raw_samples, reported_dates)
  if (is.null(out$R)) {
    out$R <- extract_parameter("gen_R", raw_samples, reported_dates)
  }

  # Breakpoints (if present in model)
  if (args$bp_n > 0) {
    breakpoints <- extract_parameter("bp_effects", raw_samples, 1:args$bp_n)
    if (!is.null(breakpoints)) {
      out$breakpoints <- breakpoints[, strat := date][
        , c("time", "date") := NULL
      ]
    }
  }

  # Growth rate
  growth_rate <- extract_parameter("r", raw_samples, reported_dates[-1])
  if (!is.null(growth_rate)) {
    incomplete_dates <- unique(growth_rate[is.na(value), ][["date"]])
    growth_rate[date %in% incomplete_dates, value := NA]
  }
  out$growth_rate <- growth_rate

  # Day of week effect
  if (args$week_effect > 1) {
    day_of_week <- extract_parameter(
      "day_of_week_simplex", raw_samples, 1:args$week_effect
    )
    if (!is.null(day_of_week)) {
      day_of_week <- day_of_week[, value := value * args$week_effect]
      out$day_of_week <- day_of_week[, strat := date][
        , c("time", "date") := NULL
      ]
    }
  }

  # Delay parameters
  if (args$delay_params_length > 0) {
    delay_params <- extract_parameter(
      "delay_params", raw_samples, seq_len(args$delay_params_length)
    )
    if (!is.null(delay_params)) {
      out$delay_params <- delay_params[, strat := as.character(time)][
        , time := NULL
      ][, date := NULL]
    }
  }

  # Auto-detect and extract all static parameters from params matrix
  param_id_names <- names(raw_samples)[
    startsWith(names(raw_samples), "param_id_")
  ]
  param_names <- sub("^param_id_", "", param_id_names)

  for (param in param_names) {
    result <- extract_static_parameter(param, raw_samples)
    if (!is.null(result)) {
      # Use standard naming conventions
      param_name <- switch(param,
        "dispersion" = "reporting_overdispersion",
        "frac_obs" = "fraction_observed",
        param  # default: use param name as-is
      )
      out[[param_name]] <- result
    }
  }

  # Combine all parameters into single data.table
  combined <- data.table::rbindlist(out, fill = TRUE, idcol = "variable")

  # Add strat column if missing
  if (is.null(combined$strat)) {
    combined <- combined[, strat := NA]
  }

  # Add type column based on horizon
  horizon <- if (is.null(args$horizon)) 0 else args$horizon
  shift <- args$seeding_time

  combined <- combined[
    ,
    type := data.table::fcase(
      date > (max(date, na.rm = TRUE) - horizon),
      "forecast",
      date > (max(date, na.rm = TRUE) - horizon - shift),
      "estimate based on partial data",
      is.na(date), NA_character_,
      default = "estimate"
    )
  ]

  combined[]
}
