#' Format Posterior Samples
#'
#' @description `r lifecycle::badge("stable")`
#' Summaries posterior samples and adds additional custom variables.
#'
#' @param posterior_samples A list of posterior samples as returned by
#' [format_simulation_output()].
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
#' @description `r lifecycle::badge("stable")`
#' Formats simulation output from Stan models into structured data.tables with
#' dates. This is an internal function used by [simulate_infections()] and
#' [forecast_infections()] to process simulation results.
#'
#' This differs from [get_samples()] in that it's designed for simulation
#' outputs which have different array structures (especially with
#' `drop_length_1 = TRUE`) and need different date ranges for different
#' parameters.
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
#' @param merge if TRUE, merge samples into a single data.table using
#' rbindlist. If FALSE returns a list of samples by parameter.
#'
#' @inheritParams extract_samples
#' @return A list of `<data.frame>`'s each containing the simulated trajectories
#' of each parameter, or a single merged data.table if merge = TRUE.
#' @importFrom rstan extract
#' @importFrom data.table data.table
#' @keywords internal
format_simulation_output <- function(stan_fit, data, reported_dates,
                                     imputed_dates, reported_inf_dates,
                                     drop_length_1 = FALSE, merge = FALSE) {
  # extract sample from stan object
  samples <- extract_samples(stan_fit)

  ## drop initial length 1 dimensions if requested
  if (drop_length_1) {
    samples <- lapply(samples, function(x) {
      if (length(dim(x)) > 1 && dim(x)[1] == 1) dim(x) <- dim(x)[-1]
      x
    })
  }

  for (data_name in names(data)) {
    if (!(data_name %in% names(samples))) {
      samples[[data_name]] <- data[[data_name]]
    }
  }

  # construct reporting list
  out <- list()
  # report infections, and R
  out$infections <- extract_latent_state(
    "infections",
    samples,
    reported_inf_dates
  )
  out$infections <- out$infections[date >= min(reported_dates)]
  out$reported_cases <- extract_latent_state(
    "imputed_reports",
    samples,
    imputed_dates
  )
  if ("estimate_r" %in% names(data)) {
    if (data$estimate_r == 1) {
      out$R <- extract_latent_state(
        "R",
        samples,
        reported_dates
      )
      if (data$bp_n > 0) {
        out$breakpoints <- extract_latent_state(
          "bp_effects",
          samples,
          1:data$bp_n
        )
        out$breakpoints <- out$breakpoints[
          ,
          strat := date
        ][, c("time", "date") := NULL]
      }
    } else {
      out$R <- extract_latent_state(
        "gen_R",
        samples,
        reported_dates
      )
    }
  }
  out$growth_rate <- extract_latent_state(
    "r",
    samples,
    reported_dates[-1]
  )
  incomplete_dates <- unique(out$growth_rate[is.na(value), ][["date"]])
  out$growth_rate[date %in% incomplete_dates, value := NA]
  if (data$week_effect > 1) {
    out$day_of_week <- extract_latent_state(
      "day_of_week_simplex",
      samples,
      1:data$week_effect
    )
    out$day_of_week <- out$day_of_week[, value := value * data$week_effect]
    out$day_of_week <- out$day_of_week[, strat := date][
      ,
      c("time", "date") := NULL
    ]
  }
  if (data$delay_n_p > 0) {
    out$delay_params <- extract_latent_state(
      "delay_params", samples, seq_len(data$delay_params_length)
    )
    out$delay_params <-
      out$delay_params[, strat := as.character(time)][, time := NULL][
        ,
        date := NULL
      ]
  }
  # Auto-detect and extract all static parameters from params matrix
  all_params <- extract_parameters(samples, args = data)
  if (!is.null(all_params)) {
    # Get unique parameter names
    param_names <- unique(all_params$parameter)

    for (param in param_names) {
      result <- all_params[parameter == param]
      if (nrow(result) > 0) {
        out[[param]] <- result
      }
    }
  }
  out
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
#' @importFrom rlang %||%
#' @keywords internal
format_samples_with_dates <- function(raw_samples, args, observations) {
  # Reported dates cover the observation period plus any forecast horizon
  reported_dates <- seq(
    min(observations$date),
    max(observations$date) + args$horizon,
    by = "days"
  )
  # Full dates include the seeding period before observations
  dates <- seq(
    min(observations$date) - args$seeding_time,
    max(reported_dates),
    by = "days"
  )

  # Extract each parameter into a data.table
  out <- list()

  # Infections (for estimate_infections)
  infections <- extract_latent_state("infections", raw_samples, dates)
  if (!is.null(infections)) {
    out$infections <- infections[date >= min(reported_dates)]
  }

  # Reported cases (for estimate_infections)
  out$reported_cases <- extract_latent_state(
    "imputed_reports", raw_samples, reported_dates[args$imputed_times]
  )

  # R (reproduction number) - try R first, then gen_R
  out$R <- extract_latent_state("R", raw_samples, reported_dates)
  if (is.null(out$R)) {
    out$R <- extract_latent_state("gen_R", raw_samples, reported_dates)
  }

  # Breakpoints (if present in model)
  if (args$bp_n > 0) {
    breakpoints <- extract_latent_state("bp_effects", raw_samples, 1:args$bp_n)
    if (!is.null(breakpoints)) {
      out$breakpoints <- breakpoints[, strat := date][
        , c("time", "date") := NULL
      ]
    }
  }

  # Growth rate
  growth_rate <- extract_latent_state("r", raw_samples, reported_dates[-1])
  if (!is.null(growth_rate)) {
    incomplete_dates <- unique(growth_rate[is.na(value), ][["date"]])
    growth_rate[date %in% incomplete_dates, value := NA]
  }
  out$growth_rate <- growth_rate

  # Day of week effect
  if (args$week_effect > 1) {
    day_of_week <- extract_latent_state(
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
    out$delay_params <- extract_delays(raw_samples, args = args)
  }

  # Params matrix
  out$params <- extract_parameters(raw_samples, args = args)

  # Combine all parameters into single data.table
  # idcol adds 'variable' column from list names (infections, R, params, etc.)
  combined <- data.table::rbindlist(out, fill = TRUE, idcol = "variable")

  # Add strat column if missing
  if (is.null(combined$strat)) {
    combined <- combined[, strat := NA]
  }

  # Add type column based on horizon
  horizon <- args$horizon %||% 0
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
