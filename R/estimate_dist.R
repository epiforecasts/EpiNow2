#' Estimate a delay distribution using primarycensored
#'
#' @description `r lifecycle::badge("experimental")`
#' Estimate a delay distribution from observed data using primarycensored's
#' sophisticated handling of truncation and censoring.
#'
#' @param data Either:
#'   - A numeric vector of delay values (assumes daily censoring)
#'   - A data.frame with columns: delay, delay_upper, optionally n
#'
#' @param dist Character string, which distribution to fit. Supported:
#'   - "lognormal" (default)
#'   - "gamma"
#'   - "weibull"
#'
#' @param max_value Numeric, maximum delay value for PMF. If not provided,
#'   inferred from data.
#'
#' @param truncation_time Numeric, the maximum observation time (right
#'   truncation point). This should match the truncation applied during data
#'   collection. If not provided, uses `max(delay_upper) + 10` which may
#'   underestimate the effect of truncation.
#'
#' @param verbose Logical, print progress messages? Defaults to FALSE.
#'
#' @param param_bounds Optional list with lower and upper bounds for parameters.
#'   If not provided, sensible bounds are derived from the data.
#'
#' @return A `<dist_spec>` object summarising the fitted distribution.
#'
#' @details
#' This function uses primarycensored's Stan functions to estimate
#' delay distributions while properly accounting for:
#' - Primary event censoring (e.g., daily reporting of exposure)
#' - Secondary event censoring (e.g., daily reporting of symptom onset)
#' - Right truncation (observation window effects)
#'
#' The primarycensored Stan functions are vendored (included in the package),
#' so the model is pre-compiled and runs without needing primarycensored at
#' runtime.
#'
#' @inheritParams estimate_infections
#' @export
#' @examples
#' \donttest{
#' # Fit lognormal distribution
#' delays <- rlnorm(100, log(5), 0.5)
#' result <- estimate_dist(delays, dist = "lognormal")
#' }
estimate_dist <- function(data,
                          dist = "lognormal",
                          stan = stan_opts(adapt_delta = 0.95),
                          max_value = NULL,
                          truncation_time = NULL,
                          verbose = FALSE,
                          param_bounds = NULL) {

  # Input validation
  if (!dist %in% c("lognormal", "gamma", "weibull")) {
    cli::cli_abort(
      c(
        "x" = "Unsupported distribution: {dist}",
        "i" = "Supported: lognormal, gamma, weibull"
      )
    )
  }

  # Convert data to proper format
  delay_data <- .prepare_delay_intervals(data, verbose)

  # Get distribution ID (primarycensored convention)
  dist_id <- switch(dist,
    "lognormal" = 1L,
    "gamma" = 2L,
    "weibull" = 3L
  )

  # Get parameter bounds
  if (is.null(param_bounds)) {
    param_bounds <- .get_param_bounds_auto(delay_data, dist)
    if (verbose) {
      # nolint start: object_usage_linter
      lower <- paste0("[", round(param_bounds$lower, 2), "]", collapse = ", ")
      upper <- paste0("[", round(param_bounds$upper, 2), "]", collapse = ", ")
      # nolint end
      cli::cli_alert_info("Parameter bounds: {lower} to {upper}")
    }
  }

  # Prepare Stan data
  d_inferred <- max(delay_data$delay_upper) + 10
  stan_data <- list(
    n = nrow(delay_data),
    delay = as.integer(delay_data$delay),
    delay_upper = delay_data$delay_upper,
    n_obs = as.integer(delay_data$n),
    pwindow = 1.0,  # Daily primary censoring
    D = truncation_time %||% d_inferred,  # Truncation time
    dist_id = dist_id,
    primary_id = 1L,  # Uniform primary censoring
    n_primary_params = 0L,  # No primary params for uniform
    primary_params = numeric(0),  # Empty array
    # Standard EpiNow2 params interface
    n_params_variable = 2L,
    n_params_fixed = 0L,
    params_lower = param_bounds$lower,
    params_upper = param_bounds$upper,
    params_fixed_lookup = array(c(0L, 0L)),
    params_variable_lookup = array(c(1L, 2L)),
    params_value = numeric(0),
    prior_dist = array(param_bounds$prior_dist),
    prior_dist_params_length = 4L,
    prior_dist_params = array(param_bounds$prior_dist_params)
  )

  if (verbose) {
    # nolint start: object_usage_linter
    backend <- stan$backend %||% "rstan"
    # nolint end
    cli::cli_alert_info(
      "Fitting {dist} to {sum(stan_data$n_obs)} observations using {backend}"
    )
  }

  # Compute initial values based on MoM estimates
  midpoints <- (delay_data$delay + delay_data$delay_upper) / 2
  obs_weights <- delay_data$n
  wmean <- stats::weighted.mean(midpoints, obs_weights)
  wvar <- stats::weighted.mean((midpoints - wmean)^2, obs_weights)

  init_params <- switch(dist,
    "lognormal" = c(log(wmean), sqrt(log(1 + wvar / wmean^2))),
    "gamma" = c(wmean^2 / wvar, wmean / wvar),
    "weibull" = c(1.0, wmean)
  )
  # Ensure init is within bounds
  init_params <- pmax(init_params, param_bounds$lower + 0.01)
  init_params <- pmin(init_params, param_bounds$upper - 0.01)
  init_fn <- function() list(params = init_params)

  # Create stan args and fit using shared infrastructure
  stan_args <- create_stan_args(
    stan = stan, data = stan_data, init = init_fn, model = "estimate_dist",
    verbose = verbose
  )
  fit <- fit_model(stan_args, id = "estimate_dist")

  # Extract and convert to dist_spec
  result <- .extract_to_dist_spec(
    fit = fit,
    dist = dist,
    max_value = max_value %||% max(delay_data$delay_upper)
  )

  if (verbose) {
    cli::cli_alert_success("Fitting complete")
  }

  result
}

#' Prepare delay data as intervals
#'
#' @keywords internal
.prepare_delay_intervals <- function(data, verbose = FALSE) {

  if (is.numeric(data)) {
    # Vector - convert to intervals

    if (verbose) {
      cli::cli_alert_info("Converting vector to interval format")
    }

    # Clean
    data <- as.integer(data)
    data <- data[!is.na(data) & data >= 0]

    # Aggregate
    delay_counts <- table(data)

    delay_df <- data.frame(
      delay = as.numeric(names(delay_counts)),
      delay_upper = as.numeric(names(delay_counts)) + 1,
      n = as.integer(delay_counts)
    )

  } else if (is.data.frame(data)) {

    required <- c("delay", "delay_upper")
    if (!all(required %in% names(data))) {
      cli::cli_abort(
        c(
          "x" = "Data frame must have columns: delay, delay_upper",
          "i" = "Found: {paste(names(data), collapse = ', ')}"
        )
      )
    }

    delay_df <- data

    if (!"n" %in% names(delay_df)) {
      delay_df$n <- 1L
    }

  } else {
    cli::cli_abort("data must be a numeric vector or data frame")
  }

  delay_df
}

#' Get automatic parameter bounds and priors
#'
#' @keywords internal
.get_param_bounds_auto <- function(delay_data, dist) {

  # Use data to inform bounds and priors
  midpoints <- (delay_data$delay + delay_data$delay_upper) / 2
  obs_weights <- delay_data$n

  wmean <- stats::weighted.mean(midpoints, obs_weights)
  wvar <- stats::weighted.mean((midpoints - wmean)^2, obs_weights)
  wsd <- sqrt(wvar)

  # Prior dist codes: 0 = lognormal, 1 = gamma, 2 = normal
  # Distribution-specific bounds and priors
  switch(dist,
    "lognormal" = list(
      lower = c(log(max(0.1, wmean / 10)), 0.01),
      upper = c(log(wmean * 10), 10),
      prior_dist = c(2L, 2L),  # normal priors for both
      prior_dist_params = c(log(wmean), 1.0, 0.5, 0.5)
    ),
    "gamma" = {
      # Method of moments estimates
      shape_mom <- wmean^2 / wvar
      rate_mom <- wmean / wvar
      # Use gamma priors with shape=2 (weakly informative) and rate based on MoM
      list(
        lower = c(0.01, 0.01),
        upper = c(50, 20),
        prior_dist = c(1L, 1L),  # gamma priors (better for positive params)
        # gamma(shape=2, rate) has mode at 1/rate, mean at 2/rate
        prior_dist_params = c(2, 2 / shape_mom, 2, 2 / rate_mom)
      )
    },
    "weibull" = list(
      lower = c(0.1, 0.1),
      upper = c(10, max(midpoints) * 3),
      prior_dist = c(2L, 2L),  # normal priors
      prior_dist_params = c(1.0, 1.0, wmean, wsd)
    )
  )
}

#' Extract parameters and convert to dist_spec
#'
#' @keywords internal
.extract_to_dist_spec <- function(fit, dist, max_value) {
  param_names <- switch(dist,
    "lognormal" = c("meanlog", "sdlog"),
    "gamma" = c("shape", "rate"),
    "weibull" = c("shape", "scale")
  )

  samples <- extract_samples(fit, pars = param_names)

  params <- lapply(stats::setNames(param_names, param_names), function(name) {
    Normal(mean = mean(samples[[name]]), sd = sd(samples[[name]]))
  })

  new_dist_spec(params = params, max = max_value, distribution = dist)
}
