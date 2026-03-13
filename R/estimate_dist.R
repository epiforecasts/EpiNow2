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
#'
#' @param priors A list of `<dist_spec>` objects specifying priors for the
#'   distribution parameters. Names must match the parameters of the chosen
#'   distribution. Defaults depend on `dist`:
#'   - lognormal: `list(meanlog = Normal(1, 1), sdlog = Normal(0.5, 0.5))`
#'   - gamma: `list(shape = Normal(2, 2), rate = Normal(0.5, 0.5))`
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
#' # Fit lognormal distribution with default priors
#' delays <- rlnorm(100, log(5), 0.5)
#' result <- estimate_dist(delays, dist = "lognormal")
#'
#' # Fit with custom priors
#' result <- estimate_dist(
#'   delays,
#'   dist = "lognormal",
#'   priors = list(
#'     meanlog = Normal(1.5, 0.5),
#'     sdlog = Normal(0.5, 0.25)
#'   )
#' )
#' }
estimate_dist <- function(data,
                          dist = "lognormal",
                          priors = if (dist == "lognormal") {
                            list(
                              meanlog = Normal(1, 1),
                              sdlog = Normal(0.5, 0.5)
                            )
                          } else {
                            list(
                              shape = Normal(2, 2),
                              rate = Normal(0.5, 0.5)
                            )
                          },
                          stan = stan_opts(),
                          max_value = NULL,
                          truncation_time = NULL,
                          verbose = FALSE) {

  # Input validation
  if (!dist %in% c("lognormal", "gamma")) {
    cli::cli_abort(
      c(
        "x" = "Unsupported distribution: {dist}",
        "i" = "Supported: lognormal, gamma"
      )
    )
  }

  # Convert data to proper format
  delay_data <- .prepare_delay_intervals(data, verbose)

  # Get distribution ID (primarycensored convention)
  dist_id <- switch(dist,
    "lognormal" = 1L,
    "gamma" = 2L
  )

  # Create params list using make_param in canonical order
  param_names <- switch(dist,
    "lognormal" = c("meanlog", "sdlog"),
    "gamma" = c("shape", "rate")
  )

  # Validate prior names
  if (!setequal(names(priors), param_names)) {
    cli::cli_abort(
      c(
        "x" = "Invalid prior names for {dist} distribution",
        "i" = "Expected: {paste(param_names, collapse = ', ')};
              got: {paste(names(priors), collapse = ', ')}"
      )
    )
  }

  lbounds <- lower_bounds(dist)
  params <- lapply(param_names, function(name) {
    make_param(name, priors[[name]], lower_bound = lbounds[[name]])
  })

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
    primary_params = numeric(0)  # Empty array
  )

  # Add params using standard EpiNow2 infrastructure
  stan_data <- c(stan_data, create_stan_params(params))

  if (verbose) {
    cli::cli_alert_info(
      "Fitting {dist} to {sum(stan_data$n_obs)} observations using
      {stan$backend %||% 'rstan'}"
    )
  }

  # Compute initial values based on MoM estimates
  midpoints <- (delay_data$delay + delay_data$delay_upper) / 2
  obs_weights <- delay_data$n
  wmean <- stats::weighted.mean(midpoints, obs_weights)
  wvar <- stats::weighted.mean((midpoints - wmean)^2, obs_weights)

  init_params <- switch(dist,
    "lognormal" = c(log(wmean), sqrt(log(1 + wvar / wmean^2))),
    "gamma" = c(wmean^2 / wvar, wmean / wvar)
  )
  # Ensure init is within bounds
  init_params <- pmax(init_params, stan_data$params_lower + 0.01)
  init_params <- pmin(init_params, stan_data$params_upper - 0.01)
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

#' Extract parameters and convert to dist_spec
#'
#' @keywords internal
.extract_to_dist_spec <- function(fit, dist, max_value) {
  # Extract raw params - naming handled here based on distribution
  samples <- extract_samples(fit, pars = "params")

  param_names <- switch(dist,
    "lognormal" = c("meanlog", "sdlog"),
    "gamma" = c("shape", "rate")
  )

  # samples$params is a matrix with columns for each parameter
  params <- lapply(seq_along(param_names), function(i) {
    Normal(mean = mean(samples$params[, i]), sd = sd(samples$params[, i]))
  })
  names(params) <- param_names

  new_dist_spec(params = params, max = max_value, distribution = dist)
}
