#' Estimate a delay distribution using primarycensored
#'
#' @description `r lifecycle::badge("experimental")`
#' Estimate a delay distribution from observed data using primarycensored's
#' sophisticated handling of truncation and censoring. Works with rstan
#' (no cmdstanr dependency).
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
#' @param samples Numeric, number of posterior samples. Defaults to 2000.
#'
#' @param chains Numeric, number of MCMC chains. Defaults to 4.
#'
#' @param cores Numeric, number of cores to use. Defaults to 1.
#'
#' @param max_value Numeric, maximum delay value for PMF. If not provided,
#'   inferred from data.
#'
#' @param verbose Logical, print progress messages? Defaults to FALSE.
#'
#' @param backend Character, which Stan backend to use:
#'   - "rstan" (default, uses primarycensored Stan functions)
#'   - "cmdstanr" (if available)
#'
#' @param param_bounds Optional list with lower and upper bounds for parameters.
#'   If not provided, sensible bounds are derived from the data.
#'
#' @param ... Additional arguments passed to Stan sampling.
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
#' @export
#' @examples
#' \donttest{
#' # Fit lognormal distribution
#' delays <- rlnorm(100, log(5), 0.5)
#' result <- estimate_dist(delays, dist = "lognormal", samples = 1000)
#' }
estimate_dist <- function(data,
                          dist = "lognormal",
                          samples = 2000,
                          chains = 4,
                          cores = 1,
                          max_value = NULL,
                          verbose = FALSE,
                          backend = c("rstan", "cmdstanr"),
                          param_bounds = NULL,
                          ...) {

  # Select backend
  backend <- match.arg(backend)

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
      lower <- paste0("[", round(param_bounds$lower, 2), "]", collapse = ", ")
      upper <- paste0("[", round(param_bounds$upper, 2), "]", collapse = ", ")
      cli::cli_alert_info("Parameter bounds: {lower} to {upper}")
    }
  }

  # Prepare Stan data
  stan_data <- list(
    n = nrow(delay_data),
    delay = as.integer(delay_data$delay),
    delay_upper = delay_data$delay_upper,
    n_obs = as.integer(delay_data$n),
    pwindow = 1.0,  # Daily primary censoring
    D = max(delay_data$delay_upper) + 10,  # Truncation time
    dist_id = dist_id,
    primary_id = 1L,  # Uniform primary censoring
    n_primary_params = 0L,  # No primary params for uniform
    primary_params = numeric(0),  # Empty array
    param_lower = param_bounds$lower,
    param_upper = param_bounds$upper
  )

  if (verbose) {
    cli::cli_alert_info(
      "Fitting {dist} to {sum(stan_data$n_obs)} observations using {backend}"
    )
  }

  # Fit
  if (backend == "cmdstanr") {
    fit <- .fit_with_cmdstanr_pcd(
      stan_data, samples, chains, cores, verbose, ...
    )
  } else {
    fit <- .fit_with_rstan_pcd(
      stan_data, samples, chains, cores, verbose, ...
    )
  }

  # Extract and convert to dist_spec
  result <- .extract_to_dist_spec(
    fit = fit,
    dist = dist,
    max_value = max_value %||% max(delay_data$delay_upper),
    backend = backend
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

#' Get automatic parameter bounds
#'
#' @keywords internal
.get_param_bounds_auto <- function(delay_data, dist) {
  # Use data to inform bounds
  midpoints <- (delay_data$delay + delay_data$delay_upper) / 2
  obs_weights <- delay_data$n

  wmean <- stats::weighted.mean(midpoints, obs_weights)

  # Distribution-specific bounds
  switch(dist,
    "lognormal" = list(
      lower = c(log(max(0.1, wmean / 10)), 0.01),
      upper = c(log(wmean * 10), 10)
    ),
    "gamma" = list(
      lower = c(0.01, 0.001),
      upper = c(100, 10)
    ),
    "weibull" = list(
      lower = c(0.1, 0.1),
      upper = c(10, max(midpoints) * 3)
    )
  )
}

#' Fit with rstan using pre-compiled estimate_dist model
#'
#' @keywords internal
.fit_with_rstan_pcd <- function(stan_data, samples, chains, cores,
                                verbose, ...) {
  # Get pre-compiled model
  model <- stanmodels[["estimate_dist"]]

  # Fit
  rstan::sampling(
    model,
    data = stan_data,
    chains = chains,
    cores = cores,
    iter = samples + 1000,
    warmup = 1000,
    control = list(adapt_delta = 0.95),
    verbose = verbose,
    ...
  )
}

#' Fit with cmdstanr
#'
#' @keywords internal
.fit_with_cmdstanr_pcd <- function(stan_data, samples, chains, cores,
                                   verbose, ...) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "x" = "cmdstanr not available",
        "i" = "Use backend='rstan' or install cmdstanr"
      )
    )
  }

  # Get Stan model file and include path for vendored primarycensored
  model_file <- system.file(
    "stan", "estimate_dist.stan",
    package = "EpiNow2"
  )
  include_path <- system.file("stan", package = "EpiNow2")

  # Compile
  model <- cmdstanr::cmdstan_model(
    model_file,
    include_paths = include_path
  )

  # Fit
  model$sample(
    data = stan_data,
    chains = chains,
    parallel_chains = cores,
    iter_warmup = 1000,
    iter_sampling = samples,
    adapt_delta = 0.95,
    show_messages = verbose,
    ...
  )
}

#' Extract parameters and convert to dist_spec
#'
#' @keywords internal
.extract_to_dist_spec <- function(fit, dist, max_value, backend) {

  if (backend == "rstan") {
    # Extract from rstan fit
    samples <- rstan::extract(fit)

    if (dist == "lognormal") {
      params <- list(
        meanlog = Normal(
          mean = mean(samples$meanlog),
          sd = sd(samples$meanlog)
        ),
        sdlog = Normal(
          mean = mean(samples$sdlog),
          sd = sd(samples$sdlog)
        )
      )
    } else if (dist == "gamma") {
      params <- list(
        shape = Normal(
          mean = mean(samples$shape),
          sd = sd(samples$shape)
        ),
        rate = Normal(
          mean = mean(samples$rate),
          sd = sd(samples$rate)
        )
      )
    } else if (dist == "weibull") {
      params <- list(
        shape = Normal(
          mean = mean(samples$shape),
          sd = sd(samples$shape)
        ),
        scale = Normal(
          mean = mean(samples$scale),
          sd = sd(samples$scale)
        )
      )
    }

  } else {  # cmdstanr
    draws <- fit$draws()

    if (dist == "lognormal") {
      meanlog_draws <- posterior::subset_draws(draws, "meanlog")
      sdlog_draws <- posterior::subset_draws(draws, "sdlog")

      params <- list(
        meanlog = Normal(
          mean = mean(meanlog_draws),
          sd = sd(meanlog_draws)
        ),
        sdlog = Normal(
          mean = mean(sdlog_draws),
          sd = sd(sdlog_draws)
        )
      )
    } else if (dist == "gamma") {
      shape_draws <- posterior::subset_draws(draws, "shape")
      rate_draws <- posterior::subset_draws(draws, "rate")

      params <- list(
        shape = Normal(
          mean = mean(shape_draws),
          sd = sd(shape_draws)
        ),
        rate = Normal(
          mean = mean(rate_draws),
          sd = sd(rate_draws)
        )
      )
    } else if (dist == "weibull") {
      shape_draws <- posterior::subset_draws(draws, "shape")
      scale_draws <- posterior::subset_draws(draws, "scale")

      params <- list(
        shape = Normal(
          mean = mean(shape_draws),
          sd = sd(shape_draws)
        ),
        scale = Normal(
          mean = mean(scale_draws),
          sd = sd(scale_draws)
        )
      )
    }
  }

  # Create dist_spec
  new_dist_spec(
    params = params,
    max = max_value,
    distribution = dist
  )
}
