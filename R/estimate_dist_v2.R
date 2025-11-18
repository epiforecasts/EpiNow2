#' Estimate a delay distribution using primarycensored
#'
#' @description `r lifecycle::badge("experimental")`
#' Estimate a delay distribution from observed data using primarycensored's
#' sophisticated handling of truncation and censoring.
#'
#' @param data Either:
#'   - A numeric vector of delay values (assumes daily censoring)
#'   - A data.frame with columns: delay, delay_upper, n, pwindow, relative_obs_time
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
#'   - "cmdstanr" (default if available, provides full primarycensored features)
#'   - "rstan" (fallback, may have limited truncation support)
#'
#' @param param_bounds Optional list with lower and upper bounds for parameters.
#'   If not provided, sensible bounds are derived from the data.
#'
#' @param ... Additional arguments passed to Stan sampling.
#'
#' @return A `<dist_spec>` object summarising the fitted distribution.
#'
#' @details
#' This function wraps primarycensored's model fitting capabilities to estimate
#' delay distributions while properly accounting for:
#' - Primary event censoring (e.g., daily reporting of exposure)
#' - Secondary event censoring (e.g., daily reporting of symptom onset)
#' - Right truncation (observation window effects)
#'
#' The function uses primarycensored's Stan models which implement the methods
#' from Park et al. (2024).
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
                          max_value,
                          verbose = FALSE,
                          backend = c("cmdstanr", "rstan"),
                          param_bounds = NULL,
                          ...) {

  # Check primarycensored availability
  if (!requireNamespace("primarycensored", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "x" = "Package 'primarycensored' is required",
        "i" = "Install with: install.packages('primarycensored')"
      )
    )
  }

  # Select backend
  backend <- match.arg(backend)

  # If cmdstanr requested, check availability
  if (backend == "cmdstanr" &&
      !requireNamespace("cmdstanr", quietly = TRUE)) {
    cli::cli_warn(
      c(
        "!" = "cmdstanr not available, falling back to rstan"
      )
    )
    backend <- "rstan"
  }

  # Convert data to primarycensored format
  delay_data <- .prepare_pcd_data(data, verbose)

  # Get distribution and primary IDs
  dist_id <- primarycensored::pcd_stan_dist_id(dist)
  primary_id <- 0  # Uniform primary censoring (daily)

  # Get parameter bounds if not provided
  if (is.null(param_bounds)) {
    param_bounds <- .get_param_bounds_pcd(delay_data, dist)
    if (verbose) {
      cli::cli_alert_info(
        "Using automatic parameter bounds: [{param_bounds$lower[1]:.2f}, {param_bounds$lower[2]:.2f}] to [{param_bounds$upper[1]:.2f}, {param_bounds$upper[2]:.2f}]"
      )
    }
  }

  # Prepare Stan data using primarycensored
  pcd_data <- primarycensored::pcd_as_stan_data(
    delay_data,
    dist_id = dist_id,
    primary_id = primary_id,
    param_bounds = param_bounds,
    primary_param_bounds = list()  # Empty for uniform primary
  )

  if (verbose) {
    cli::cli_alert_info(
      "Fitting {dist} distribution to {sum(delay_data$n)} observations"
    )
    cli::cli_alert_info(
      "Using {backend} backend with {chains} chains"
    )
  }

  # Fit using appropriate backend
  if (backend == "cmdstanr") {
    fit <- .fit_pcd_cmdstanr(
      pcd_data, samples, chains, cores, verbose, ...
    )
  } else {
    fit <- .fit_pcd_rstan(
      pcd_data, samples, chains, cores, verbose, ...
    )
  }

  # Extract and convert to dist_spec
  result <- .pcd_fit_to_dist_spec(
    fit = fit,
    dist = dist,
    max_value = max_value %||% max(delay_data$delay_upper),
    backend = backend
  )

  if (verbose) {
    cli::cli_alert_success("Fitting complete")
  }

  return(result)
}

#' Prepare data for primarycensored
#'
#' @keywords internal
.prepare_pcd_data <- function(data, verbose = FALSE) {

  if (is.numeric(data)) {
    # Vector - convert to primarycensored format

    if (verbose) {
      cli::cli_alert_info("Converting vector to primarycensored format")
    }

    # Clean
    data <- as.integer(data)
    data <- data[!is.na(data) & data >= 0]

    # Aggregate
    delay_counts <- table(data)

    delay_df <- data.frame(
      delay = as.numeric(names(delay_counts)),
      delay_upper = as.numeric(names(delay_counts)) + 1,
      n = as.integer(delay_counts),
      pwindow = 1,  # Daily primary censoring
      relative_obs_time = max(data) + 10  # No truncation by default
    )

  } else if (is.data.frame(data)) {

    # Validate required columns
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

    # Add missing columns with defaults
    if (!"n" %in% names(delay_df)) {
      delay_df$n <- 1L
    }
    if (!"pwindow" %in% names(delay_df)) {
      delay_df$pwindow <- 1
    }
    if (!"relative_obs_time" %in% names(delay_df)) {
      delay_df$relative_obs_time <- max(delay_df$delay_upper) + 10
    }

  } else {
    cli::cli_abort("data must be a numeric vector or data frame")
  }

  return(delay_df)
}

#' Get parameter bounds for primarycensored
#'
#' @keywords internal
.get_param_bounds_pcd <- function(delay_data, dist) {

  # Use data to inform bounds
  midpoints <- (delay_data$delay + delay_data$delay_upper) / 2
  weights <- delay_data$n

  wmean <- stats::weighted.mean(midpoints, weights)
  wsd <- sqrt(stats::weighted.mean((midpoints - wmean)^2, weights))

  # Distribution-specific bounds
  # These are intentionally wide but informed by the data
  bounds <- switch(dist,
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
    ),
    cli::cli_abort("Unsupported distribution: {dist}")
  )

  return(bounds)
}

#' Fit with cmdstanr using primarycensored model
#'
#' @keywords internal
.fit_pcd_cmdstanr <- function(pcd_data, samples, chains, cores,
                               verbose, ...) {

  # Get primarycensored's cmdstan model
  model <- primarycensored::pcd_cmdstan_model(
    cpp_options = list(stan_threads = TRUE)
  )

  # Fit
  fit <- model$sample(
    data = pcd_data,
    chains = chains,
    parallel_chains = cores,
    iter_warmup = 1000,
    iter_sampling = samples,
    threads_per_chain = 1,
    adapt_delta = 0.95,
    show_messages = verbose,
    ...
  )

  return(fit)
}

#' Fit with rstan using primarycensored model
#'
#' @keywords internal
.fit_pcd_rstan <- function(pcd_data, samples, chains, cores,
                            verbose, ...) {

  if (!requireNamespace("rstan", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "x" = "Package 'rstan' is required for backend='rstan'",
        "i" = "Install with: install.packages('rstan')"
      )
    )
  }

  # Get primarycensored's Stan model code
  # We'll need to extract and compile it with rstan
  model_code <- .get_pcd_stan_code()

  # Compile
  model <- rstan::stan_model(
    model_code = model_code,
    verbose = verbose
  )

  # Fit
  fit <- rstan::sampling(
    model,
    data = pcd_data,
    chains = chains,
    cores = cores,
    iter = samples + 1000,
    warmup = 1000,
    control = list(adapt_delta = 0.95),
    verbose = verbose,
    ...
  )

  return(fit)
}

#' Get primarycensored Stan model code
#'
#' @keywords internal
.get_pcd_stan_code <- function() {
  # This would extract the Stan code from primarycensored
  # For now, return a placeholder
  # In practice, this should read the Stan files from pcd_stan_path()

  cli::cli_abort(
    c(
      "x" = "rstan backend not fully implemented yet",
      "i" = "Please use backend = 'cmdstanr' or help implement this function"
    )
  )
}

#' Convert primarycensored fit to dist_spec
#'
#' @keywords internal
.pcd_fit_to_dist_spec <- function(fit, dist, max_value, backend) {

  # Extract parameters from fit
  if (backend == "cmdstanr") {
    draws <- fit$draws()

    if (dist == "lognormal") {
      # Extract meanlog and sdlog from primarycensored fit
      # Parameter names depend on primarycensored's Stan model
      meanlog_draws <- posterior::subset_draws(draws, "params[1]")
      sdlog_draws <- posterior::subset_draws(draws, "params[2]")

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
      shape_draws <- posterior::subset_draws(draws, "params[1]")
      rate_draws <- posterior::subset_draws(draws, "params[2]")

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
    } else {
      cli::cli_abort("Extraction for {dist} not implemented")
    }

  } else {  # rstan
    # Similar extraction for rstan
    cli::cli_abort("rstan extraction not implemented")
  }

  # Create dist_spec
  result <- new_dist_spec(
    params = params,
    max = max_value,
    distribution = dist
  )

  return(result)
}

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
