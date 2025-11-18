#' Estimate a delay distribution
#'
#' @description `r lifecycle::badge("experimental")`
#' Estimate a delay distribution from observed data with support for
#' interval censoring using Stan/MCMC inference.
#'
#' @param data Either:
#'   - A numeric vector of delay values (assumes daily censoring)
#'   - A data.frame with columns: delay, delay_upper, n (counts)
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
#' @param backend Character, which Stan backend to use. Options:
#'   - "rstan" (default, more compatible)
#'   - "cmdstanr" (faster, requires cmdstanr installation)
#'
#' @param ... Additional arguments passed to Stan sampling.
#'
#' @return A `<dist_spec>` object summarising the fitted distribution.
#'
#' @details
#' This function provides an interface for estimating delay distributions
#' that properly accounts for interval censoring in the observed data.
#'
#' The function fits a Bayesian model using Stan to estimate distribution
#' parameters while accounting for:
#' - Daily censoring of observations (values represent intervals)
#' - Uncertainty in parameter estimates via posterior distribution
#'
#' For the lognormal distribution, the function returns the mean and standard
#' deviation of the log-transformed delays. For gamma, it returns shape and
#' rate parameters.
#'
#' @export
#' @examples
#' \donttest{
#' # Fit lognormal distribution to simulated delays
#' delays <- rlnorm(100, log(5), 0.5)
#' result <- estimate_dist(delays, dist = "lognormal", samples = 1000)
#' result
#' }
estimate_dist <- function(data,
                          dist = "lognormal",
                          samples = 2000,
                          chains = 4,
                          cores = 1,
                          max_value,
                          verbose = FALSE,
                          backend = "rstan",
                          ...) {

  # Input validation
  if (!dist %in% c("lognormal", "gamma", "weibull")) {
    cli::cli_abort(
      c(
        "x" = "Unsupported distribution: {dist}",
        "i" = "Supported: lognormal, gamma, weibull"
      )
    )
  }

  # Convert data to interval format
  delay_data <- .prepare_interval_data(data, verbose)

  # Get distribution ID
  dist_id <- switch(dist,
    "lognormal" = 1,
    "gamma" = 2,
    "weibull" = 3
  )

  # Get sensible parameter bounds for this distribution
  param_bounds <- .get_param_bounds(delay_data, dist)

  # Prepare Stan data
  stan_data <- list(
    N = nrow(delay_data),
    delay = delay_data$delay,
    delay_upper = delay_data$delay_upper,
    n = delay_data$n,
    dist_id = dist_id,
    D = max(delay_data$delay_upper) + 10,  # Truncation time
    param_lower = param_bounds$lower,
    param_upper = param_bounds$upper
  )

  if (verbose) {
    cli::cli_alert_info(
      "Fitting {dist} distribution to {sum(delay_data$n)} observations"
    )
    cli::cli_alert_info(
      "Using {chains} chains with {samples} samples each"
    )
  }

  # Compile and fit model
  if (backend == "cmdstanr") {
    fit <- .fit_with_cmdstanr(
      stan_data, samples, chains, cores, verbose, ...
    )
  } else {
    fit <- .fit_with_rstan(
      stan_data, samples, chains, cores, verbose, ...
    )
  }

  # Extract posterior and convert to dist_spec
  result <- .stan_fit_to_dist_spec(
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

#' Prepare data in interval format
#'
#' @param data Input data (vector or data frame)
#' @param verbose Logical, print messages?
#' @return Data frame with delay, delay_upper, n
#' @keywords internal
.prepare_interval_data <- function(data, verbose = FALSE) {

  if (is.numeric(data)) {
    # Vector of delay values - assume daily censoring
    # Observed value X means true delay in [X, X+1)

    if (verbose) {
      cli::cli_alert_info("Converting vector to interval-censored format")
    }

    # Clean data
    data <- as.integer(data)
    data <- data[!is.na(data) & data >= 0]

    # Create frequency table
    delay_counts <- table(data)

    delay_df <- data.frame(
      delay = as.numeric(names(delay_counts)),
      delay_upper = as.numeric(names(delay_counts)) + 1,
      n = as.integer(delay_counts)
    )

  } else if (is.data.frame(data)) {

    required_cols <- c("delay", "delay_upper")

    if (!all(required_cols %in% names(data))) {
      cli::cli_abort(
        c(
          "x" = "Data frame must have columns: delay, delay_upper",
          "i" = "Found: {paste(names(data), collapse = ', ')}"
        )
      )
    }

    delay_df <- data

    # Add n column if missing
    if (!"n" %in% names(delay_df)) {
      delay_df$n <- 1L
    }

    delay_df$n <- as.integer(delay_df$n)

  } else {
    cli::cli_abort("data must be a numeric vector or data frame")
  }

  return(delay_df)
}

#' Get sensible parameter bounds for a distribution
#'
#' @param delay_data Data frame with delay observations
#' @param dist Distribution name
#' @return List with lower and upper bounds
#' @keywords internal
.get_param_bounds <- function(delay_data, dist) {

  # Use midpoints for rough estimation
  midpoints <- (delay_data$delay + delay_data$delay_upper) / 2
  weights <- delay_data$n

  wmean <- stats::weighted.mean(midpoints, weights)
  wsd <- sqrt(stats::weighted.mean((midpoints - wmean)^2, weights))

  # Distribution-specific bounds
  # These are wide enough to be uninformative but constrained enough
  # to ensure numerical stability

  bounds <- switch(dist,
    "lognormal" = list(
      # meanlog can be any real number, sdlog must be positive
      # Use data to inform reasonable ranges
      lower = c(log(wmean) - 5, 0.01),
      upper = c(log(wmean) + 5, 10)
    ),
    "gamma" = list(
      # Both shape and rate must be positive
      # Use method of moments for rough bounds
      lower = c(0.01, 0.001),
      upper = c(100, 10)
    ),
    "weibull" = list(
      # Both shape and scale must be positive
      lower = c(0.1, 0.1),
      upper = c(10, max(midpoints) * 3)
    )
  )

  return(bounds)
}

#' Fit model with rstan using primarycensored functions
#'
#' @keywords internal
.fit_with_rstan <- function(stan_data, samples, chains, cores,
                             verbose, ...) {

  if (!requireNamespace("rstan", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "x" = "Package 'rstan' is required for backend='rstan'",
        "i" = "Install with: install.packages('rstan')"
      )
    )
  }

  # Get primarycensored Stan functions path for include
  if (requireNamespace("primarycensored", quietly = TRUE)) {
    pcd_path <- primarycensored::pcd_stan_path()
    use_pcd <- TRUE

    if (verbose) {
      cli::cli_alert_info(
        "Using primarycensored Stan functions from {pcd_path}"
      )
    }
  } else {
    use_pcd <- FALSE
    if (verbose) {
      cli::cli_alert_warn(
        "primarycensored not available, using simplified model"
      )
    }
  }

  # Get the model file
  model_file <- system.file(
    "stan",
    if (use_pcd) "estimate_dist_pcd.stan" else "estimate_dist.stan",
    package = "EpiNow2"
  )

  # Compile with primarycensored include path if available
  if (use_pcd) {
    # Compile with include paths to primarycensored
    model <- rstan::stan_model(
      file = model_file,
      include_paths = pcd_path,
      verbose = verbose
    )
  } else {
    model <- epinow2_stan_model("rstan", "estimate_dist")
  }

  # Sampling arguments
  stan_args <- create_stan_args(
    stan = stan_opts(
      model,
      samples = samples,
      warmup = 1000,
      control = list(adapt_delta = 0.95),
      chains = chains,
      cores = cores
    ),
    data = stan_data,
    verbose = verbose,
    model = "estimate_dist"
  )

  # Fit
  fit <- fit_model(stan_args, id = "estimate_dist")

  return(fit)
}

#' Fit model with cmdstanr
#'
#' @keywords internal
.fit_with_cmdstanr <- function(stan_data, samples, chains, cores,
                                verbose, ...) {

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "x" = "Package 'cmdstanr' is required for backend='cmdstanr'",
        "i" = "Install from: https://mc-stan.org/cmdstanr/"
      )
    )
  }

  # Get model file path
  model_path <- system.file(
    "stan", "estimate_dist.stan",
    package = "EpiNow2"
  )

  # Compile model
  model <- cmdstanr::cmdstan_model(model_path)

  # Fit
  fit <- model$sample(
    data = stan_data,
    chains = chains,
    parallel_chains = cores,
    iter_warmup = 1000,
    iter_sampling = samples,
    adapt_delta = 0.95,
    show_messages = verbose,
    ...
  )

  return(fit)
}

#' Convert Stan fit to dist_spec
#'
#' @keywords internal
.stan_fit_to_dist_spec <- function(fit, dist, max_value, backend) {

  # Extract posterior samples
  if (backend == "rstan") {
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
