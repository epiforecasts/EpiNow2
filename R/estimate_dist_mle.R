#' Estimate a delay distribution using MLE (EXPERIMENTAL)
#'
#' @description `r lifecycle::badge("experimental")`
#' Estimate a delay distribution from observed data using maximum likelihood
#' estimation via the primarycensored package's fitdistdoublecens() function.
#'
#' @param data Either:
#'   - A numeric vector of delay values (for simple case)
#'   - A data.frame with columns: delay_lower, delay_upper, n (counts)
#'
#' @param dist Character string, which distribution to fit. Supported options:
#'   - "lognormal" (default) - maps to "lnorm" in fitdistrplus
#'   - "gamma"
#'   - "weibull"
#'   - "exponential" - maps to "exp"
#'
#' @param max_value Numeric, maximum delay value. If not provided, inferred
#'   from data.
#'
#' @param verbose Logical, print progress messages? Defaults to FALSE.
#'
#' @param ... Additional arguments passed to fitdistdoublecens().
#'
#' @return A `<dist_spec>` object summarising the fitted distribution.
#'
#' @details
#' This function provides a simple interface using maximum likelihood estimation
#' (MLE) to fit delay distributions that account for interval censoring.
#'
#' Uses the primarycensored::fitdistdoublecens() function which wraps
#' fitdistrplus for doubly censored data.
#'
#' @export
estimate_dist_mle <- function(data,
                              dist = "lognormal",
                              max_value,
                              verbose = FALSE,
                              ...) {

  # Check dependencies
  if (!requireNamespace("primarycensored", quietly = TRUE)) {
    stop(
      "Package 'primarycensored' is required. ",
      "Install it with: install.packages('primarycensored')"
    )
  }

  if (!requireNamespace("fitdistrplus", quietly = TRUE)) {
    stop(
      "Package 'fitdistrplus' is required. ",
      "Install it with: install.packages('fitdistrplus')"
    )
  }

  # Map EpiNow2 distribution names to fitdistrplus names
  fitdistr_dist <- switch(dist,
    "lognormal" = "lnorm",
    "gamma" = "gamma",
    "weibull" = "weibull",
    "exponential" = "exp",
    stop("Unsupported distribution: ", dist)
  )

  # Convert data to required format
  delay_data <- .prepare_delay_data_for_mle(data, verbose)

  # Get starting values based on distribution
  start_params <- .get_start_params(delay_data, fitdistr_dist)

  if (verbose) {
    cli::cli_alert_info(
      "Fitting {dist} distribution using MLE with {nrow(delay_data)} observations"
    )
  }

  # Fit using fitdistdoublecens
  # Based on web search, the function expects:
  # - data: data frame with left/right interval bounds
  # - distr: distribution name
  # - start: initial parameter values
  # - Additional column specifications
  fit <- primarycensored::fitdistdoublecens(
    delay_data,
    distr = fitdistr_dist,
    start = start_params,
    ...
  )

  if (verbose) {
    cli::cli_alert_success("Fitting complete")
    print(summary(fit))
  }

  # Extract MLE estimates
  params <- stats::coef(fit)

  # Convert to dist_spec format
  result <- .mle_fit_to_dist_spec(
    params = params,
    dist = dist,
    max_value = max_value %||% max(delay_data$delay_upper)
  )

  return(result)
}

#' Prepare delay data for MLE fitting
#'
#' @param data Input data (vector or data frame)
#' @param verbose Logical, print messages?
#' @return Data frame with delay_lower, delay_upper, n
#' @keywords internal
.prepare_delay_data_for_mle <- function(data, verbose = FALSE) {

  if (is.numeric(data)) {
    # Vector of delay values
    # Assume daily censoring: observed value X means true delay in [X, X+1)

    if (verbose) {
      cli::cli_alert_info("Converting vector to interval-censored format")
    }

    # Create frequency table
    delay_counts <- table(data)

    delay_df <- data.frame(
      delay_lower = as.numeric(names(delay_counts)),
      delay_upper = as.numeric(names(delay_counts)) + 1,
      n = as.numeric(delay_counts)
    )

  } else if (is.data.frame(data)) {
    # Assume already in correct format or can be converted

    required_cols <- c("delay_lower", "delay_upper")

    if (all(required_cols %in% names(data))) {
      delay_df <- data

      # Add n column if missing (assume each row is one observation)
      if (!"n" %in% names(delay_df)) {
        delay_df$n <- 1
      }

    } else {
      stop(
        "Data frame must have columns: delay_lower, delay_upper",
        "\nFound columns: ", paste(names(data), collapse = ", ")
      )
    }

  } else {
    stop("data must be a numeric vector or data frame")
  }

  return(delay_df)
}

#' Get starting parameter values for MLE
#'
#' @param delay_data Data frame with delay intervals
#' @param dist Distribution name (fitdistrplus format)
#' @return Named list of starting parameters
#' @keywords internal
.get_start_params <- function(delay_data, dist) {

  # Use midpoints of intervals for rough estimation
  midpoints <- (delay_data$delay_lower + delay_data$delay_upper) / 2
  weights <- delay_data$n

  # Weighted mean and SD
  wmean <- stats::weighted.mean(midpoints, weights)
  wsd <- sqrt(stats::weighted.mean((midpoints - wmean)^2, weights))

  # Distribution-specific starting values
  start <- switch(dist,
    "lnorm" = {
      # Method of moments for lognormal
      # If X ~ Lognormal(mu, sigma), then
      # E[X] = exp(mu + sigma^2/2)
      # Var[X] = exp(2*mu + sigma^2) * (exp(sigma^2) - 1)

      # Rough approximation
      sigma2_est <- log(1 + (wsd^2 / wmean^2))
      mu_est <- log(wmean) - sigma2_est / 2

      list(meanlog = mu_est, sdlog = sqrt(sigma2_est))
    },
    "gamma" = {
      # Method of moments for gamma
      # E[X] = shape * scale
      # Var[X] = shape * scale^2

      shape_est <- wmean^2 / wsd^2
      scale_est <- wsd^2 / wmean

      list(shape = shape_est, scale = scale_est)
    },
    "weibull" = {
      # Simple starting values
      list(shape = 2, scale = wmean)
    },
    "exp" = {
      # Exponential: E[X] = 1/rate
      list(rate = 1 / wmean)
    },
    stop("No starting values defined for distribution: ", dist)
  )

  return(start)
}

#' Convert MLE fit to dist_spec
#'
#' @param params Named vector of fitted parameters
#' @param dist Distribution name (EpiNow2 format)
#' @param max_value Maximum value for PMF
#' @return A dist_spec object
#' @keywords internal
.mle_fit_to_dist_spec <- function(params, dist, max_value) {

  # Convert fitdistrplus parameters to dist_spec format
  # For MLE, we have point estimates, not distributions
  # So we'll create a "fixed" distribution

  if (dist == "lognormal") {
    result <- dist_spec(
      dist = "lognormal",
      meanlog = params["meanlog"],
      sdlog = params["sdlog"],
      max = max_value
    )
  } else if (dist == "gamma") {
    # fitdistrplus uses shape/scale parameterization
    # dist_spec uses shape/rate, so convert
    result <- dist_spec(
      dist = "gamma",
      shape = params["shape"],
      rate = 1 / params["scale"],  # rate = 1/scale
      max = max_value
    )
  } else if (dist == "weibull") {
    result <- dist_spec(
      dist = "weibull",
      shape = params["shape"],
      scale = params["scale"],
      max = max_value
    )
  } else if (dist == "exponential") {
    result <- dist_spec(
      dist = "exponential",
      rate = params["rate"],
      max = max_value
    )
  } else {
    stop("Unsupported distribution: ", dist)
  }

  return(result)
}

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
