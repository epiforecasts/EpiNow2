#' Estimate a delay distribution (PROTOTYPE)
#'
#' @description `r lifecycle::badge("experimental")`
#' Estimate a delay distribution from observed data with support for right
#' truncation and primary event censoring using the primarycensored package.
#'
#' @param data Either:
#'   - A numeric vector of delay values (for simple case)
#'   - A data.frame with linelist data containing delay observations
#'
#' @param dist Character string, which distribution to fit. Supported options:
#'   - "lognormal" (default)
#'   - "gamma"
#'   - "weibull" (if supported by primarycensored)
#'
#' @param primary_dist Character, the primary distribution to use. Default is
#'   "uniform" for daily censoring.
#'
#' @param samples Numeric, number of posterior samples. Defaults to 2000.
#'
#' @param chains Numeric, number of MCMC chains. Defaults to 4.
#'
#' @param cores Numeric, number of cores to use. Defaults to 1.
#'
#' @param max_value Numeric, maximum delay value. If not provided, inferred
#'   from data.
#'
#' @param verbose Logical, print progress messages? Defaults to FALSE.
#'
#' @param backend Character, which Stan backend to use. Must be "cmdstanr"
#'   for primarycensored support. Defaults to "cmdstanr".
#'
#' @param ... Additional arguments passed to fitting functions.
#'
#' @return A `<dist_spec>` object summarising the fitted distribution.
#'
#' @details
#' This function provides a modern interface for estimating delay distributions
#' that properly accounts for:
#' - Primary event censoring (e.g., daily reporting of symptom onset)
#' - Secondary event censoring (e.g., daily reporting of case notification)
#' - Right truncation (observations may be truncated by observation time)
#'
#' The function uses the primarycensored package which implements the methods
#' described in Park et al. (2024) and Charniga et al. (2024).
#'
#' @references
#' Park, S. W., et al. "Estimating epidemiological delay distributions for
#'   infectious diseases", *medRxiv*, 2024.
#'   \doi{https://doi.org/10.1101/2024.01.12.24301247}
#' Charniga, K., et al. "Best practices for estimating and reporting
#'   epidemiological delay distributions of infectious diseases using public
#'   health surveillance and healthcare data", *arXiv e-prints*, 2024.
#'   \doi{10.48550/arXiv.2405.08841}
#'
#' @export
#' @examples
#' \donttest{
#' # Simple vector input (backwards compatible with estimate_delay)
#' delays <- rlnorm(500, log(5), 1)
#' dist <- estimate_dist(delays, samples = 1000)
#'
#' # Linelist data with truncation
#' # linelist <- data.frame(
#' #   ptime_lwr = sample(0:10, 100, replace = TRUE),
#' #   stime_lwr = sample(5:20, 100, replace = TRUE),
#' #   obs_time = 30
#' # )
#' # dist <- estimate_dist(linelist, dist = "gamma", samples = 1000)
#' }
estimate_dist <- function(data,
                          dist = "lognormal",
                          primary_dist = "uniform",
                          samples = 2000,
                          chains = 4,
                          cores = 1,
                          max_value,
                          verbose = FALSE,
                          backend = "cmdstanr",
                          ...) {
  # Check that primarycensored is available
  if (!requireNamespace("primarycensored", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "x" = "The {.pkg primarycensored} package is required for
        {.fn estimate_dist}.",
        "i" = "Install it with:
        {.code install.packages('primarycensored')}"
      )
    )
  }

  # Check backend
  if (backend != "cmdstanr") {
    cli::cli_abort(
      c(
        "x" = "{.fn estimate_dist} requires {.field backend = 'cmdstanr'}",
        "i" = "The primarycensored package only supports cmdstanr."
      )
    )
  }

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "x" = "The {.pkg cmdstanr} package is required.",
        "i" = "Install it from: {.url https://mc-stan.org/cmdstanr/}"
      )
    )
  }

  # Step 1: Get distribution IDs
  dist_id <- primarycensored::pcd_stan_dist_id(dist)

  # Primary distribution ID (for censoring of primary events)
  # For uniform censoring (daily), pwindow in data handles it, no dist needed
  if (primary_dist == "uniform") {
    primary_id <- 0  # 0 means no primary distribution (uniform window)
  } else {
    primary_id <- primarycensored::pcd_stan_dist_id(primary_dist)
  }

  # Step 2: Convert input data to primarycensored format
  pcd_data <- .convert_to_pcd_data(
    data, dist, dist_id, primary_dist, primary_id, verbose
  )

  # Step 3: Compile/load the Stan model
  if (verbose) {
    cli::cli_alert_info("Compiling Stan model...")
  }
  model <- primarycensored::pcd_cmdstan_model(
    cpp_options = list(stan_threads = TRUE)
  )

  # Step 4: Fit the model
  if (verbose) {
    cli::cli_alert_info("Fitting {dist} distribution...")
  }

  fit <- model$sample(
    data = pcd_data,
    chains = chains,
    parallel_chains = cores,
    threads_per_chain = 1,  # Use threading within chains
    iter_warmup = 1000,
    iter_sampling = ceiling(samples / chains),
    refresh = if (verbose) 500 else 0,
    show_messages = verbose,
    show_exceptions = verbose,
    ...
  )

  # Step 4: Extract posterior samples and convert to dist_spec
  result <- .extract_dist_spec(
    fit = fit,
    dist = dist,
    max_value = if (missing(max_value)) pcd_data$D else max_value
  )

  return(result)
}


#' Convert data to primarycensored format (internal helper)
#'
#' @param data Input data (vector or data.frame)
#' @param dist Distribution type
#' @param dist_id Distribution ID for Stan
#' @param primary_dist Primary distribution
#' @param primary_id Primary distribution ID for Stan
#' @param verbose Verbose output?
#'
#' @return A list of data for primarycensored Stan model
#' @keywords internal
.convert_to_pcd_data <- function(data, dist, dist_id, primary_dist, primary_id,
                                  verbose) {
  # Case 1: Simple numeric vector (backwards compatible)
  if (is.numeric(data) && !is.data.frame(data)) {
    return(.vector_to_pcd_data(data, dist_id, primary_id, verbose))
  }

  # Case 2: Data frame (linelist)
  if (is.data.frame(data)) {
    return(.linelist_to_pcd_data(data, dist_id, primary_id, verbose))
  }

  cli::cli_abort(
    c(
      "x" = "Unsupported data format.",
      "i" = "{.arg data} must be a numeric vector or data.frame."
    )
  )
}


#' Convert vector to primarycensored data (internal)
#'
#' @inheritParams .convert_to_pcd_data
#'
#' @return List for primarycensored
#' @keywords internal
.vector_to_pcd_data <- function(values, dist_id, primary_id, verbose) {
  # Clean data
  values <- as.integer(values)
  values <- values[!is.na(values)]
  values <- values[values >= 0]

  if (verbose) {
    cli::cli_alert_info(
      "Using {length(values)} delay observations (range: {min(values)}-{max(values)})"
    )
  }

  # For simple vector input, we assume:
  # - Primary events are uniformly distributed over [0, 1) (daily censoring)
  # - Secondary events are uniformly distributed over [delay, delay+1)
  # - No truncation (set D high to effectively disable it)

  # Create data frame for primarycensored
  # pcd_as_stan_data expects: delay, delay_upper, n, pwindow, relative_obs_time
  delay_df <- data.frame(
    delay = values,
    delay_upper = values + 1,
    n = 1,  # Each observation has weight 1
    pwindow = 1,  # Primary window (daily censoring)
    relative_obs_time = max(values) + 10  # No truncation
  )

  # Use primarycensored's data preparation helper
  # Use default parameter bounds (omit to use defaults)
  pcd_data <- primarycensored::pcd_as_stan_data(
    delay_df,
    dist_id = dist_id,
    primary_id = primary_id
  )

  return(pcd_data)
}


#' Convert linelist to primarycensored data (internal)
#'
#' @inheritParams .convert_to_pcd_data
#'
#' @return List for primarycensored
#' @keywords internal
.linelist_to_pcd_data <- function(data, dist_id, primary_id, verbose) {
  # Validate required columns
  # This is a simplified version - full implementation would be more robust
  required_cols <- c("ptime_lwr", "stime_lwr")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "x" = "Linelist data missing required columns: {missing_cols}",
        "i" = "Required: {.field ptime_lwr}, {.field stime_lwr}",
        "i" = "Optional: {.field ptime_upr}, {.field stime_upr},
        {.field obs_time}"
      )
    )
  }

  # Calculate delays from primary to secondary event
  data$delay <- data$stime_lwr - data$ptime_lwr

  # Handle upper bounds for delays
  if ("stime_upr" %in% names(data) && "ptime_lwr" %in% names(data)) {
    data$delay_upper <- data$stime_upr - data$ptime_lwr
  } else {
    # Assume daily censoring for secondary event
    data$delay_upper <- data$delay + 1
  }

  # Add count column if not present
  if (!"n" %in% names(data)) {
    data$n <- 1  # Each row is one observation
  }

  # Add primary window if not present
  if (!"pwindow" %in% names(data)) {
    data$pwindow <- 1  # Daily censoring
  }

  # Add relative observation time for truncation
  if ("obs_time" %in% names(data) && "ptime_lwr" %in% names(data)) {
    data$relative_obs_time <- data$obs_time - data$ptime_lwr
  } else {
    # No truncation - set relative_obs_time high
    data$relative_obs_time <- max(data$delay_upper) + 10
  }

  if (verbose) {
    cli::cli_alert_info(
      "Using {nrow(data)} linelist observations (delay range: {min(data$delay)}-{max(data$delay)})"
    )
  }

  # Use primarycensored's data preparation
  # Pass the data frame directly (omit optional params to use defaults)
  pcd_data <- primarycensored::pcd_as_stan_data(
    data,
    dist_id = dist_id,
    primary_id = primary_id
  )

  return(pcd_data)
}


#' Extract dist_spec from primarycensored fit (internal)
#'
#' @param fit CmdStanMCMC object from primarycensored
#' @param dist Distribution type
#' @param max_value Maximum value for dist_spec
#'
#' @return A dist_spec object
#' @keywords internal
.extract_dist_spec <- function(fit, dist, max_value) {
  # Extract posterior draws
  draws <- fit$draws(format = "df")

  # Extract distribution parameters based on type
  # Parameter names from primarycensored Stan model
  params <- switch(dist,
    lognormal = list(
      meanlog = draws$mu,
      sdlog = draws$sigma
    ),
    gamma = list(
      shape = draws$alpha,
      rate = draws$beta
    ),
    weibull = list(
      shape = draws$shape,
      scale = draws$scale
    ),
    cli::cli_abort("Distribution {dist} not yet implemented")
  )

  # Convert parameter samples to Normal distributions (uncertainty representation)
  # This follows the pattern in bootstrapped_dist_fit (estimate_delay.R:230-232)
  params_dist <- lapply(params, function(x) {
    Normal(mean = mean(x), sd = sd(x))
  })

  # Create dist_spec object
  result <- new_dist_spec(
    params = params_dist,
    max = max_value,
    distribution = dist
  )

  # TODO: Could add additional attributes here:
  # - result$fit <- fit  # Store raw fit for diagnostics
  # - result$diagnostics <- fit$diagnostic_summary()
  # - result$loo <- loo(fit)  # Model comparison

  return(result)
}
