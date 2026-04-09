#' Estimate a delay distribution using primarycensored
#'
#' @description `r lifecycle::badge("experimental")`
#' Fit a delay distribution accounting for primary and secondary
#' event censoring (double interval censoring) and right
#' truncation.
#' Estimation is done via MCMC using a Stan model that vendors
#' likelihood functions from the
#' \link[primarycensored:primarycensored-package]{primarycensored}
#' package.
#' For more flexible delay distribution modelling (e.g.
#' time-varying delays, partial pooling, or regression on
#' covariates), see the
#' \href{https://epidist.epinowcast.org/}{epidist} package.
#' If you use this function, please cite `primarycensored` in
#' addition to `EpiNow2`.
#'
#' @param data A data.frame with date columns:
#'   - `pdate_lwr` (required): lower bound of primary event date
#'   - `pdate_upr` (optional): upper bound of primary event date
#'     (default: `pdate_lwr + 1`)
#'   - `sdate_lwr` (required): lower bound of secondary event
#'     date
#'   - `sdate_upr` (optional): upper bound of secondary event
#'     date (default: `sdate_lwr + 1`)
#'   - `obs_date` (optional): observation/censoring date
#'     (default: `max(sdate_upr)`)
#'   - `n` (optional): observation count/weight (default: 1)
#'
#' @param dist Character string, which distribution to fit.
#'   One of `"lognormal"` (default), `"gamma"`, `"normal"`,
#'   or `"exp"`.
#'
#' @param priors A list of `<dist_spec>` objects specifying priors
#'   for the distribution parameters.
#'   Names must match the parameters of the chosen distribution.
#'   Defaults depend on `dist`:
#'   - lognormal:
#'     `list(meanlog = Normal(1, 1), sdlog = Normal(0.5, 0.5))`
#'   - gamma:
#'     `list(shape = Normal(2, 2), rate = Normal(0.5, 0.5))`
#'   - normal:
#'     `list(mean = Normal(5, 5), sd = Normal(1, 1))`
#'   - exp: `list(rate = Normal(0.5, 0.5))`
#'
#' @param primary Character string specifying the primary event
#'   distribution. One of:
#'   - `"uniform"` (default): uniform distribution over the
#'     primary window
#'   - `"expgrowth"`: exponential growth distribution.
#'     Requires `primary_params` to supply a fixed growth rate.
#'
#' @param primary_params Numeric vector of parameters for the
#'   primary distribution.
#'   Only used when `primary = "expgrowth"`, in which case it
#'   should be a single numeric value for the growth rate.
#'   The growth rate is passed as fixed data to Stan, not
#'   estimated.
#'
#' @param max_value Numeric, maximum delay value for PMF.
#'   If not provided, inferred from data.
#'
#' @param obs_time_threshold Numeric, multiplier for the
#'   obs-time-to-Inf heuristic. Observations where
#'   `relative_obs_time > max(delay_upr) * obs_time_threshold`
#'   are treated as untruncated. Default 2, following
#'   `epidist`. Set to `Inf` to disable.
#'
#' @param verbose Logical, print progress messages?
#'   Defaults to FALSE.
#'
#' @return An `<estimate_dist>` object (inheriting from
#'   `<epinowfit>`) with components:
#'   \describe{
#'     \item{fit}{The Stan fit object.}
#'     \item{args}{The Stan data list used for fitting.}
#'     \item{data}{The input data.}
#'   }
#'   Use [get_parameters()] to extract the fitted `<dist_spec>`.
#'
#' @details
#' The model fits an interval-censored delay distribution while
#' accounting for:
#' - Primary event censoring (e.g., daily reporting of exposure)
#' - Secondary event censoring (e.g., daily reporting of symptom
#'   onset)
#' - Right truncation (observation window effects)
#' - Per-observation truncation times (via `obs_date`)
#'
#' When a data frame with date columns is provided, observations
#' are aggregated by unique combinations of `(delay_lwr,
#' delay_upr, pwindow, relative_obs_time)` to reduce the number
#' of likelihood evaluations.
#' Observations where the relative observation time is much
#' larger than the maximum observed delay are treated as
#' untruncated (observation time set to infinity).
#'
#' The `primarycensored` Stan functions are vendored (included
#' in the package), so the model is pre-compiled and runs without
#' needing `primarycensored` at runtime.
#'
#' ## Limitations
#'
#' - Delay distributions are limited to lognormal, gamma,
#'   normal, and exponential.
#' - The primary event distribution is limited to uniform or
#'   exponential growth with a fixed rate.
#'   Primary event parameters are not estimated.
#' - Left truncation is not yet exposed (internally set to 0).
#'
#' @references
#' Park SW, et al. (2024) "Estimating epidemiological delay
#' distributions for infectious diseases."
#' doi:10.1101/2024.01.12.24301247
#'
#' Charniga K, Park SW, et al. (2024) "Best practices for
#' estimating and reporting epidemiological delay
#' distributions of infectious diseases."
#' PLoS Comput Biol 20(10): e1012520.
#' doi:10.1371/journal.pcbi.1012520
#'
#' Please cite `primarycensored` if you use this function;
#' see `citation("primarycensored")`.
#'
#' @seealso `vignette("estimate-dist", package = "EpiNow2")` for
#'   a worked example, and
#'   [primarycensored::primarycensored-package] for the underlying
#'   censoring methodology.
#'
#' @importFrom checkmate assert_string assert_list assert_number
#'   assert_numeric assert_logical assert_date
#'   assert_data_frame assert_integerish
#' @inheritParams estimate_infections
#' @export
#' @examples
#' \donttest{
#' # Fit lognormal distribution from date-based linelist
#' if (requireNamespace("primarycensored", quietly = TRUE)) {
#'   set.seed(1)
#'   n <- 100
#'   D <- 30
#'   pdate_lwr <- as.Date("2023-01-01") + rpois(n, 5)
#'   delays_sim <- primarycensored::rprimarycensored(
#'     n = n, rdist = rlnorm,
#'     meanlog = log(5), sdlog = 0.5,
#'     pwindow = 1, D = D
#'   )
#'   linelist <- data.frame(
#'     pdate_lwr = pdate_lwr,
#'     sdate_lwr = pdate_lwr + delays_sim,
#'     obs_date = pdate_lwr + D
#'   )
#'   result <- estimate_dist(linelist, dist = "lognormal")
#' }
#' }
estimate_dist <- function(data,
                          dist = "lognormal",
                          priors = switch(dist,
                            "lognormal" = list(
                              meanlog = Normal(1, 1),
                              sdlog = Normal(0.5, 0.5)
                            ),
                            "gamma" = list(
                              shape = Normal(2, 2),
                              rate = Normal(0.5, 0.5)
                            ),
                            "normal" = list(
                              mean = Normal(5, 5),
                              sd = Normal(1, 1)
                            ),
                            "exp" = list(
                              rate = Normal(0.5, 0.5)
                            )
                          ),
                          primary = "uniform",
                          primary_params = numeric(0),
                          stan = stan_opts(),
                          max_value = NULL,
                          obs_time_threshold = 2,
                          verbose = FALSE) {

  # Validate inputs
  assert_string(dist)
  assert_list(priors)
  assert_string(primary)
  assert_numeric(primary_params)
  assert_number(obs_time_threshold, lower = 0)
  assert_logical(verbose, len = 1)

  dist_id <- .get_dist_id(dist)
  param_names <- .get_param_names(dist)

  # Validate primary distribution
  primary_id <- switch(primary,
    "uniform" = 1L,
    "expgrowth" = 2L,
    cli::cli_abort(c(
      "x" = "Unsupported primary distribution: {primary}",
      "i" = "Supported: uniform, expgrowth"
    ))
  )
  if (primary == "expgrowth" && length(primary_params) != 1) {
    cli::cli_abort(c(
      "x" = paste(
        "primary_params must be a single numeric value",
        "(growth rate) when primary = \"expgrowth\""
      )
    ))
  }

  # Validate prior names
  if (!setequal(names(priors), param_names)) {
    cli::cli_abort(c(
      "x" = "Invalid prior names for {dist} distribution",
      "i" = paste(
        "Expected: {paste(param_names, collapse = ', ')};",
        "got: {paste(names(priors), collapse = ', ')}"
      )
    ))
  }

  # Convert data to aggregated format
  delay_data <- .prepare_linelist_data(
    data, obs_time_threshold, verbose
  )

  # Build params list
  lbounds <- lower_bounds(dist)
  params <- lapply(param_names, function(name) {
    make_param(
      name, priors[[name]], lower_bound = lbounds[[name]]
    )
  })

  # Prepare Stan data
  stan_data <- list(
    n = nrow(delay_data),
    delay = as.integer(delay_data$delay_lwr),
    delay_upper = as.numeric(delay_data$delay_upr),
    n_obs = as.integer(delay_data$n),
    pwindow = as.integer(delay_data$pwindow),
    D = as.numeric(delay_data$relative_obs_time),
    L = rep(0.0, nrow(delay_data)),
    dist_id = dist_id,
    primary_id = primary_id,
    n_primary_params = as.integer(length(primary_params)),
    primary_params = array(as.numeric(primary_params))
  )

  # Add params using standard EpiNow2 infrastructure
  stan_data <- c(stan_data, create_stan_params(params))

  if (verbose) {
    cli::cli_alert_info(
      "Fitting {dist} to {sum(stan_data$n_obs)} observations \\
      ({nrow(delay_data)} unique combinations) using \\
      {stan$backend %||% 'rstan'}"
    )
  }

  # Compute initial values based on MoM estimates
  midpoints <- (delay_data$delay_lwr + delay_data$delay_upr) / 2
  obs_weights <- delay_data$n
  wmean <- stats::weighted.mean(midpoints, obs_weights)
  wvar <- stats::weighted.mean(
    (midpoints - wmean)^2, obs_weights
  )

  init_params <- switch(dist,
    "lognormal" = c(log(wmean), sqrt(log(1 + wvar / wmean^2))),
    "gamma" = c(wmean^2 / wvar, wmean / wvar)
  )
  # Ensure init is within bounds
  init_params <- pmax(
    init_params, stan_data$params_lower + 0.01
  )
  init_params <- pmin(
    init_params, stan_data$params_upper - 0.01
  )
  init_fn <- function() list(params = init_params)

  # Create stan args and fit using shared infrastructure
  stan_args <- create_stan_args(
    stan = stan, data = stan_data, init = init_fn,
    model = "estimate_dist", verbose = verbose
  )
  fit <- fit_model(stan_args, id = "estimate_dist")

  out <- list(
    data = data,
    args = c(
      stan_data,
      list(
        dist = dist,
        max_value = max_value %||% max(delay_data$delay_upr)
      )
    ),
    fit = fit
  )
  class(out) <- c("estimate_dist", "epinowfit", class(out))

  if (verbose) {
    cli::cli_alert_success("Fitting complete")
  }

  out
}

#' Map distribution name to primarycensored ID
#'
#' @param dist Character distribution name
#' @return Integer distribution ID
#' @keywords internal
.get_dist_id <- function(dist) {
  # Map EpiNow2 names to primarycensored names
  pc_name <- switch(dist,
    "lognormal" = "lnorm",
    "exp" = "exp",
    dist
  )
  tryCatch(
    primarycensored::pcd_stan_dist_id(pc_name, "delay"),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Unsupported distribution: {dist}",
        "i" = "Supported: lognormal, gamma, normal, exp"
      ))
    }
  )
}

#' Map distribution name to parameter names
#'
#' @param dist Character distribution name
#' @return Character vector of parameter names
#' @keywords internal
.get_param_names <- function(dist) {
  switch(dist,
    "lognormal" = c("meanlog", "sdlog"),
    "gamma" = c("shape", "rate"),
    "normal" = c("mean", "sd"),
    "exp" = "rate",
    cli::cli_abort(c(
      "x" = "Unsupported distribution: {dist}",
      "i" = "Supported: lognormal, gamma, normal, exp"
    ))
  )
}

#' Prepare linelist data for delay estimation
#'
#' Converts date-based or numeric delay data into an aggregated format
#' suitable for the primarycensored Stan model. Handles date-to-numeric
#' conversion, computation of derived columns, an obs-time-to-Inf
#' heuristic, and aggregation by unique delay/censoring combinations.
#'
#' @param data A data frame with date columns (`pdate_lwr`,
#'   `sdate_lwr`, and optionally `pdate_upr`, `sdate_upr`,
#'   `obs_date`, `n`).
#' @param verbose Logical, print progress messages?
#' @return A data frame with columns: `delay_lwr`, `delay_upr`,
#'   `pwindow`, `relative_obs_time`, `n`.
#' @keywords internal
.prepare_linelist_data <- function(data,
                                   obs_time_threshold = 2,
                                   verbose = FALSE) {

  assert_data_frame(data)

  # Validate required columns
  required <- c("pdate_lwr", "sdate_lwr")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = paste(
        "Data frame must have columns:",
        toString(required)
      ),
      "i" = "Found: {paste(names(data), collapse = ', ')}"
    ))
  }

  # Validate column types
  assert_date(data$pdate_lwr, .var.name = "pdate_lwr")
  assert_date(data$sdate_lwr, .var.name = "sdate_lwr")

  # Validate optional columns if present, default if absent
  if (!is.null(data$pdate_upr)) {
    assert_date(data$pdate_upr, .var.name = "pdate_upr")
  } else {
    data$pdate_upr <- data$pdate_lwr + 1
    cli::cli_inform(
      "Assuming daily primary censoring \\
      (pdate_upr = pdate_lwr + 1)"
    )
  }
  if (!is.null(data$sdate_upr)) {
    assert_date(data$sdate_upr, .var.name = "sdate_upr")
  } else {
    data$sdate_upr <- data$sdate_lwr + 1
    cli::cli_inform(
      "Assuming daily secondary censoring \\
      (sdate_upr = sdate_lwr + 1)"
    )
  }
  if (!is.null(data$obs_date)) {
    assert_date(data$obs_date, .var.name = "obs_date")
  } else {
    data$obs_date <- max(data$sdate_upr)
    cli::cli_inform(
      "No obs_date supplied, using \\
      max(sdate_upr): {max(data$obs_date)}"
    )
  }
  if (!is.null(data$n)) {
    assert_integerish(
      data$n, lower = 1, .var.name = "n"
    )
  } else {
    data$n <- 1L
  }

  # Check upper bounds >= lower bounds
  if (any(data$pdate_upr <= data$pdate_lwr)) {
    cli::cli_abort(
      "pdate_upr must be > pdate_lwr for all rows"
    )
  }
  if (any(data$sdate_upr <= data$sdate_lwr)) {
    cli::cli_abort(
      "sdate_upr must be > sdate_lwr for all rows"
    )
  }
  if (any(data$obs_date <= data$pdate_lwr)) {
    cli::cli_abort(
      "obs_date must be > pdate_lwr for all rows"
    )
  }

  # Convert dates to numeric relative to min(pdate_lwr)
  origin <- min(data$pdate_lwr)
  ptime_lwr <- as.numeric(
    difftime(data$pdate_lwr, origin, units = "days")
  )
  ptime_upr <- as.numeric(
    difftime(data$pdate_upr, origin, units = "days")
  )
  stime_lwr <- as.numeric(
    difftime(data$sdate_lwr, origin, units = "days")
  )
  stime_upr <- as.numeric(
    difftime(data$sdate_upr, origin, units = "days")
  )
  obs_time <- as.numeric(
    difftime(data$obs_date, origin, units = "days")
  )

  # Compute derived columns
  delay_lwr <- as.integer(stime_lwr - ptime_lwr)
  delay_upr <- as.integer(ceiling(stime_upr - ptime_lwr))
  pwindow <- as.integer(ptime_upr - ptime_lwr)
  relative_obs_time <- obs_time - ptime_lwr

  # Check for negative delays (sdate_lwr < pdate_lwr)
  n_negative <- sum(delay_lwr < 0)
  if (n_negative > 0) {
    cli::cli_abort(c(
      "x" = paste(
        "{n_negative} observation(s) have sdate_lwr",
        "earlier than pdate_lwr (negative delay)"
      ),
      "i" = "Check your data for date errors."
    ))
  }

  # Validate obs_date >= sdate_upr for each observation
  bad_trunc <- relative_obs_time < delay_upr
  if (any(bad_trunc)) {
    cli::cli_abort(c(
      "x" = paste(
        "{sum(bad_trunc)} observation(s) have obs_date",
        "earlier than sdate_upr"
      ),
      "i" = paste(
        "obs_date must be >= sdate_upr for all rows.",
        "Increase obs_date or check your data."
      )
    ))
  }

  # Obs-time-to-Inf heuristic: if relative_obs_time is much
  # larger than the max delay, treat as untruncated.
  # Default threshold follows epidist's obs_time_threshold.
  max_delay <- max(delay_upr)
  threshold <- max_delay * obs_time_threshold
  far_from_truncation <- relative_obs_time > threshold
  if (any(far_from_truncation)) {
    cli::cli_inform(
      paste(
        "Setting {sum(far_from_truncation)} observation(s)",
        "with obs_time > {round(threshold, 1)} to",
        "untruncated (Inf)"
      )
    )
  }
  relative_obs_time[far_from_truncation] <- Inf

  # Aggregate by unique combinations
  agg_df <- data.frame(
    delay_lwr = delay_lwr,
    delay_upr = delay_upr,
    pwindow = pwindow,
    relative_obs_time = relative_obs_time,
    n = as.integer(data$n)
  )

  result <- stats::aggregate(
    n ~ delay_lwr + delay_upr + pwindow + relative_obs_time,
    data = agg_df,
    FUN = sum
  )

  if (verbose) {
    cli::cli_alert_info(
      "Aggregated {nrow(agg_df)} observations into \\
      {nrow(result)} unique combinations"
    )
  }

  result
}

#' Extract parameters and convert to dist_spec
#'
#' @keywords internal
.extract_to_dist_spec <- function(fit, dist, max_value) {
  samples <- extract_samples(fit, pars = "delay_params")

  param_names <- .get_param_names(dist)

  params <- lapply(seq_along(param_names), function(i) {
    Normal(
      mean = mean(samples$delay_params[, i]),
      sd = sd(samples$delay_params[, i])
    )
  })
  names(params) <- param_names

  new_dist_spec(
    params = params, max = max_value, distribution = dist
  )
}
