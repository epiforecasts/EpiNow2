#' Prepare truncation observations for Stan
#'
#' @description Internal function to process a list of observation snapshots
#' into the matrix format required by the truncation Stan model.
#'
#' @param data A list of `<data.frame>`s each containing date and confirm
#'   columns. Each data set should be a snapshot of reported data.
#' @param trunc_max Integer, the maximum truncation delay to consider.
#'
#' @return A list containing:
#' - `obs`: Matrix of observations (time x datasets)
#' - `obs_dist`: Vector of NA counts per dataset (used to determine truncation)
#' - `t`: Number of time points
#' - `obs_sets`: Number of observation datasets
#' - `dirty_obs`: The processed data.tables (ordered by nrow)
#'
#' @keywords internal
prepare_truncation_obs <- function(data, trunc_max) {
  # Convert to data.tables and find common date range
  dirty_obs <- purrr::map(data, data.table::as.data.table)
  earliest_date <- max(
    as.Date(
      purrr::map_chr(dirty_obs, function(x) x[, as.character(min(date))])
    )
  )
  dirty_obs <- purrr::map(dirty_obs, function(x) x[date >= earliest_date])

  # Order by number of rows (shortest first)
  nrow_obs <- order(purrr::map_dbl(dirty_obs, nrow))
  dirty_obs <- dirty_obs[nrow_obs]

  # Merge all observations into a single data.table with columns named 1, 2, ...
  obs <- purrr::map(dirty_obs, data.table::copy)
  obs <- purrr::map(seq_along(obs), ~ obs[[.]][, (as.character(.)) := confirm][
    ,
    confirm := NULL
  ])
  obs <- purrr::reduce(obs, merge, all = TRUE)

  # Calculate observation start point and distance metrics
  obs_start <- max(nrow(obs) - trunc_max - sum(is.na(obs$`1`)) + 1, 1)
  obs_dist <- purrr::map_dbl(2:(ncol(obs)), ~ sum(is.na(obs[[.]])))

  # Create observation matrix (replacing NAs with 0)
  obs_data <- obs[, -1][, purrr::map(.SD, ~ ifelse(is.na(.), 0, .))]
  obs_data <- as.matrix(obs_data[obs_start:.N])

  list(
    obs = obs_data,
    obs_dist = obs_dist,
    t = nrow(obs_data),
    obs_sets = ncol(obs_data),
    dirty_obs = dirty_obs
  )
}

#' Merge truncation predictions with observations for display
#'
#' @description Internal function to prepare data for plotting or returning
#' merged predictions and observations. Combines predictions with observed
#' data from each snapshot, including the latest observations as reference.
#'
#' @param observations A list of `<data.frame>`s containing date and confirm
#'   columns, as stored in an `estimate_truncation` object.
#' @param predictions A `<data.table>` of predictions from [get_predictions()].
#'
#' @return A `<data.table>` with columns: date, report_date, confirm (observed),
#'   last_confirm (from latest snapshot), and prediction columns (median, CrIs).
#'
#' @keywords internal
merge_trunc_pred_obs <- function(observations, predictions) {
  # Get latest observations for reference
  last_obs <- data.table::as.data.table(observations[[length(observations)]])
  last_obs <- last_obs[, .(date, last_confirm = confirm)]

  # Get truncated observations from each snapshot with report_date

  obs_list <- purrr::map(observations, function(obs) {
    obs_dt <- data.table::as.data.table(obs)
    obs_dt[, report_date := max(date)]
    obs_dt
  })
  obs_combined <- data.table::rbindlist(obs_list)

  # Merge predictions with observations
  result <- data.table::merge.data.table(
    predictions, obs_combined[, .(date, confirm, report_date)],
    by = c("date", "report_date")
  )
  data.table::merge.data.table(result, last_obs, by = "date")
}

#' Estimate Truncation of Observed Data
#'
#' @description `r lifecycle::badge("stable")`
#' Estimates a truncation distribution from multiple snapshots of the same
#' data source over time. This distribution can then be used passed to the
#' `truncation` argument in [regional_epinow()], [epinow()], and
#' [estimate_infections()] to adjust for truncated data and propagate the
#' uncertainty associated with data truncation into the estimates.
#'
#' See [here](https://gist.github.com/seabbs/176b0c7f83eab1a7192a25b28bbd116a)
#' for an example of using this approach on Covid-19 data in England. The
#' functionality offered by this function is now available in a more principled
#' manner in the [`epinowcast` R package](https://package.epinowcast.org/).
#'
#' The model of truncation is as follows:
#'
#' 1. The truncation distribution is assumed to be discretised log normal wit
#' a mean and standard deviation that is informed by the data.
#'
#' 2. The data set with the latest observations is adjusted for truncation using
#' the truncation distribution.
#'
#' 3. Earlier data sets are recreated by applying the truncation distribution to
#' the adjusted latest observations in the time period of the earlier data set.
#' These data sets are then compared to the earlier observations assuming a
#' negative binomial observation model with an additive noise term to deal with
#' zero observations.
#'
#' This model is then fit using `stan` with standard normal, or half normal,
#' prior for the mean, standard deviation, 1 over the square root of the
#' overdispersion and additive noise term.
#'
#' This approach assumes that:
#'  - Current truncation is related to past truncation.
#'  - Truncation is a multiplicative scaling of underlying reported cases.
#'  - Truncation is log normally distributed.
#'
#' @param data  A list of `<data.frame>`s each containing a date variable
#' and a confirm (numeric) variable. Each data set should be a snapshot
#' of the reported data over time. All data sets must contain a complete vector
#' of dates.
#'
#' @param verbose Logical, should model fitting progress be returned.
#'
#' @param ... Additional parameters to pass to [rstan::sampling()].
#'
#' @return An `<estimate_truncation>` object containing:
#'
#' - `observations`: The input data (list of `<data.frame>`s).
#' - `args`: A list of arguments used for fitting (stan data).
#' - `fit`: The stan fit object.
#'
#' @seealso [get_samples()] [get_predictions()] [get_parameters()]
#' @export
#' @inheritParams calc_CrIs
#' @inheritParams estimate_infections
#' @importFrom purrr map reduce map_dbl walk
#' @importFrom rstan sampling
#' @importFrom data.table copy .N as.data.table merge.data.table setDT
#' @importFrom data.table setcolorder
#' @importFrom rlang arg_match
#' @importFrom checkmate assert_character assert_numeric assert_class
#' assert_logical
#' @examples
#' \donttest{
#' # set number of cores to use
#' old_opts <- options()
#' options(mc.cores = ifelse(interactive(), 4, 1))
#'
#' # fit model to example data
#' # See [example_truncated] for more details
#' est <- estimate_truncation(example_truncated,
#'   verbose = interactive(),
#'   chains = 2, iter = 2000
#' )
#'
#' # extract the estimated truncation distribution
#' get_parameters(est)[["truncation"]]
#' # summarise the truncation distribution parameters
#' summary(est)
#' # validation plot of observations vs estimates
#' plot(est)
#'
#' # Pass the truncation distribution to `epinow()`.
#' # Note, we're using the last snapshot as the observed data as it contains
#' # all the previous snapshots. Also, we're using the default options for
#' # illustrative purposes only.
#' out <- epinow(
#'   generation_time = generation_time_opts(example_generation_time),
#'   example_truncated[[5]],
#'   truncation = trunc_opts(get_parameters(est)[["truncation"]])
#' )
#' plot(out)
#' options(old_opts)
#' }
estimate_truncation <- function(data,
                                truncation = trunc_opts(
                                  LogNormal(
                                    meanlog = Normal(0, 1),
                                    sdlog = Normal(1, 1),
                                    max = 10
                                  )
                                ),
                                stan = stan_opts(),
                                CrIs = c(0.2, 0.5, 0.9),
                                filter_leading_zeros = FALSE,
                                zero_threshold = Inf,
                                verbose = TRUE,
                                ...) {
  # Validate inputs
  walk(data, check_reports_valid, model = "estimate_infections")
  assert_class(truncation, "dist_spec")
  assert_numeric(CrIs, lower = 0, upper = 1)
  assert_logical(filter_leading_zeros)
  assert_numeric(zero_threshold, lower = 0)
  assert_logical(verbose)

  # Prepare observation matrix for Stan
  obs_prep <- prepare_truncation_obs(data, trunc_max = max(truncation))
  stan_data <- list(
    obs = obs_prep$obs,
    obs_dist = obs_prep$obs_dist,
    t = obs_prep$t,
    obs_sets = obs_prep$obs_sets
  )

  stan_data <- c(stan_data, create_stan_delays(
    truncation = truncation,
    time_points = stan_data$t
  ))

  # initial conditions
  init_fn <- function() {
    c(create_delay_inits(stan_data), list(
      dispersion = abs(rnorm(1, 0, 1)),
      sigma = abs(rnorm(1, 0, 1))
    ))
  }
  stan_args <- create_stan_args(
    stan = stan, data = stan_data, init = init_fn, model = "estimate_truncation"
  )

  # Warn if truncation distribution is longer than observed time
  check_truncation_length(stan_args, time_points = stan_data$t)

  # fit
  fit <- fit_model(stan_args, id = "estimate_truncation")

  out <- list(
    observations = data,
    args = stan_data,
    fit = fit
  )

  class(out) <- c("estimate_truncation", "epinowfit", class(out))
  out
}

#' Plot method for estimate_truncation
#'
#' @description `r lifecycle::badge("experimental")`
#' [plot()] method for class `<estimate_truncation>`. Returns
#' a plot faceted over each dataset used in fitting with the latest
#' observations as columns, the data observed at the time (and so truncated)
#' as dots and the truncation adjusted estimates as a ribbon.
#'
#' @param x A list of output as produced by [estimate_truncation()]
#'
#' @param ... Pass additional arguments to plot function. Not currently in use.
#'
#' @return `ggplot2` object
#' @seealso [estimate_truncation()]
#' @method plot estimate_truncation
#' @importFrom ggplot2 ggplot aes geom_col geom_point labs scale_x_date
#' @importFrom ggplot2 scale_y_continuous theme theme_bw
#' @export
plot.estimate_truncation <- function(x, ...) {
  preds <- get_predictions(x)
  plot_data <- merge_trunc_pred_obs(x$observations, preds)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = last_confirm)) +
    ggplot2::geom_col(
      fill = "grey", col = "white",
      show.legend = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = plot_data,
      ggplot2::aes(x = date, y = confirm)
    ) +
    ggplot2::facet_wrap(~report_date, scales = "free")

  p <- plot_CrIs(p, extract_CrIs(plot_data),
    alpha = 0.8, linewidth = 1
  )

  p +
    ggplot2::theme_bw() +
    ggplot2::labs(
      y = "Reports", x = "Date", col = "Type", fill = "Type"
    ) +
    ggplot2::scale_x_date(date_breaks = "day", date_labels = "%b %d") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
}

#' @export
#' @method $ estimate_truncation
`$.estimate_truncation` <- function(x, name) {
  # Handle $dist with deprecation warning
  if (name == "dist") {
    lifecycle::deprecate_warn(
      "1.8.0",
      I("estimate_truncation()$dist"),
      I("get_parameters(x)[['truncation']]")
    )
    return(get_parameters(x)[["truncation"]])
  }

  if (name == "obs") {
    lifecycle::deprecate_warn(
      "1.8.0",
      I("estimate_truncation()$obs"),
      I("get_predictions() and observations")
    )
    # Reconstruct old format: predictions merged with observations
    preds <- get_predictions(x)
    obs <- .subset2(x, "observations")
    return(merge_trunc_pred_obs(obs, preds))
  }

  if (name == "data") {
    lifecycle::deprecate_warn(
      "1.8.0",
      I("estimate_truncation()$data"),
      I("estimate_truncation()$args")
    )
    return(.subset2(x, "args"))
  }

  if (name == "last_obs") {
    lifecycle::deprecate_warn(
      "1.8.0",
      I("estimate_truncation()$last_obs"),
      details = "Use the last element of `observations` instead."
    )
    obs <- .subset2(x, "observations")
    last <- data.table::as.data.table(obs[[length(obs)]])
    return(last[, .(date, confirm)])
  }

  if (name == "cmf") {
    lifecycle::deprecate_warn(
      "1.8.0",
      I("estimate_truncation()$cmf"),
      I("get_parameters(x)[['truncation']]")
    )
    trunc_dist <- get_parameters(x)[["truncation"]]
    # Extract mean parameter values for discretisation
    dist_type <- get_distribution(trunc_dist)
    param_names <- natural_params(dist_type)
    params <- lapply(param_names, function(p) {
      trunc_dist[[1]][[p]]$parameters$mean
    })
    names(params) <- param_names
    fixed_dist <- new_dist_spec(
      params = params,
      max = max(trunc_dist),
      distribution = dist_type
    )
    pmf <- discretise(fixed_dist)[[1]]
    return(cumsum(pmf))
  }

  # Use .subset2 instead of NextMethod for list-based S3 objects
  .subset2(x, name)
}

#' @export
#' @method [[ estimate_truncation
`[[.estimate_truncation` <- function(x, name) {
  # Delegate to $ method for deprecated element handling
  deprecated_names <- c("dist", "obs", "data", "last_obs", "cmf")
  if (name %in% deprecated_names) {
    return(`$.estimate_truncation`(x, name))
  }
  # Use .subset2 instead of NextMethod for list-based S3 objects
  .subset2(x, name)
}
