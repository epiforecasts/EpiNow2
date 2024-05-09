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
#' @param obs Deprecated; use `data` instead.
#'
#' @param model A compiled stan model to override the default model. May be
#' useful for package developers or those developing extensions.
#'
#' @param weigh_delay_priors Deprecated; use the `weight_prior` option in
#'   [trunc_opts()] instead.
#'
#' @param verbose Logical, should model fitting progress be returned.
#'
#' @param ... Additional parameters to pass to [rstan::sampling()].
#'
#' @return A list containing: the summary parameters of the truncation
#' distribution (`dist`), which could be passed to the `truncation` argument
#' of [epinow()], [regional_epinow()], and [estimate_infections()], the
#' estimated CMF of the truncation distribution (`cmf`, can be used to
#' adjusted new data), a `<data.frame>` containing the observed truncated
#' data, latest observed data and the adjusted for
#' truncation observations (`obs`), a `<data.frame>` containing the last
#' observed data (`last_obs`, useful for plotting and validation), the data
#' used for fitting (`data`) and the fit object (`fit`).
#'
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
#' # summary of the distribution
#' est$dist
#' # summary of the estimated truncation cmf (can be applied to new data)
#' print(est$cmf)
#' # observations linked to truncation adjusted estimates
#' print(est$obs)
#' # validation plot of observations vs estimates
#' plot(est)
#'
#' # Pass the truncation distribution to `epinow()`.
#' # Note, we're using the last snapshot as the observed data as it contains
#' # all the previous snapshots. Also, we're using the default options for
#' # illustrative purposes only.
#' out <- epinow(
#'   example_truncated[[5]],
#'   truncation = trunc_opts(est$dist)
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
                                model = NULL,
                                stan = stan_opts(),
                                CrIs = c(0.2, 0.5, 0.9),
                                filter_leading_zeros = FALSE,
                                zero_threshold = Inf,
                                weigh_delay_priors = FALSE,
                                verbose = TRUE,
                                ...,
                                obs) {

  if (!missing(obs)) {
    lifecycle::deprecate_stop(
      "1.5.0",
      "estimate_truncation(obs)",
      "estimate_truncation(data)"
    )
  }
  if (!is.null(model)) {
    lifecycle::deprecate_stop(
      "1.5.0",
      "estimate_truncation(model)",
      "estimate_truncation(stan)"
    )
  }
  if (!missing(weigh_delay_priors)) {
    lifecycle::deprecate_stop(
      "1.5.0",
      "estimate_truncation(weigh_delay_priors)",
      "trunc_opts(weight_prior)"
    )
  }
   # Validate inputs
  walk(data, check_reports_valid, model = "estimate_truncation")
  assert_class(truncation, "dist_spec")
  assert_class(model, "stanfit", null.ok = TRUE)
  assert_numeric(CrIs, lower = 0, upper = 1)
  assert_logical(filter_leading_zeros)
  assert_numeric(zero_threshold, lower = 0)
  assert_logical(weigh_delay_priors)
  assert_logical(verbose)

  ## code block to remove in next EpiNow2 version
  construct_trunc <- FALSE

  # combine into ordered matrix
  dirty_obs <- purrr::map(data, data.table::as.data.table)
  dirty_obs <- purrr::map(dirty_obs,
    create_clean_reported_cases,
      horizon = 0,
      filter_leading_zeros = filter_leading_zeros,
      zero_threshold = zero_threshold,
      add_breakpoints = FALSE
  )
  earliest_date <- max(
    as.Date(
      purrr::map_chr(dirty_obs, function(x) x[, as.character(min(date))])
    )
  )
  dirty_obs <- purrr::map(dirty_obs, function(x) x[date >= earliest_date])
  nrow_obs <- order(purrr::map_dbl(dirty_obs, nrow))
  dirty_obs <- dirty_obs[nrow_obs]
  obs <- purrr::map(dirty_obs, data.table::copy)
  obs <- purrr::map(seq_along(obs), ~ obs[[.]][, (as.character(.)) := confirm][
    ,
    confirm := NULL
  ])
  obs <- purrr::reduce(obs, merge, all = TRUE)
  obs_start <- max(nrow(obs) - max(truncation) - sum(is.na(obs$`1`)) + 1, 1)
  obs_dist <- purrr::map_dbl(2:(ncol(obs)), ~ sum(is.na(obs[[.]])))
  obs_data <- obs[, -1][, purrr::map(.SD, ~ ifelse(is.na(.), 0, .))]
  obs_data <- as.matrix(obs_data[obs_start:.N])

  # convert to stan list
  stan_data <- list(
    obs = obs_data,
    obs_dist = obs_dist,
    t = nrow(obs_data),
    obs_sets = ncol(obs_data)
  )

  stan_data <- c(stan_data, create_stan_delays(
    trunc = truncation,
    time_points = stan_data$t
  ))

  # initial conditions
  init_fn <- function() {
    data <- c(create_delay_inits(stan_data), list(
      phi = abs(rnorm(1, 0, 1)),
      sigma = abs(rnorm(1, 0, 1))
    ))
    return(data)
  }

  # fit
  args <- create_stan_args(
    stan = stan, data = stan_data, init = init_fn, model = "estimate_truncation"
  )
  fit <- fit_model(args, id = "estimate_truncation")

  out <- list()
  # Summarise fit truncation distribution for downstream usage
  delay_params <- extract_stan_param(fit, params = "delay_params")
  params_mean <- round(delay_params$mean, 3)
  params_sd <- round(delay_params$sd, 3)
  parameters <- purrr::map(seq_along(params_mean), function(id) {
    Normal(params_mean[id], params_sd[id])
  })
  names(parameters) <- natural_params(get_distribution(truncation))
  parameters$max <- max(truncation)
  out$dist <- new_dist_spec(
    params = parameters,
    distribution = get_distribution(truncation)
  )

  # summarise reconstructed observations
  recon_obs <- extract_stan_param(fit, "recon_obs",
    CrIs = CrIs,
    var_names = TRUE
  )
  recon_obs <- recon_obs[, id := variable][, variable := NULL]
  recon_obs <- recon_obs[, dataset := seq_len(.N)][
    ,
    dataset := dataset %% stan_data$obs_sets
  ][
    dataset == 0, dataset := stan_data$obs_sets
  ]
  # link reconstructed observations to observed
  last_obs <-
    data.table::copy(dirty_obs[[length(dirty_obs)]])[, last_confirm := confirm][
      ,
      confirm := NULL
    ]
  link_obs <- function(index) {
    target_obs <- dirty_obs[[index]][, index := .N - 0:(.N - 1)]
    target_obs <- target_obs[index < max(truncation)]
    estimates <- recon_obs[dataset == index][, c("id", "dataset") := NULL]
    estimates <- estimates[, lapply(.SD, as.integer)]
    estimates <- estimates[, index := .N - 0:(.N - 1)]
    if (!is.null(estimates$n_eff)) {
      estimates[, "n_eff" := NULL]
    }
    if (!is.null(estimates$Rhat)) {
      estimates[, "Rhat" := NULL]
    }

    target_obs <-
      data.table::merge.data.table(
        target_obs, last_obs,
        by = "date"
      )
    target_obs[, report_date := max(date)]
    target_obs <- data.table::merge.data.table(target_obs, estimates,
      by = "index", all.x = TRUE
    )
    target_obs <- target_obs[order(date)][, index := NULL]
    return(target_obs)
  }
  out$obs <- purrr::map(1:(stan_data$obs_sets), link_obs)
  out$obs <- data.table::rbindlist(out$obs)
  out$last_obs <- last_obs
  # summarise estimated cmf of the truncation distribution
  out$cmf <- extract_stan_param(fit, "trunc_rev_cmf", CrIs = CrIs)
  out$cmf <- data.table::as.data.table(out$cmf)[, index := seq_len(.N)]
  data.table::setcolorder(out$cmf, "index")
  out$data <- stan_data
  out$fit <- fit

  class(out) <- c("estimate_truncation", class(out))
  return(out)
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
#' @seealso plot estimate_truncation
#' @method plot estimate_truncation
#' @importFrom ggplot2 ggplot aes geom_col geom_point labs scale_x_date
#' @importFrom ggplot2 scale_y_continuous theme theme_bw
#' @export
plot.estimate_truncation <- function(x, ...) {
  plot <- ggplot2::ggplot(x$obs, ggplot2::aes(x = date, y = last_confirm)) +
    ggplot2::geom_col(
      fill = "grey", col = "white",
      show.legend = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = x$obs,
      ggplot2::aes(x = date, y = confirm)
    ) +
    ggplot2::facet_wrap(~report_date, scales = "free")

  plot <- plot_CrIs(plot, extract_CrIs(x$obs),
    alpha = 0.8, linewidth = 1
  )

  plot <- plot +
    ggplot2::theme_bw() +
    ggplot2::labs(
      y = "Confirmed Cases", x = "Date", col = "Type", fill = "Type"
    ) +
    ggplot2::scale_x_date(date_breaks = "day", date_labels = "%b %d") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  return(plot)
}
