#' Estimate Truncation of Observed Data
#'
#' @description `r lifecycle::badge("stable")`
#' Estimates a truncation distribution from multiple snapshots of the same
#' data source over time. This distribution can then be used in
#' `regional_epinow`, `epinow`, and `estimate_infections` to adjust for
#' truncated data. See
#' [here](https://gist.github.com/seabbs/176b0c7f83eab1a7192a25b28bbd116a)
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
#' @param obs A list of data frames each containing a date variable
#' and a confirm (integer) variable. Each data set should be a snapshot
#' of the reported data over time. All data sets must contain a complete vector
#' of dates.
#'
#' @param max_truncation Deprecated; use `trunc_max` instead.
#'
#' @param trunc_max Integer, defaults to 10. Maximum number of
#' days to include in the truncation distribution.
#'
#' @param trunc_dist Character, defaults to "lognormal". The parametric
#' distribution to be used for truncation.
#'
#' @param model A compiled stan model to override the default model. May be
#' useful for package developers or those developing extensions.
#'
#' @param verbose Logical, should model fitting progress be returned.
#'
#' @param ... Additional parameters to pass to `rstan::sampling`.
#'
#' @return A list containing: the summary parameters of the truncation
#'  distribution (`dist`), the estimated CMF of the truncation distribution
#' (`cmf`, can be used to adjusted new data), a data frame containing the
#' observed truncated data, latest observed data and the adjusted for
#' truncation observations (`obs`), a data frame containing the last
#' observed data (`last_obs`, useful for plotting and validation), the data
#' used for fitting (`data`) and the fit object (`fit`).
#'
#' @author Sam Abbott
#' @export
#' @inheritParams calc_CrIs
#' @importFrom purrr map reduce map_dbl
#' @importFrom rstan sampling
#' @importFrom data.table copy .N as.data.table merge.data.table setDT setcolorder
#' @examples
#' # set number of cores to use
#' old_opts <- options()
#' options(mc.cores = ifelse(interactive(), 4, 1))
#'
#' # get example case counts
#' reported_cases <- example_confirmed[1:60]
#'
#' # define example truncation distribution (note not integer adjusted)
#' trunc <- list(
#'   mean = convert_to_logmean(3, 2),
#'   mean_sd = 0.1,
#'   sd = convert_to_logsd(3, 2),
#'   sd_sd = 0.1,
#'   max = 10
#' )
#'
#' # apply truncation to example data
#' construct_truncation <- function(index, cases, dist) {
#'   set.seed(index)
#'   cmf <- cumsum(
#'     dlnorm(
#'       1:(dist$max + 1),
#'       rnorm(1, dist$mean, dist$mean_sd),
#'       rnorm(1, dist$sd, dist$sd_sd)
#'     )
#'   )
#'   cmf <- cmf / cmf[dist$max + 1]
#'   cmf <- rev(cmf)[-1]
#'   trunc_cases <- data.table::copy(cases)[1:(.N - index)]
#'   trunc_cases[(.N - length(cmf) + 1):.N, confirm := as.integer(confirm * cmf)]
#'   return(trunc_cases)
#' }
#' example_data <- purrr::map(c(20, 15, 10, 0),
#'   construct_truncation,
#'   cases = reported_cases,
#'   dist = trunc
#' )
#'
#' # fit model to example data
#' est <- estimate_truncation(example_data,
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
#' options(old_opts)
estimate_truncation <- function(obs, max_truncation, trunc_max = 10,
                                trunc_dist = c("lognormal"),
                                model = NULL,
                                CrIs = c(0.2, 0.5, 0.9),
                                verbose = TRUE,
                                ...) {
  trunc_dist <- match.arg(trunc_dist)

  if (!missing(max_truncation) && missing(trunc_max)) {
    warning(
      "The `max_truncation` argument is deprecated. ",
      "Use `trunc_max` instead."
    )
    trunc_max <- max_truncation
  }

  # combine into ordered matrix
  dirty_obs <- purrr::map(obs, data.table::as.data.table)
  nrow_obs <- order(purrr::map_dbl(dirty_obs, nrow))
  dirty_obs <- dirty_obs[nrow_obs]
  obs <- purrr::map(dirty_obs, data.table::copy)
  obs <- purrr::map(seq_along(obs), ~ obs[[.]][, (as.character(.)) := confirm][
    ,
    confirm := NULL
  ])
  obs <- purrr::reduce(obs, merge, all = TRUE)
  obs_start <- nrow(obs) - trunc_max - sum(is.na(obs$`1`)) + 1
  obs_dist <- purrr::map_dbl(2:(ncol(obs)), ~ sum(is.na(obs[[.]])))
  obs_data <- obs[, -1][, purrr::map(.SD, ~ ifelse(is.na(.), 0, .))]
  obs_data <- as.matrix(obs_data[obs_start:.N])

  # convert to stan list
  data <- list(
    obs = obs_data,
    obs_dist = obs_dist,
    t = nrow(obs_data),
    obs_sets = ncol(obs_data),
    trunc_max = trunc_max,
    trunc_dist = trunc_dist
  )

  ## convert to integer
  data$trunc_dist <-
    which(eval(formals()[["trunc_dist"]]) == trunc_dist) - 1

  # initial conditions
  init_fn <- function() {
    data <- list(
      logmean = rnorm(1, 0, 1),
      logsd = abs(rnorm(1, 0, 1)),
      phi = abs(rnorm(1, 0, 1)),
      sigma = abs(rnorm(1, 0, 1))
    )
    return(data)
  }

  # fit
  if (is.null(model)) {
    model <- epinow2_model("estimate_truncation")
  }
  fit <- model$sample(
    data = data,
    init = init_fn,
    refresh = ifelse(verbose, 50, 0),
    ...
  )

  out <- list()
  # Summarise fit truncation distribution for downstream usage
  out$dist <- list(
    mean = round(fit$summary(variables = "logmean")[[2]], 3),
    mean_sd = round(fit$summary(variables = "logmean")[[4]], 3),
    sd = round(fit$summary(variables = "logsd")[[2]], 3),
    sd_sd = round(fit$summary(variables = "logsd")[[4]], 3),
    max = trunc_max
  )

  # summarise reconstructed observations
  recon_obs <- extract_stan_param(fit, "recon_obs",
    CrIs = CrIs,
    var_names = TRUE
  )
  recon_obs <- recon_obs[, id := variable][, variable := NULL]
  recon_obs <- recon_obs[, dataset := 1:.N][
    ,
    dataset := dataset %% data$obs_sets
  ][
    dataset == 0, dataset := data$obs_sets
  ]
  # link reconstructed observations to observed
  last_obs <-
    data.table::copy(dirty_obs[[length(dirty_obs)]])[, last_confirm := confirm][
      ,
      confirm := NULL
    ]
  link_obs <- function(index) {
    target_obs <- dirty_obs[[index]][, index := .N - 0:(.N - 1)]
    target_obs <- target_obs[index < trunc_max]
    estimates <- recon_obs[dataset == index][, c("id", "dataset") := NULL]
    estimates <- estimates[, lapply(.SD, as.integer)]
    estimates <- estimates[, index := .N - 0:(.N - 1)]
    if (!is.null(estimates$n_eff)) {
      estimates[, c("n_eff") := NULL]
    }
    if (!is.null(estimates$Rhat)) {
      estimates[, c("Rhat") := NULL]
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
  out$obs <- purrr::map(1:(data$obs_sets), link_obs)
  out$obs <- data.table::rbindlist(out$obs)
  out$last_obs <- last_obs
  # summarise estimated cmf of the truncation distribution
  out$cmf <- extract_stan_param(fit, "rev_cmf", CrIs = CrIs)
  out$cmf <- data.table::as.data.table(out$cmf)[, index := seq_len(.N)]
  data.table::setcolorder(out$cmf, "index")
  out$data <- data
  out$fit <- fit

  class(out) <- c("estimate_truncation", class(out))
  return(out)
}

#' Plot method for estimate_truncation
#'
#' @description `r lifecycle::badge("experimental")`
#' `plot` method for class "estimate_truncation". Returns
#' a plot faceted over each dataset used in fitting with the latest
#' observations as columns, the data observed at the time (and so truncated)
#' as dots and the truncation adjusted estimates as a ribbon.
#'
#' @param x A list of output as produced by `estimate_truncation`
#'
#' @param ... Pass additional arguments to plot function. Not currently in use.
#'
#' @return `ggplot2` object
#' @author Sam Abbott
#' @seealso plot estimate_truncation
#' @method plot estimate_truncation
#' @importFrom ggplot2 ggplot aes geom_col geom_point labs scale_x_date scale_y_continuous theme theme_bw
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
    ggplot2::labs(y = "Confirmed Cases", x = "Date", col = "Type", fill = "Type") +
    ggplot2::scale_x_date(date_breaks = "day", date_labels = "%b %d") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  return(plot)
}
