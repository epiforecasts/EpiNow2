#' Adjust from Case Counts by Infection Date to Date of Report
#'
#' @description  `r lifecycle::badge("deprecated")`
#' Maps from cases by date of infection to date of report via date of
#' onset.
#' @param infections `<data.table>` containing a `date` variable and a numeric
#' `cases` variable.
#'
#' @param delay_defs A list of single row data.tables that each  defines a
#' delay distribution (model, parameters and maximum delay for each model).
#' See [lognorm_dist_def()] for an example of the structure.
#'
#' @param reporting_effect A numeric vector of length 7 that allows the scaling
#' of reported cases by the day on which they report (1 = Monday, 7 = Sunday).
#' By default no scaling occurs.
#'
#' @param reporting_model A function that takes a single numeric vector as an
#' argument and returns a single numeric vector. Can be used to apply stochastic
#' reporting effects. See the examples for details.
#'
#' @return A `data.table` containing a `date` variable (date of report) and a
#' `cases` variable. If `return_onset = TRUE` there will be a third variable
#' `reference` which indicates what the date variable refers to.
#' @keywords internal
#' @export
#' @inheritParams sample_approx_dist
#' @importFrom data.table setorder data.table data.table
#' @importFrom lubridate wday
#' @examples
#' \donttest{
#' # This function is deprecated and its functionality can now be accessed
#' # from [simulate_secondary()].
#' # Here are some examples of how to use [simulate_secondary()] to replace
#' # adjust_infection_to_report().
#'
#' # Old (using adjust_infection_to_report()):
#' # Define example case data
#' cases <- data.table::copy(example_confirmed)
#' cases <- cases[, cases := as.integer(confirm)]
#' # Define a simple reporting delay distribution
#' delay_def <- lognorm_dist_def(
#'  mean = 5, mean_sd = 1, sd = 3, sd_sd = 1,
#'  max_value = 30, samples = 1, to_log = TRUE
#' )
#' report <- adjust_infection_to_report(
#'  cases,
#'  delay_defs = list(delay_def),
#'  reporting_model = function(n) rpois(length(n), n)
#' )
#' print(report)
#'
#' # New (using simulate_secondary()):
#' cases <- data.table::copy(example_confirmed)
#' cases <- cases[, primary := as.integer(confirm)]
#' uncertain_delay <- LogNormal(
#'  mean = Normal(5, 1), sd = Normal(3, 1),
#'  max = 30
#' )
#' delay <- fix_dist(uncertain_delay, strategy = "sample")
#' report <- simulate_secondary(
#'  cases,
#'  delays = delay_opts(delay),
#'  obs = obs_opts(family = "poisson")
#' )
#' print(report)
#' }
adjust_infection_to_report <- function(infections, delay_defs,
                                       reporting_model, reporting_effect,
                                       type = "sample",
                                       truncate_future = TRUE) {
  deprecate_warn(
    when = "1.5.0",
    what = "adjust_infection_to_report()",
    with = "simulate_secondary()",
    details = c(
      "See equivalent examples using `simulate_secondary()`",
      "in ?adjust_infection_to_report.",
      "This function will be removed completely in the next version."
    )
  )
  # Reset DT Defaults on Exit
  set_dt_single_thread()

  ## deprecated
  sample_single_dist <- function(input, delay_def) {
    ## Define sample delay fn
    sample_delay_fn <- function(n, ...) {
      EpiNow2::dist_skel(
        n = n,
        model = delay_def$model[[1]],
        params = delay_def$params[[1]],
        max_value = delay_def$max_value[[1]],
        ...
      )
    }


    ## Infection to onset
    out <- suppressWarnings(EpiNow2::sample_approx_dist(
      cases = input,
      dist_fn = sample_delay_fn,
      max_value = delay_def$max_value,
      direction = "forwards",
      type = type,
      truncate_future = FALSE
    ))

    return(out)
  }

  sample_dist_spec <- function(input, delay_def) {
    ## Define sample delay fn
    sample_delay_fn <- function(n, dist, cum, ...) {
      fixed_dist <- discretise(fix_dist(delay_def, strategy = "sample"))
      if (dist) {
        fixed_dist[[1]]$pmf[n + 1]
      } else {
        sample(seq_along(fixed_dist[[1]]$pmf) - 1, size = n, replace = TRUE)
      }
    }

    ## Infection to onset
    out <- suppressWarnings(EpiNow2::sample_approx_dist(
      cases = input,
      dist_fn = sample_delay_fn,
      max_value = max(delay_def),
      direction = "forwards",
      type = type,
      truncate_future = FALSE
    ))

    return(out)
  }

  if (is(delay_defs, "dist_spec")) {
    report <- sample_dist_spec(infections, extract_single_dist(delay_defs, 1))
    if (length(delay_defs) > 1) {
      for (def in seq(2, length(delay_defs))) {
        report <- sample_dist_spec(report, extract_single_dist(delay_defs, def))
      }
    }
  } else {
    deprecate_warn(
      "1.5.0",
      "adjust_infection_to_report(delay_defs = 'should be a dist_spec')",
      details = "Specifying this as a list of data tables is deprecated."
    )
    report <- sample_single_dist(infections, delay_defs[[1]])
    if (length(delay_defs) > 1) {
      for (def in 2:length(delay_defs)) {
        report <- sample_single_dist(report, delay_defs[[def]])
      }
    }
  }
  ## Add a weekly reporting effect if present
  if (!missing(reporting_effect)) {
    reporting_effect <- data.table::data.table(
      day = 1:7,
      effect = reporting_effect
    )

    report <- report[, day := lubridate::wday(date, week_start = 1)]
    report <- report[reporting_effect, on = "day"]
    report <- report[, cases := as.integer(cases * effect)][
      ,
      `:=`(effect = NULL, day = NULL)
    ]

    report <- data.table::setorder(report, date)
  }

  if (!missing(reporting_model)) {
    report <- report[, cases := reporting_model(cases)]
  }

  ## Truncate reported cases by maximum infection date
  if (type == "sample" && truncate_future) {
    report <- report[date <= max(infections$date)]
  }
  return(report)
}

#' Specify a distribution.
#'
#' @description `r lifecycle::badge("deprecated")`
#' This function is deprecated as a user-facing function (while its
#' functionality is still used internally). Construct distributions using
#' the corresponding distribution function such as [Gamma()], [LogNormal()],
#' [Normal()] or [Fixed()] instead.
#'
#' @param distribution Character, defaults to "lognormal". The (discretised)
#' distribution to be used. Can be "lognormal", "gamma", "normal" or "fixed".
#' The corresponding parameters (defined in [natural_params()]) are passed
#' as `params_mean`,  and their uncertainty as `params_sd`.
#'
#' @param params_mean Numeric. Central values of the parameters of the
#' distribution as defined in [natural_params().
#'
#' @param params_sd Numeric. Standard deviations of the parameters of the
#' distribution as defined in [natural_params().
#'
#' @param max Numeric, maximum value of the distribution. The distribution will
#' be truncated at this value. Default: `Inf`, i.e. no maximum.
#'
#' @param mean Deprecated; use `params_mean` instead.
#'
#' @param sd Deprecated; use `params_mean` instead.
#'
#' @param mean_sd Deprecated; use `params_sd` instead.
#'
#' @param sd_sd Deprecated; use `params_sd` instead.
#'
#' @param pmf Numeric, a vector of values that represent the (nonparametric)
#' probability mass function of the delay (starting with 0); defaults to an
#' empty vector corresponding to a parametric specification of the distribution
#' (using \code{params_mean}, and \code{params_sd}.
#' @param fixed Deprecated, use [fix_dist()] instead.
#' @return A list of distribution options.
#' @importFrom rlang warn arg_match
#' @export
#' @keywords internal
dist_spec <- function(distribution = c(
                        "lognormal", "normal", "gamma", "fixed", "empty"
                      ),
                      params_mean = numeric(0), params_sd = numeric(0),
                      mean, sd = 0, mean_sd = 0, sd_sd = 0,
                      max = Inf, pmf = numeric(0), fixed = FALSE) {

  lifecycle::deprecate_warn(
    "1.5.0",
    "dist_spec()",
    details = c(
      paste0(
        "Please use distribution functions such as `Gamma()` or `Lognormal()` ",
        "instead."
      ),
      "The function will become internal only in version 2.0.0."
    )
  )
  ## check for deprecated parameters
  if (!missing(fixed)) {
    lifecycle::deprecate_warn(
      "1.5.0",
      "dist_spec(fixed)",
      "fix_dist()"
    )
    params_sd <- NULL
  }
  ## check for deprecated parameters
  if (!all(missing(mean), missing(sd), missing(mean_sd), missing(sd_sd)) &&
      (length(params_mean) > 0 || length(params_sd) > 0)) {
    stop("Distributional parameters should not be given as `mean`, `sd`, etc. ",
         "in addition to `params_mean` or `params_sd`")
  }
  distribution <- match.arg(distribution)
  ## check if distribution is given as empty and warn about deprecation if so
  if (distribution == "empty") {
    deprecate_warn(
      "1.5.0",
      "dist_spec(distribution = 'must not be \"empty\"')",
      details = "Please use `Fixed(0)` instead."
    )
  }

  if (!all(missing(mean), missing(sd), missing(mean_sd), missing(sd_sd))) {
    if (sd == 0 && mean_sd == 0 && sd_sd == 0) {
      distribution <- "fixed"
    }
    ## deprecated arguments given
    if (distribution == "lognormal") {
      params_mean <- c(meanlog = mean, sdlog = sd)
      params_sd <- c(meanlog = mean_sd, sdlog = sd_sd)
    } else if (distribution == "gamma") {
      temp_dist <- Gamma(
        mean = Normal(mean, mean_sd),
        sd = Normal(sd, sd_sd)
      )
      params_mean <- vapply(temp_dist[[1]]$parameters, mean, numeric(1))
      params_sd <- vapply(temp_dist[[1]]$parameters, sd_dist, numeric(1))
    } else if (distribution == "normal") {
      params_mean <- c(mean = mean, sd = sd)
      params_sd <- c(mean = mean_sd, sd = sd_sd)
    } else if (distribution == "fixed") {
      params_mean <- mean
    }
  }
  if (length(pmf) > 0) {
    if (!all(
      missing(mean), missing(sd), missing(mean_sd), missing(sd_sd),
      missing(params_mean), missing(params_sd)
    )) {
      stop("Distributional parameters should not be given in addition to `pmf`")
    }
    distribution <- "nonparametric"
    parameters <- list(pmf = pmf)
  } else {
    if (length(params_sd) == 0) {
      params_sd <- rep(0, length(params_mean))
    }
    parameters <- lapply(seq_along(params_mean), function(id) {
      Normal(params_mean[id], params_sd[id])
    })
    names(parameters) <- natural_params(distribution)
    parameters$max <- max
  }
  return(new_dist_spec(parameters, distribution))
}

#' Generate a Gamma Distribution Definition Based on Parameter Estimates
#'
#' @description `r lifecycle::badge("deprecated")`
#' Deprecated; use [Gamma()] instead to define a gamma distribution.
#'
#' @param shape Numeric, shape parameter of the gamma distribution.
#'
#' @param shape_sd Numeric, standard deviation of the shape parameter.
#'
#' @param scale Numeric, scale parameter of the gamma distribution.
#'
#' @param scale_sd  Numeric, standard deviation of the scale parameter.
#'
#' @param samples Numeric, number of sample distributions to generate.
#'
#' @importFrom truncnorm rtruncnorm
#' @return A `<data.table>` defining the distribution as used by [dist_skel()]
#' @keywords internal
#' @export
#' @inheritParams dist_skel
#' @inheritParams lognorm_dist_def
#' @examples
#' # using estimated shape and scale
#' def <- gamma_dist_def(
#'   shape = 5.807, shape_sd = 0.2,
#'   scale = 0.9, scale_sd = 0.05,
#'   max_value = 20, samples = 10
#' )
#' print(def)
#' def$params[[1]]
#'
#' # using mean and sd
#' def <- gamma_dist_def(
#'   mean = 3, mean_sd = 0.5,
#'   sd = 3, sd_sd = 0.1,
#'   max_value = 20, samples = 10
#' )
#' print(def)
#' def$params[[1]]
gamma_dist_def <- function(shape, shape_sd,
                           scale, scale_sd,
                           mean, mean_sd,
                           sd, sd_sd,
                           max_value, samples) {
  lifecycle::deprecate_warn(
    "1.5.0", "gamma_dist_def()", "Gamma()",
    "The function will be removed completely in version 2.0.0."
  )

  if (missing(shape) && missing(scale) && !missing(mean) && !missing(sd)) {
    if (!missing(mean_sd)) {
      mean <- truncnorm::rtruncnorm(samples, a = 0, mean = mean, sd = mean_sd)
    }
    if (!missing(sd_sd)) {
      sd <- truncnorm::rtruncnorm(samples, a = 0, mean = sd, sd = sd_sd)
    }
    scale <- sd^2 / mean
    shape <- mean / scale
  } else {
    if (!missing(shape_sd)) {
      shape <- truncnorm::rtruncnorm(
        samples,
        a = 0, mean = shape, sd = shape_sd
      )
    }
    if (!missing(scale_sd)) {
      scale <- 1 / truncnorm::rtruncnorm(
        samples,
        a = 0, mean = scale, sd = scale_sd
      )
    }
  }

  rate <- 1 / scale

  dist <- data.table::data.table(
    model = rep("gamma", samples),
    params = purrr::list_transpose(
      list(
        shape = shape,
        rate = rate
      )
    ),
    max_value = rep(max_value, samples)
  )
  return(dist)
}

#' Generate initial conditions by fitting to cumulative cases
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Fits a model to cumulative cases. This may be a useful approach to
#' initialising a full model fit for certain data sets where the sampler gets
#' stuck or cannot easily be initialised as fitting to cumulative cases changes
#' the shape of the posterior distribution. In `estimate_infections()`,
#' `epinow()` and `regional_epinow()` this option can be engaged by setting
#' `stan_opts(init_fit = "cumulative")`.
#'
#' This implementation is based on the approach taken in
#' [epidemia](https://github.com/ImperialCollegeLondon/epidemia/) authored by
#' James Scott.
#'
#' @param samples Numeric, defaults to 50. Number of posterior samples.
#'
#' @param warmup Numeric, defaults to 50. Number of warmup samples.
#'
#' @param verbose Logical, should fitting progress be returned. Defaults to
#' `FALSE`.
#'
#' @inheritParams create_initial_conditions
#' @importFrom rstan sampling
#' @importFrom futile.logger flog.debug
#' @importFrom utils capture.output
#' @inheritParams fit_model_with_nuts
#' @inheritParams stan_opts
#' @return A stanfit object
#' @keywords internal
init_cumulative_fit <- function(args, samples = 50, warmup = 50,
                                id = "init", verbose = FALSE,
                                backend = "rstan") {
  deprecate_warn(
    when = "1.5.0",
    what = "init_cumulative_fit()"
  )
  futile.logger::flog.debug(
    "%s: Fitting to cumulative data to initialise chains", id,
    name = "EpiNow2.epinow.estimate_infections.fit"
  )
  # copy main run settings and override to use only 100 iterations and a single
  # chain
  initial_args <- create_stan_args(
    stan = stan_opts(
      args$object,
      samples = samples,
      warmup = warmup,
      control = list(adapt_delta = 0.9, max_treedepth = 13),
      chains = 1,
      cores = 2,
      backend = backend,
      open_progress = FALSE,
      show_messages = FALSE
    ),
    data = args$data, init = args$init
  )
  # change observations to be cumulative in order to protect against noise and
  # give an approximate fit (though for Rt constrained to be > 1)
  initial_args$data$cases <- cumsum(initial_args$data$cases)
  initial_args$data$shifted_cases <- cumsum(initial_args$data$shifted_cases)

  # initial fit
  if (verbose) {
    fit <- fit_model(initial_args, id = "init_cumulative")
  } else {
    out <- tempfile(tmpdir = tempdir(check = TRUE))
    capture.output(
      {
        fit <- fit_model(initial_args, id = "init_cumulative")
      },
      type = c("output", "message"),
      split = FALSE,
      file = out
    )
  }
  return(fit)
}

#' Generate a Log Normal Distribution Definition Based on Parameter Estimates
#'
#' @description `r lifecycle::badge("deprecated")`
#' Generates a distribution definition when only parameter estimates
#' are available for log normal distributed parameters. See [rlnorm()] for
#' distribution information.
#'
#' @param mean Numeric, log mean parameter of the gamma distribution.
#'
#' @param mean_sd Numeric, standard deviation of the log mean parameter.
#'
#' @param sd Numeric, log sd parameter of the gamma distribution.
#'
#' @param sd_sd  Numeric, standard deviation of the log sd parameter.
#'
#' @param samples Numeric, number of sample distributions to generate.
#'
#' @param to_log Logical, should parameters be logged before use.
#'
#' @return A `<data.table>` defining the distribution as used by [dist_skel()]
#' @importFrom truncnorm rtruncnorm
#' @export
#' @keywords internal
#' @inheritParams dist_skel
#' @examples
#' def <- lognorm_dist_def(
#'   mean = 1.621, mean_sd = 0.0640,
#'   sd = 0.418, sd_sd = 0.0691,
#'   max_value = 20, samples = 10
#' )
#' print(def)
#' def$params[[1]]
#'
#' def <- lognorm_dist_def(
#'   mean = 5, mean_sd = 1,
#'   sd = 3, sd_sd = 1,
#'   max_value = 20, samples = 10,
#'   to_log = TRUE
#' )
#' print(def)
#' def$params[[1]]
lognorm_dist_def <- function(mean, mean_sd,
                             sd, sd_sd,
                             max_value, samples,
                             to_log = FALSE) {
  lifecycle::deprecate_warn(
    "1.5.0", "lognorm_dist_def()", "LogNormal()",
    "The function will be removed completely in version 2.0.0."
  )

  transform_mean <- function(mu, sig) {
    mean_location <- log(mu^2 / sqrt(sig^2 + mu^2))
    mean_location
  }

  transform_sd <- function(mu, sig) {
    mean_shape <- sqrt(log(1 + (sig^2 / mu^2)))
    mean_shape
  }

  if (missing(mean_sd)) {
    sampled_means <- mean
  } else {
    sampled_means <- truncnorm::rtruncnorm(
      samples,
      a = 0, mean = mean, sd = mean_sd
    )
  }

  if (missing(sd_sd)) {
    sampled_sds <- sd
  } else {
    sampled_sds <- truncnorm::rtruncnorm(samples, a = 0, mean = sd, sd = sd_sd)
  }
  means <- sampled_means
  sds <- sampled_sds

  if (to_log) {
    means <- mapply(transform_mean, sampled_means, sampled_sds)
    sds <- mapply(transform_sd, sampled_means, sampled_sds)
  }

  dist <- data.table::data.table(
    model = rep("lognormal", samples),
    params = purrr::list_transpose(
      list(
        meanlog = means,
        sdlog = sds
      )
    ),
    max_value = rep(max_value, samples)
  )
  return(dist)
}

#' Report case counts by date of report
#'
#' @description `r lifecycle::badge("deprecated")`
#' Convolves latent infections to reported cases via an observation model.
#' Likely to be removed/replaced in later releases by functionality drawing on
#' the `stan` implementation.
#'
#' @param case_estimates A data.table of case estimates with the following
#' variables: date, sample, cases
#'
#' @param case_forecast A data.table of case forecasts with the following
#' variables: date, sample, cases. If not supplied the default is not to
#' incorporate forecasts.
#'
#' @param reporting_effect A `data.table` giving the weekly reporting effect
#'  with the following variables: `sample` (must be the same as in `nowcast`),
#'  `effect` (numeric scaling factor for each weekday),`day` (numeric 1 - 7
#'  (1 = Monday and 7 = Sunday)). If not supplied then no weekly reporting
#'  effect is assumed.
#'
#' @return A list of `data.table`s. The first entry contains the following
#' variables `sample`, `date` and `cases` with the second being summarised
#' across samples.
#'
#' @keywords internal
#' @export
#' @inheritParams estimate_infections
#' @inheritParams adjust_infection_to_report
#' @importFrom data.table data.table rbindlist
#' @importFrom future.apply future_lapply
#' @examples
#' \donttest{
#' # This function is deprecated and its functionality can now be accessed
#' # from [simulate_secondary()].
#' # Here are some examples of how to use [simulate_secondary()] to replace
#' # report_cases().
#' # Old (using report_cases()):
#' # Define case data
#' cases <- example_confirmed[1:40]
#' cases <- cases[, cases := as.integer(confirm)]
#' cases <- cases[, confirm := NULL][, sample := 1]
#' reported_cases <- report_cases(
#'  case_estimates = cases,
#'  delays = delay_opts(example_incubation_period + example_reporting_delay),
#'  type = "sample"
#' )
#' print(reported_cases$samples)
#'
#' # New (using simulate_secondary()):
#' cases <- example_confirmed[1:40]
#' cases <- cases[, primary := as.integer(confirm)]
#' report <- simulate_secondary(
#'  cases,
#'  delays = delay_opts(
#'    fix_dist(example_incubation_period + example_reporting_delay)
#'  ),
#'  obs = obs_opts(family = "poisson")
#' )
#' print(report)
#' }
report_cases <- function(case_estimates,
                         case_forecast = NULL,
                         delays,
                         type = "sample",
                         reporting_effect,
                         CrIs = c(0.2, 0.5, 0.9)) {
  deprecate_warn(
    when = "1.5.0",
    what = "report_cases()",
    with = "simulate_secondary()",
    details = c(
      "See equivalent examples using `simulate_secondary()`",
      "in ?report_cases.",
      "This function will be removed completely in version 2.0.0."
    )
  )
  samples <- length(unique(case_estimates$sample))

  # add a null reporting effect if missing
  if (missing(reporting_effect)) {
    reporting_effect <- data.table::data.table(
      sample = list(1:samples),
      effect = rep(1, 7),
      day = 1:7
    )
    reporting_effect <- reporting_effect[,
      .(sample = unlist(sample)), by = .(effect, day)
    ]
  }
  # filter and sum nowcast to use only upscaled cases by date of infection
  infections <- data.table::copy(case_estimates)

  # add in case forecast if present
  if (!is.null(case_forecast)) {
    infections <- data.table::rbindlist(list(
      infections,
      case_forecast[, .(date, sample, cases = as.integer(cases))]
    ), use.names = TRUE)
  }

  ## For each sample map to report date
  report <- future.apply::future_lapply(1:max(infections$sample),
    function(id) {
      suppressWarnings(
        EpiNow2::adjust_infection_to_report(infections[sample == id],
          delay_defs = delays,
          type = type,
          reporting_effect = reporting_effect[sample == id, ]$effect
        )
      )
    },
    future.seed = TRUE
  )

  report <- data.table::rbindlist(report, idcol = "sample")

  out <- list()
  # bind all samples together
  out$samples <- report
  # summarise samples
  out$summarised <- calc_summary_measures(
    report[, value := cases][, cases := NULL],
    summarise_by = "date",
    order_by = "date",
    CrIs = CrIs
  )
  return(out)
}

#' Approximate Sampling a Distribution using Counts
#'
#' @description `r lifecycle::badge("deprecated")`
#' Convolves cases by a PMF function. This function will soon be removed or
#' replaced with a more robust stan implementation.
#'
#' @param cases A `<data.frame>` of cases (in date order) with the following
#' variables: `date` and `cases`.
#'
#' @param max_value Numeric, maximum value to allow. Defaults to 120 days
#'
#' @param direction Character string, defato "backwards". Direction in which to
#' map cases. Supports either "backwards" or "forwards".
#'
#' @param dist_fn Function that takes two arguments with the first being
#' numeric and the second being logical (and defined as `dist`). Should return
#' the probability density or a sample from the defined distribution. See
#' the examples for more.
#'
#' @param earliest_allowed_mapped A character string representing a date
#' ("2020-01-01"). Indicates the earliest allowed mapped value.
#'
#' @param type Character string indicating the method to use to transform
#' counts. Supports either "sample"  which approximates sampling or "median"
#' would shift by the median of the distribution.
#'
#' @param truncate_future Logical, should cases be truncated if they occur
#' after the first date reported in the data. Defaults to `TRUE`.
#'
#' @return A `<data.table>` of cases by date of onset
#' @keywords internal
#' @export
#' @importFrom purrr map_dfc
#' @importFrom data.table data.table setorder
#' @importFrom lubridate days
#' @examples
#' \donttest{
#' cases <- example_confirmed
#' cases <- cases[, cases := as.integer(confirm)]
#' print(cases)
#'
#' # total cases
#' sum(cases$cases)
#'
#' delay_fn <- function(n, dist, cum) {
#'   if (dist) {
#'     pgamma(n + 0.9999, 2, 1) - pgamma(n - 1e-5, 2, 1)
#'   } else {
#'     as.integer(rgamma(n, 2, 1))
#'   }
#' }
#'
#' onsets <- sample_approx_dist(
#'   cases = cases,
#'   dist_fn = delay_fn
#' )
#'
#' # estimated onset distribution
#' print(onsets)
#'
#' # check that sum is equal to reported cases
#' total_onsets <- median(
#'   purrr::map_dbl(
#'     1:10,
#'     ~ sum(sample_approx_dist(
#'       cases = cases,
#'       dist_fn = delay_fn
#'     )$cases)
#'   )
#' )
#' total_onsets
#'
#'
#' # map from onset cases to reported
#' reports <- sample_approx_dist(
#'   cases = cases,
#'   dist_fn = delay_fn,
#'   direction = "forwards"
#' )
#'
#'
#' # map from onset cases to reported using a mean shift
#' reports <- sample_approx_dist(
#'   cases = cases,
#'   dist_fn = delay_fn,
#'   direction = "forwards",
#'   type = "median"
#' )
#' }
sample_approx_dist <- function(cases = NULL,
                               dist_fn = NULL,
                               max_value = 120,
                               earliest_allowed_mapped = NULL,
                               direction = "backwards",
                               type = "sample",
                               truncate_future = TRUE) {
  deprecate_warn(
    "1.5.0",
    "sample_approx_dist()",
    details = "The function will be removed completely in the next version."
  )
  if (type == "sample") {
    if (direction == "backwards") {
      direction_fn <- rev
    } else if (direction == "forwards") {
      direction_fn <- function(x) {
        x
      }
    }
    # reverse cases so starts with current first
    reversed_cases <- direction_fn(cases$cases)
    reversed_cases[is.na(reversed_cases)] <- 0
    # draw from the density fn of the dist
    draw <- dist_fn(0:max_value, dist = TRUE, cum = FALSE)

    # approximate cases
    mapped_cases <- do.call(cbind, purrr::map(
      seq_along(reversed_cases),
      ~ c(
        rep(0, . - 1),
        stats::rbinom(
          length(draw),
          rep(reversed_cases[.], length(draw)),
          draw
        ),
        rep(0, length(reversed_cases) - .)
      )
    ))


    # set dates order based on direction mapping
    if (direction == "backwards") {
      dates <- seq(min(cases$date) - lubridate::days(length(draw) - 1),
        max(cases$date),
        by = "days"
      )
    } else if (direction == "forwards") {
      dates <- seq(min(cases$date),
        max(cases$date) + lubridate::days(length(draw) - 1),
        by = "days"
      )
    }

    # summarises movements and sample for placement of non-integer cases
    case_sum <- direction_fn(rowSums(mapped_cases))
    floor_case_sum <- floor(case_sum)
    sample_cases <- floor_case_sum +
      as.numeric((runif(seq_along(case_sum)) < (case_sum - floor_case_sum)))

    # summarise imputed onsets and build output data.table
    mapped_cases <- data.table::data.table(
      date = dates,
      cases = sample_cases
    )

    # filter out all zero cases until first recorded case
    mapped_cases <- data.table::setorder(mapped_cases, date)
    mapped_cases <- mapped_cases[
      ,
      cum_cases := cumsum(cases)
    ][cum_cases != 0][, cum_cases := NULL]
  } else if (type == "median") {
    shift <- as.integer(
      median(as.integer(dist_fn(1000, dist = FALSE)), na.rm = TRUE)
    )

    if (direction == "backwards") {
      mapped_cases <- data.table::copy(cases)[
        ,
        date := date - lubridate::days(shift)
      ]
    } else if (direction == "forwards") {
      mapped_cases <- data.table::copy(cases)[
        ,
        date := date + lubridate::days(shift)
      ]
    }
  }

  if (!is.null(earliest_allowed_mapped)) {
    mapped_cases <- mapped_cases[date >= as.Date(earliest_allowed_mapped)]
  }

  # filter out future cases
  if (direction == "forwards" && truncate_future) {
    max_date <- max(cases$date)
    mapped_cases <- mapped_cases[date <= max_date]
  }
  return(mapped_cases)
}
