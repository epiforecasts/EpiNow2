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
adjust_infection_to_report <- function(infections, delay_defs,
                                       reporting_model, reporting_effect,
                                       type = "sample",
                                       truncate_future = TRUE) {
  deprecate_stop(
    when = "1.5.0",
    what = "adjust_infection_to_report()",
    with = "simulate_secondary()",
    details = c(
      "See equivalent examples using `simulate_secondary()`",
      "in ?adjust_infection_to_report."
    )
  )
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
                      max = Inf, pmf = numeric(0), fixed = FALSE) {

  lifecycle::deprecate_stop(
    "1.5.0",
    "dist_spec()",
    details = c(
      paste0(
        "Please use distribution functions such as `Gamma()` or `Lognormal()` ",
        "instead."
      )
    )
  )
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
gamma_dist_def <- function(shape, shape_sd,
                           scale, scale_sd,
                           mean, mean_sd,
                           sd, sd_sd,
                           max_value, samples) {
  lifecycle::deprecate_stop(
    "1.5.0", "gamma_dist_def()", "Gamma()"
  )
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
  deprecate_stop(
    when = "1.5.0",
    what = "init_cumulative_fit()"
  )
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
lognorm_dist_def <- function(mean, mean_sd,
                             sd, sd_sd,
                             max_value, samples,
                             to_log = FALSE) {
  lifecycle::deprecate_stop(
    "1.5.0", "lognorm_dist_def()", "LogNormal()"
  )
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
report_cases <- function(case_estimates,
                         case_forecast = NULL,
                         delays,
                         type = "sample",
                         reporting_effect,
                         CrIs = c(0.2, 0.5, 0.9)) {
  deprecate_stop(
    when = "1.5.0",
    what = "report_cases()",
    with = "simulate_secondary()",
    details = c(
      "See equivalent examples using `simulate_secondary()`",
      "in ?report_cases."
    )
  )
}

#' Approximate Sampling a Distribution using Counts
#'
#' @description `r lifecycle::badge("deprecated")`
#' Deprecated; Convolves cases by a PMF function.
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
sample_approx_dist <- function(cases = NULL,
                               dist_fn = NULL,
                               max_value = 120,
                               earliest_allowed_mapped = NULL,
                               direction = "backwards",
                               type = "sample",
                               truncate_future = TRUE) {
  deprecate_stop(
    "1.5.0",
    "sample_approx_dist()"
  )
}

#' Get a Literature Distribution
#'
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated. Please specify a distribution
#' using functions such as [Gamma()] or [LogNormal()] instead.
#'
#' @param data A `<data.table>` in the format of `generation_times`.
#'
#' @param disease A character string indicating the disease of interest.
#'
#' @param source A character string indicating the source of interest.
#'
#' @param max_value Numeric, the maximum value to allow. Defaults to 14 days.
#'
#' @param fixed Logical, defaults to `FALSE`. Should distributions be supplied
#' as fixed values (vs with uncertainty)?
#'
#' @return A list defining a distribution
#'
#' @seealso [dist_spec()]
#' @export
#' @keywords internal
get_dist <- function(data, disease, source, max_value = 14, fixed = FALSE) {
  lifecycle::deprecate_stop(
    "1.5.0", "get_dist()",
    details = c(
      paste(
        "Please use distribution functions such as `Gamma()` or `Lognormal()`",
        "instead."
      )
    )
  )
}

#' Get a Literature Distribution for the Generation Time
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Extracts a literature distribution from `generation_times`.
#' This function has been deprecated. Please specify a distribution
#' using functions such as [Gamma()] or [LogNormal()] instead.
#'
#' @inheritParams get_dist
#' @inherit get_dist
#' @export
#' @seealso [dist_spec()]
#' @keywords internal
get_generation_time <- function(disease, source, max_value = 14,
                                fixed = FALSE) {
  lifecycle::deprecate_stop(
    "1.5.0", "get_generation_time()",
    details = c(
      paste(
        "Please use distribution functions such as `Gamma()` or `Lognormal()`",
        "instead."
      ),
      paste(
        "To obtain the previous estimate by Ganyani et al. (2020) use",
        "`example_generation_time`."
      )
    )
  )
}

#'  Get a Literature Distribution for the Incubation Period
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Extracts a literature distribution from `generation_times`.
#' This function has been deprecated. Please specify a distribution
#' using functions such as [Gamma()] or [LogNormal()] instead.
#'
#' @inheritParams get_dist
#' @inherit get_dist
#' @export
#' @keywords internal
get_incubation_period <- function(disease, source, max_value = 14,
                                  fixed = FALSE) {
  lifecycle::deprecate_stop(
    "1.5.0", "get_incubation_period()",
    details = c(
      paste(
        "Please use distribution functions such as `Gamma()` or `Lognormal()`",
        "instead."
      ),
     paste(
      "To obtain the previous estimate by Ganyani et al. (2020) use",
      "`example_incubation_period`."
     )
    )
  )
}

#' Rstan Sampling Options
#'
#' @description `r lifecycle::badge("deprecated")`
#' Deprecated; use [stan_sampling_opts()] instead.
#' @param ... Additional parameters to pass to [rstan::sampling()].
#' @inheritParams stan_sampling_opts
#' @return A list of arguments to pass to [rstan::sampling()].
#' @export
rstan_sampling_opts <- function(cores = getOption("mc.cores", 1L),
                                warmup = 250,
                                samples = 2000,
                                chains = 4,
                                control = list(),
                                save_warmup = FALSE,
                                seed = as.integer(runif(1, 1, 1e8)),
                                future = FALSE,
                                max_execution_time = Inf,
                                ...) {
  lifecycle::deprecate_stop(
    "1.5.0", "rstan_sampling_opts()",
    "stan_sampling_opts()"
  )
}

#' Rstan Variational Bayes Options
#'
#' @description `r lifecycle::badge("deprecated")`
#' Deprecated; use [stan_vb_opts()] instead.
#' @inheritParams stan_vb_opts
#' @return A list of arguments to pass to [rstan::vb()].
#' @export
rstan_vb_opts <- function(samples = 2000,
                          trials = 10,
                          iter = 10000, ...) {
  lifecycle::deprecate_stop(
    "1.5.0", "rstan_vb_opts()",
    "stan_vb_opts()"
  )
}

#' Rstan Options
#'
#' @description `r lifecycle::badge("deprecated")`
#' Deprecated; specify options in [stan_opts()] instead.
#'
#' @param object Stan model object. By default uses the compiled package
#' default.
#'
#' @param method A character string, defaulting to sampling. Currently supports
#' [rstan::sampling()] ("sampling") or [rstan::vb()].
#'
#' @param ... Additional parameters to pass  underlying option functions.
#' @importFrom rlang arg_match
#' @return A list of arguments to pass to the appropriate rstan functions.
#' @export
#' @inheritParams rstan_sampling_opts
#' @seealso [rstan_sampling_opts()] [rstan_vb_opts()]
rstan_opts <- function(object = NULL,
                       samples = 2000,
                       method = c("sampling", "vb"), ...) {
  lifecycle::deprecate_stop(
    "1.5.0", "rstan_opts()",
    "stan_opts()"
  )
}

#' Distribution Skeleton
#'
#' @description `r lifecycle::badge("deprecated")`
#' This function acts as a skeleton for a truncated distribution defined by
#' model type, maximum value and model parameters. It is designed to be used
#' with the output from [get_dist()].
#'
#' @param n Numeric vector, number of samples to take (or days for the
#' probability density).
#'
#' @param dist Logical, defaults to `FALSE`. Should the probability density be
#' returned rather than a number of samples.
#'
#' @param cum Logical, defaults to `TRUE`. If `dist = TRUE` should the returned
#' distribution be cumulative.
#'
#' @param model Character string, defining the model to be used. Supported
#'   options are exponential ("exp"), gamma ("gamma"), and log normal
#'   ("lognormal")
#'
#' @param discrete Logical,  defaults to `FALSE`. Should the probability
#'   distribution be discretised. In this case each entry of the probability
#'   mass function corresponds to the 2-length interval ending at the entry
#'   except for the first interval that covers (0, 1).  That is, the probability
#'   mass function is a vector where the first entry corresponds to the integral
#'   over the (0,1] interval of the continuous distribution, the second entry
#'   corresponds to the (0,2] interval, the third entry corresponds to the (1,
#'   3] interval etc.
#'
#' @param params A list of parameters values (by name) required for each model.
#' For the exponential model this is a rate parameter and for the gamma model
#' this is alpha and beta.
#'
#' @param max_value Numeric, the maximum value to allow. Defaults to 120.
#' Samples outside of this range are resampled.
#'
#' @return A vector of samples or a probability distribution.
#' @export
#' @examples
#'
#' ## Exponential model
#' # sample
#' dist_skel(10, model = "exp", params = list(rate = 1))
#'
#' # cumulative prob density
#' dist_skel(1:10, model = "exp", dist = TRUE, params = list(rate = 1))
#'
#' # probability density
#' dist_skel(1:10,
#'   model = "exp", dist = TRUE,
#'   cum = FALSE, params = list(rate = 1)
#' )
#'
#' ## Gamma model
#' # sample
#' dist_skel(10, model = "gamma", params = list(shape = 1, rate = 0.5))
#'
#' # cumulative prob density
#' dist_skel(0:10,
#'   model = "gamma", dist = TRUE,
#'   params = list(shape = 1, rate = 0.5)
#' )
#'
#' # probability density
#' dist_skel(0:10,
#'   model = "gamma", dist = TRUE,
#'   cum = FALSE, params = list(shape = 2, rate = 0.5)
#' )
#'
#' ## Log normal model
#' # sample
#' dist_skel(10,
#'   model = "lognormal", params = list(meanlog = log(5), sdlog = log(2))
#' )
#'
#' # cumulative prob density
#' dist_skel(0:10,
#'   model = "lognormal", dist = TRUE,
#'   params = list(meanlog = log(5), sdlog = log(2))
#' )
#'
#' # probability density
#' dist_skel(0:10,
#'   model = "lognormal", dist = TRUE, cum = FALSE,
#'   params = list(meanlog = log(5), sdlog = log(2))
#' )
dist_skel <- function(n, dist = FALSE, cum = TRUE, model,
                      discrete = FALSE, params, max_value = 120) {
  lifecycle::deprecate_warn(
    "1.6.0", "dist_skel()"
  )
  ## define unnormalised support function
  if (model == "exp") {
    updist <- function(n) {
      pexp(n, params[["rate"]])
    }
  } else if (model == "gamma") {
    updist <- function(n) {
      pgamma(n, params[["shape"]], params[["rate"]])
    }
  } else if (model == "lognormal") {
    updist <- function(n) {
      plnorm(n, params[["meanlog"]], params[["sdlog"]])
    }
  } else if (model == "normal") {
    updist <- function(n) {
      pnorm(n, params[["mean"]], params[["sd"]])
    }
  } else if (model == "fixed") {
    updist <- function(n) {
      as.integer(n > params[["value"]])
    }
  }

  if (discrete) {
    cmf <- c(0, updist(1),
      updist(seq_len(max_value)) + updist(seq_len(max_value) + 1)
    ) /
      (updist(max_value) + updist(max_value + 1))
    pmf <- diff(cmf)
    rdist <- function(n) {
      sample(
        x = seq_len(max_value + 1) - 1, size = n, prob = pmf, replace = TRUE
      )
    }
    pdist <- function(n) {
      cmf[n + 1]
    }
    ddist <- function(n) {
      pmf[n + 1]
    }
  } else {
    pdist <- function(n) {
      updist(n) / updist(max_value + 1)
    }
    ddist <- function(n) {
      pdist(n + 1) - pdist(n)
    }
    if (model == "exp") {
      rdist <- function(n) {
        rexp(n, params[["rate"]])
      }
    } else if (model == "gamma") {
      rdist <- function(n) {
        rgamma(n, params[["shape"]], params[["rate"]])
      }
    } else if (model == "lognormal") {
      rdist <- function(n) {
        rlnorm(n, params[["meanlog"]], params[["sdlog"]])
      }
    }
  }

  # define internal sampling function
  inner_skel <- function(n, dist = FALSE, cum = TRUE, max_value = NULL) {
    if (dist) {
      if (cum) {
        ret <- pdist(n)
      } else {
        ret <- ddist(n)
      }
      ret[ret > 1] <- NA_real_
      return(ret)
    } else {
      rdist(n)
    }
  }

  # define truncation wrapper
  truncated_skel <- function(n, dist, cum, max_value) {
    n <- inner_skel(n, dist, cum, max_value)
    if (!dist) {
      while (any(!is.na(n) & n >= max_value)) {
        n <- ifelse(n >= max_value, inner_skel(n), n)
      }

      n <- as.integer(n)
    }
    return(n)
  }

  # call function
  sample <- truncated_skel(n, dist = dist, cum = cum, max_value = max_value)
  return(sample)
}

#' Applies a threshold to all nonparametric distributions in a <dist_spec>
#'
#' @description `r lifecycle::badge("deprecated")`
#' This function is deprecated. Use `bound_dist()` instead.
#' @param x A `<dist_spec>`
#' @param tolerance Numeric; the desired tolerance level. Any part of the
#' cumulative distribution function beyond 1 minus this tolerance level is
#' removed.
#' @return A `<dist_spec>` where probability masses below the threshold level
#' have been removed
#' @keywords internal
apply_tolerance <- function(x, tolerance) {
  lifecycle::deprecate_warn(
    "1.6.0", "dist_skel()", "bound_dist()"
  )
  if (!is(x, "dist_spec")) {
    stop("Can only apply tolerance to distributions in a <dist_spec>.")
  }
  y <- lapply(x, function(x) {
    if (x$distribution == "nonparametric") {
      cmf <- cumsum(x$pmf)
      new_pmf <- x$pmf[c(TRUE, (1 - cmf[-length(cmf)]) >= tolerance)]
      x$pmf <- new_pmf / sum(new_pmf)
      return(x)
    } else {
      return(x)
    }
  })

  ## preserve attributes
  attributes(y) <- attributes(x)
  return(y)
}
