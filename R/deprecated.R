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
      params_mean <- vapply(get_parameters(temp_dist), mean, numeric(1))
      params_sd <- vapply(get_parameters(temp_dist), sd_dist, numeric(1))
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
