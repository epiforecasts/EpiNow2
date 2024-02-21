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
#' @author Sam Abbott
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
    "2.0.0", "gamma_dist_def()", "Gamma()",
    "The function will be removed completely in version 2.1.0."
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
#' @author Sam Abbott
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
    "2.0.0", "lognorm_dist_def()", "LogNormal()",
    "The function will be removed completely in version 2.1.0."
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

#' Tune an Inverse Gamma to Achieve the Target Truncation
#'
#' @description `r lifecycle::badge("deprecated")`
#' Allows an inverse gamma distribution to be. tuned so that less than 0.01 of
#' its probability mass function falls outside of the specified bounds. This is
#' required when using an inverse gamma prior, for example for a Gaussian
#' process. As no inverse gamma priors are currently in use and this function
#' has some stability issues it has been deprecated.
#'
#' @param lower Numeric, defaults to 2. Lower truncation bound.
#'
#' @param upper Numeric, defaults to 21. Upper truncation bound.
#'
#' @return A list of alpha and beta values that describe a inverse gamma
#' distribution that achieves the target truncation.
#' @export
#'
#' @keywords internal
#'
tune_inv_gamma <- function(lower = 2, upper = 21) {
  lifecycle::deprecate_stop(
    "1.3.6", "tune_inv_gamma()",
    details = paste0(
      "As no inverse gamma priors are currently in use and this function has ",
      "some stability issues it has been deprecated. Please let the package ",
      "authors know if deprecating this function has caused any issues. ",
      "For the last active version of the function see the one contained ",
      "in version 1.3.5 at ",
      "https://github.com/epiforecasts/EpiNow2/blob/bad836ebd650ace73ad1ead887fd0eae98c52dd6/R/dist.R#L739" # nolint
    )
  )
}
