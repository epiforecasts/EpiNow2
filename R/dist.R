#' Distribution Skeleton
#'
#' @description `r lifecycle::badge("questioning")`
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
#'   mass function corresponds to the 1-length interval ending at the entry,
#'   i.e. the probability mass function is a vector where the first entry
#'   corresponds to the integral over the (0,1] interval of the continuous
#'   distribution, the second entry corresponds to the (1,2] interval etc.
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
#' @author Sam Abbott
#' @author Sebastian Funk
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
#' dist_skel(10, model = "gamma", params = list(shape = 1, scale = 2))
#'
#' # cumulative prob density
#' dist_skel(0:10,
#'   model = "gamma", dist = TRUE,
#'   params = list(shape = 1, scale = 2)
#' )
#'
#' # probability density
#' dist_skel(0:10,
#'   model = "gamma", dist = TRUE,
#'   cum = FALSE, params = list(shape = 2, scale = 2)
#' )
#'
#' ## Log normal model
#' # sample
#' dist_skel(10, model = "lognormal", params = list(mean = log(5), sd = log(2)))
#'
#' # cumulative prob density
#' dist_skel(0:10,
#'   model = "lognormal", dist = TRUE,
#'   params = list(mean = log(5), sd = log(2))
#' )
#'
#' # probability density
#' dist_skel(0:10,
#'   model = "lognormal", dist = TRUE, cum = FALSE,
#'   params = list(mean = log(5), sd = log(2))
#' )
dist_skel <- function(n, dist = FALSE, cum = TRUE, model,
                      discrete = FALSE, params, max_value = 120) {
  if (model == "exp") {
    # define support functions for exponential dist
    rdist <- function(n) {
      rexp(n, params$rate)
    }
    pdist <- function(n) {
      pexp(n, params$rate) / pexp(max_value, params$rate)
    }
    ddist <- function(n) {
      (pexp(n + 1, params$rate) -
        pexp(n, params$rate)) /
        pexp(max_value + 1, params$rate)
    }
  } else if (model == "gamma") {
    rdist <- function(n) {
      rgamma(n, params$shape, params$scale)
    }
    pdist <- function(n) {
      pgamma(n, params$shape, params$scale) /
        pgamma(max_value + 1, params$shape, params$scale)
    }
    ddist <- function(n) {
      (pgamma(n + 1, params$shape, params$scale) -
        pgamma(n, params$shape, params$scale)) /
        pgamma(max_value + 1, params$shape, params$scale)
    }
  } else if (model == "lognormal") {
    rdist <- function(n) {
      rlnorm(n, params$mean, params$sd)
    }
    pdist <- function(n) {
      plnorm(n, params$mean, params$sd) /
        plnorm(max_value + 1, params$mean, params$sd)
    }
    ddist <- function(n) {
      (plnorm(n + 1, params$mean, params$sd) -
        plnorm(n, params$mean, params$sd)) /
        plnorm(max_value + 1, params$mean, params$sd)
    }
  }

  if (discrete) {
    cmf <- c(0, pdist(seq_len(max_value + 1)))
    pmf <- diff(cmf)
    rdist <- function(n) {
      sample(x = seq_len(max_value + 1) - 1, size = n, prob = pmf)
    }
    pdist <- function(n) {
      cmf[n + 1]
    }
    ddist <- function(n) {
      pmf[n + 1]
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


#' Fit an Integer Adjusted Exponential, Gamma or Lognormal distributions
#'
#' @description `r lifecycle::badge("stable")`
#' Fits an integer adjusted exponential, gamma or lognormal distribution using
#' stan.
#' @param values Numeric vector of values
#'
#' @param samples Numeric, number of samples to take. Must be >= 1000.
#' Defaults to 1000.
#'
#' @param dist Character string, which distribution to fit. Defaults to
#' exponential (`"exp"`) but gamma (`"gamma"`) and lognormal (`"lognormal"`) are
#' also supported.
#'
#' @param cores Numeric, defaults to 1. Number of CPU cores to use (no effect
#' if greater than the number of chains).
#'
#' @param chains Numeric, defaults to 2. Number of MCMC chains to use. More is
#' better with the minimum being two.
#'
#' @param verbose Logical, defaults to FALSE. Should verbose progress messages
#' be printed.
#'
#' @return A stan fit of an interval censored distribution
#' @author Sam Abbott
#' @export
#' @examples
#' \donttest{
#' # integer adjusted exponential model
#' dist_fit(rexp(1:100, 2),
#'   samples = 1000, dist = "exp",
#'   cores = ifelse(interactive(), 4, 1), verbose = TRUE
#' )
#'
#'
#' # integer adjusted gamma model
#' dist_fit(rgamma(1:100, 5, 5),
#'   samples = 1000, dist = "gamma",
#'   cores = ifelse(interactive(), 4, 1), verbose = TRUE
#' )
#'
#' # integer adjusted lognormal model
#' dist_fit(rlnorm(1:100, log(5), 0.2),
#'   samples = 1000, dist = "lognormal",
#'   cores = ifelse(interactive(), 4, 1), verbose = TRUE
#' )
#' }
dist_fit <- function(values = NULL, samples = 1000, cores = 1,
                     chains = 2, dist = "exp", verbose = FALSE) {
  if (samples < 1000) {
    samples <- 1000
    warning(sprintf("%s %s", "`samples` must be at least 1000.",
                    "Now setting it to 1000 internally."
                    )
            )
  }
  # model parameters
  lows <- values - 1
  lows <- ifelse(lows <= 0, 1e-6, lows)
  ups <- values + 1

  data <- list(
    N = length(values),
    low = lows,
    up = ups,
    lam_mean = numeric(0),
    prior_mean = numeric(0),
    prior_sd = numeric(0),
    par_sigma = numeric(0)
  )

  model <- stanmodels$dist_fit

  if (dist == "exp") {
    data$dist <- 0
    data$lam_mean <- array(mean(values))
  } else if (dist == "lognormal") {
    data$dist <- 1
    data$prior_mean <- array(log(mean(values)))
    data$prior_sd <- array(log(sd(values)))
  } else if (dist == "gamma") {
    data$dist <- 2
    data$prior_mean <- array(mean(values))
    data$prior_sd <- array(sd(values))
    data$par_sigma <- array(1.0)
  }

  # set adapt delta based on the sample size
  if (length(values) <= 30) {
    adapt_delta <- 0.999
  } else {
    adapt_delta <- 0.9
  }

  # fit model
  fit <- rstan::sampling(
    model,
    data = data,
    iter = samples + 1000,
    warmup = 1000,
    control = list(adapt_delta = adapt_delta),
    chains = chains,
    cores = cores,
    refresh = ifelse(verbose, 50, 0)
  )
  return(fit)
}


#' Generate a Gamma Distribution Definition Based on Parameter Estimates
#'
#' @description `r lifecycle::badge("soft-deprecated")`
#' Generates a distribution definition when only parameter estimates
#' are available for gamma distributed parameters. See [rgamma()] for
#' distribution information.
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
  if (missing(shape) && missing(scale) && !missing(mean) && !missing(sd)) {
    if (!missing(mean_sd)) {
      mean <- truncnorm::rtruncnorm(samples, a = 0, mean = mean, sd = mean_sd)
    }
    if (!missing(sd_sd)) {
      sd <- truncnorm::rtruncnorm(samples, a = 0, mean = sd, sd = sd_sd)
    }
    scale <- sd^2 / mean
    shape <- mean / scale
    scale <- 1 / scale
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

  dist <- data.table::data.table(
    model = rep("gamma", samples),
    params = purrr::transpose(
      list(
        shape = shape,
        scale = scale
      )
    ),
    max_value = rep(max_value, samples)
  )
  return(dist)
}

#' Generate a Log Normal Distribution Definition Based on Parameter Estimates
#'
#' @description `r lifecycle::badge("soft-deprecated")`
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
    params = purrr::transpose(
      list(
        mean = means,
        sd = sds
      )
    ),
    max_value = rep(max_value, samples)
  )
  return(dist)
}

#' Fit a Subsampled Bootstrap to Integer Values and Summarise Distribution
#' Parameters
#'
#' @description `r lifecycle::badge("stable")`
#' Fits an integer adjusted distribution to a subsampled bootstrap of data and
#' then integrates the posterior samples into a single set of summary
#' statistics. Can be used to generate a robust reporting delay that accounts
#' for the fact the underlying delay likely varies over time or that the size
#' of the available reporting delay sample may not be representative of the
#' current case load.
#'
#' @param values Integer vector of values.
#'
#' @param dist Character string, which distribution to fit. Defaults to
#' lognormal (`"lognormal"`) but gamma (`"gamma"`) is also supported.
#'
#' @param verbose Logical, defaults to `FALSE`. Should progress messages be
#' printed.
#'
#' @param samples Numeric, number of samples to take overall from the
#' bootstrapped posteriors.
#'
#' @param bootstraps Numeric, defaults to 1. The number of bootstrap samples
#' (with replacement) of the delay distribution to take.
#'
#' @param bootstrap_samples Numeric, defaults to 100. The number of samples to
#' take in each bootstrap. When the sample size of the supplied delay
#' distribution is less than 100 this is used instead.
#'
#' @param max_value Numeric, defaults to  the maximum value in the observed
#' data. Maximum delay to  allow (added to output but does impact fitting).
#'
#' @return A `<dist_spec>` object summarising the bootstrapped distribution
#' @author Sam Abbott
#' @importFrom purrr transpose
#' @importFrom future.apply future_lapply
#' @importFrom rstan extract
#' @importFrom data.table data.table rbindlist
#' @export
#' @examples
#' \donttest{
#' # lognormal
#' delays <- rlnorm(500, log(5), 1)
#' out <- bootstrapped_dist_fit(delays,
#'   samples = 1000, bootstraps = 10,
#'   dist = "lognormal"
#' )
#' out
#' }
bootstrapped_dist_fit <- function(values, dist = "lognormal",
                                  samples = 2000, bootstraps = 10,
                                  bootstrap_samples = 250, max_value,
                                  verbose = FALSE) {
  if (!dist %in% c("gamma", "lognormal")) {
    stop("Only lognormal and gamma distributions are supported")
  }

  if (samples < bootstraps) {
    samples <- bootstraps
  }
  ## Make values integer if not
  values <- as.integer(values)
  ## Remove NA values
  values <- values[!is.na(values)]
  ## Filter out negative values
  values <- values[values >= 0]

  get_single_dist <- function(values, samples = 1) {
    set_dt_single_thread()

    fit <- EpiNow2::dist_fit(values, samples = samples, dist = dist)


    out <- list()
    if (dist == "lognormal") {
      out$mean_samples <- sample(rstan::extract(fit)$mu, samples)
      out$sd_samples <- sample(rstan::extract(fit)$sigma, samples)
    } else if (dist == "gamma") {
      alpha_samples <- sample(rstan::extract(fit)$alpha, samples)
      beta_samples <- sample(rstan::extract(fit)$beta, samples)
      out$mean_samples <- alpha_samples / beta_samples
      out$sd_samples <- sqrt(alpha_samples) / beta_samples
    }
    return(out)
  }


  if (bootstraps == 1) {
    dist_samples <- get_single_dist(values, samples = samples)
  } else {
    ## Fit each sub sample
    dist_samples <- future.apply::future_lapply(1:bootstraps,
      function(boot) {
        get_single_dist(
          sample(values,
            min(length(values), bootstrap_samples),
            replace = TRUE
          ),
          samples = ceiling(samples / bootstraps)
        )
      },
      future.scheduling = Inf,
      future.globals = c(
        "values", "bootstraps", "samples",
        "bootstrap_samples", "get_single_dist"
      ),
      future.packages = "data.table", future.seed = TRUE
    )


    dist_samples <- purrr::transpose(dist_samples)
    dist_samples <- purrr::map(dist_samples, unlist)
  }

  out <- list()
  out$mean <- mean(dist_samples$mean_samples)
  out$mean_sd <- sd(dist_samples$mean_samples)
  out$sd <- mean(dist_samples$sd_sample)
  out$sd_sd <- sd(dist_samples$sd_samples)
  if (!missing(max_value)) {
    out$max <- max_value
  } else {
    out$max <- max(values)
  }
  return(do.call(dist_spec, out))
}

#' Estimate a Delay Distribution
#'
#' @description `r lifecycle::badge("maturing")`
#' Estimate a log normal delay distribution from a vector of integer delays.
#' Currently this function is a simple wrapper for [bootstrapped_dist_fit()].
#'
#' @param delays Integer vector of delays
#'
#' @param ... Arguments to pass to internal methods.
#'
#' @return A `<dist_spec>` summarising the bootstrapped distribution
#' @author Sam Abbott
#' @export
#' @seealso [bootstrapped_dist_fit()]
#' @examples
#' \donttest{
#' delays <- rlnorm(500, log(5), 1)
#' estimate_delay(delays, samples = 1000, bootstraps = 10)
#' }
estimate_delay <- function(delays, ...) {
  fit <- bootstrapped_dist_fit(
    values = delays,
    dist = "lognormal", ...
  )
  return(fit)
}

#' Approximate Sampling a Distribution using Counts
#'
#' @description `r lifecycle::badge("soft-deprecated")`
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
    mapped_cases <- suppressMessages(purrr::map_dfc(
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

#' Specify a distribution.
#'
#' @description `r lifecycle::badge("stable")`
#' Defines the parameters of a supported distribution for use in onward
#' modelling. Multiple distribution families are supported - see the
#' documentation for `family` for details. Alternatively, a nonparametric
#' distribution can be specified using the \code{pmf} argument.
#' This function provides distribution
#' functionality in [delay_opts()], [generation_time_opts()], and
#' [trunc_opts()].
#'
#' @param mean Numeric. If the only non-zero summary parameter
#' then this is the fixed interval of the distribution. If the `sd` is
#' non-zero then this is the mean of the distribution given by \code{dist}.
#' If this is not given a vector of empty vectors is returned.
#'
#' @param sd Numeric, defaults to 0. Sets the standard deviation of the
#' distribution.
#'
#' @param mean_sd Numeric, defaults to 0. Sets the standard deviation of the
#' uncertainty around the mean of the  distribution assuming a normal
#' prior.
#'
#' @param sd_sd Numeric, defaults to 0. Sets the standard deviation of the
#' uncertainty around the sd of the  distribution assuming a normal prior.
#'
#' @param distribution Character, defaults to "lognormal". The (discretised
#' distribution to be used. If sd == 0 then the distribution  is fixed and a
#' delta function is used. If sd > 0 then the distribution is discretised and
#' truncated.
#'
#' The following distributions are currently supported:
#'
#'  - "lognormal" - a lognormal distribution. For this distribution `mean`
#' is the mean of the natural logarithm of the delay (on the log scale) and
#' `sd` is the standard deviation of the natural logarithm of the delay.
#'
#' - "gamma" - a gamma distribution. For this distribution `mean` is the
#' mean of the delay and `sd` is the standard deviation of the delay. During
#' model fitting these are then transformed to the shape and scale of the gamma
#' distribution.
#'
#' When `distribution` is the default lognormal distribution the other function
#' arguments have the following definition:
#'  - `mean` is the mean of the natural logarithm of the delay (on the
#' log scale).
#' - `sd` is the standard deviation of the natural logarithm of the delay.
#'
#' @param max Numeric, maximum value of the distribution. The distribution will
#' be truncated at this value.
#'
#' @param pmf Numeric, a vector of values that represent the (nonparametric)
#' probability mass function of the delay (starting with 0); defaults to an
#' empty vector corresponding to a parametric specification of the distribution
#' (using \code{mean}, \code{sd} and corresponding uncertainties)
#'
#' @param fixed Deprecated, use [fix_dist()] instead
#' as coming from fixed (vs uncertain) distributions. Overrides any values
#' assigned to \code{mean_sd} and \code{sd_sd} by setting them to zero.
#' reduces compute requirement but may produce spuriously precise estimates.
#' @return A list of distribution options.
#'
#' @author Sebastian Funk
#' @author Sam Abbott
#' @importFrom rlang warn arg_match
#' @export
#' @examples
#' # A fixed lognormal distribution with mean 5 and sd 1.
#' dist_spec(mean = 5, sd = 1, max = 20, distribution = "lognormal")
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' dist_spec(
#'   mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 20,
#'   distribution = "gamma"
#' )
dist_spec <- function(mean, sd = 0, mean_sd = 0, sd_sd = 0,
                      distribution = c("lognormal", "gamma"), max,
                      pmf = numeric(0), fixed = FALSE) {
  ## deprecate previous behaviour
  warn(
    message = paste(
      "The meaning of the 'max' argument has changed compared to",
      "previous versions. It now indicates the maximum of a distribution",
      "rather than the length of the probability mass function (including 0)",
      "that it represented previously. To replicate previous behaviour reduce",
      "max by 1."
    ),
    .frequency = "regularly",
    .frequency_id = "dist_spec_max"
  )
  ## check for deprecated parameters
  if (!missing(fixed)) {
    deprecate_warn(
      "2.0.0",
      "dist_spec(fixed)",
      "fix_dist()",
      "The argument will be removed completely in version 2.1.0."
    )
    mean_sd <- 0
    sd_sd <- 0
  }
  ## check if parametric or nonparametric
  if (length(pmf) > 0 &&
    !all(
      missing(mean), missing(sd), missing(mean_sd),
      missing(sd_sd), missing(distribution), missing(max)
    )) {
    stop("Distributional parameters or a pmf can be specified, but not both.")
  }

  fixed <- mean_sd == 0 && sd_sd == 0

  ## check parametric parameters make sense
  if (!missing(mean)) {
    if (sd == 0 && sd_sd == 0) { ## integer fixed
      if (mean %% 1 != 0) {
        stop(
          "When a distribution is set to a constant ",
          "(sd == 0 and sd_sd == 0) then the mean parameter ",
          "must be an integer."
        )
      }
      max <- mean
      if (mean_sd > 0) {
        stop(
          "When a distribution has sd == 0 and ",
          "sd_sd == 0 then mean_sd must be 0, too."
        )
      }
    } else {
      if (missing(max)) {
        stop("Maximum of parametric distributions must be specified.")
      }
    }
  } else {
    if (!all(
      missing(sd), missing(mean_sd),
      missing(sd_sd), missing(distribution), missing(max)
    )) {
      stop(
        "If any distributional parameters are given then so must the mean."
      )
    }
  }

  distribution <- arg_match(distribution)
  if (fixed) {
    ret <- list(
      mean_mean = numeric(0),
      mean_sd = numeric(0),
      sd_mean = numeric(0),
      sd_sd = numeric(0),
      dist = character(0),
      max = integer(0)
    )
    if (length(pmf) == 0) {
      if (missing(mean)) { ## empty
        ret <- c(ret, list(
          n = 0,
          n_p = 0,
          n_np = 0,
          np_pmf = numeric(0),
          fixed = integer(0)
        ))
      } else { ## parametric fixed
        if (sd == 0) { ## delta
          pmf <- c(rep(0, mean), 1)
        } else {
          if (distribution == "lognormal") {
            params <- lognorm_dist_def(
              mean = mean, mean_sd = mean_sd,
              sd = sd, sd_sd = sd_sd, max_value = max, samples = 1
            )
          } else if (distribution == "gamma") {
            params <- gamma_dist_def(
              mean = mean, mean_sd = mean_sd,
              sd = sd, sd_sd = sd_sd, max_value = max, samples = 1
            )
          }
          pmf <- dist_skel(
            n = seq_len(max + 1) - 1, dist = TRUE, cum = FALSE,
            model = distribution, params = params$params[[1]], max_value = max,
            discrete = TRUE
          )
        }
      }
    } else { ## nonparametric fixed
      if (!missing(max) && (max + 1) < length(pmf)) {
        pmf <- pmf[1:(max + 1)]
      }
      pmf <- pmf / sum(pmf)
    }
    if (length(pmf) > 0) {
      ret <- c(ret, list(
        n = 1,
        n_p = 0,
        n_np = 1,
        np_pmf = pmf,
        fixed = 1L
      ))
    }
  } else {
    ret <- list(
      mean_mean = mean,
      mean_sd = mean_sd,
      sd_mean = sd,
      sd_sd = sd_sd,
      dist = distribution,
      max = max,
      n = 1,
      n_p = 1,
      n_np = 0,
      np_pmf = numeric(0),
      fixed = 0L
    )
  }
  ret <- purrr::map(ret, array)
  sum_args <- grep("(^n$|^n_$)", names(ret))
  ret$np_pmf_length <- length(ret$np_pmf)
  ret[sum_args] <- purrr::map(ret[sum_args], sum)
  attr(ret, "class") <- c("list", "dist_spec")
  return(ret)
}

#' Creates a delay distribution as the sum of two other delay distributions
#'
#' This is done via convolution with [stats::convolve()]. Nonparametric delays
#' that can be combined are processed together, and their cumulative
#' distribution function is truncated at a specified tolerance level, ensuring
#' numeric stability.
#'
#' @param e1 The first delay distribution (from a call to [dist_spec()]) to
#' combine.
#'
#' @param e2 The second delay distribution (from a call to [dist_spec()]) to
#' combine.
#'
#' @param tolerance A numeric value that sets the cumulative probability
#' to retain when truncating the cumulative distribution function of the
#' combined nonparametric delays. The default value is 0.001 with this retaining
#' 0.999 of the cumulative probability. Note that using a larger tolerance may
#' result in a smaller number of points in the combined nonparametric delay but
#' may also impact the accuracy of the combined delay (i.e., change the mean
#' and standard deviation).
#'
#' @return A delay distribution representing the sum of the two delays
#' (with class [dist_spec()])
#'
#' @author Sebastian Funk
#' @author Sam Abbott
#' @importFrom stats convolve
dist_spec_plus <- function(e1, e2, tolerance = 0.001) {
  ## process delay distributions
  delays <- c(e1, e2)
  ## combine any nonparametric delays that can be combined
  if (sum(delays$fixed) > 1) {
    new_pmf <- 1L
    group_starts <- c(1L, cumsum(delays$np_pmf_length) + 1L)
    for (i in seq_len(length(group_starts) - 1L)) {
      new_pmf <- stats::convolve(
        new_pmf,
        rev(delays$np_pmf[seq(group_starts[i], group_starts[i + 1L] - 1L)]),
        type = "open"
      )
    }
    if (tolerance > 0 && length(new_pmf) > 1) {
        cdf <- cumsum(new_pmf)
        new_pmf <- new_pmf[c(TRUE, (1 - cdf[-length(cdf)]) >= tolerance)]
        new_pmf <- new_pmf / sum(new_pmf)
    }
    delays$np_pmf <- new_pmf
    delays$fixed <- c(1, rep(0, delays$n_p))
    delays$n_np <- 1
    delays$n <- delays$n_p + 1
  }
  delays$np_pmf_length <- length(delays$np_pmf)
  return(delays)
}

#' Creates a delay distribution as the sum of two other delay distributions
#'
#' This is done via convolution with [stats::convolve()]. Nonparametric delays
#' that can be combined are processed together, and their cumulative
#' distribution function is truncated at a specified tolerance level, ensuring
#' numeric stability.
#'
#' @return A delay distribution representing the sum of the two delays
#' (with class [dist_spec()])
#' @inheritParams dist_spec_plus
#' @author Sebastian Funk
#' @method + dist_spec
#' @export
#' @examples
#' # A fixed lognormal distribution with mean 5 and sd 1.
#' lognormal <- dist_spec(
#'   mean = 1.6, sd = 1, max = 20, distribution = "lognormal"
#' )
#' lognormal + lognormal
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' gamma <- dist_spec(
#'   mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 20,
#'   distribution = "gamma"
#' )
#' lognormal + gamma
#'
#' # Using tolerance parameter
#' EpiNow2:::dist_spec_plus(lognormal, lognormal, tolerance = 0.5)
`+.dist_spec` <- function(e1, e2) {
  dist_spec_plus(e1, e2, tolerance = 0.001)
}

#' Combines multiple delay distributions for further processing
#'
#' This combines the parameters so that they can be fed as multiple delay
#' distributions to [epinow()] or [estimate_infections()].
#'
#' @param ... The delay distributions (from calls to [dist_spec()]) to combine
#' @return Combined delay distributions (with class `<dist_spec>`)
#' @author Sebastian Funk
#' @method c dist_spec
#' @importFrom purrr transpose map
c.dist_spec <- function(...) {
  ## process delay distributions
  delays <- list(...)
  if (!(all(vapply(delays, is, FALSE, "dist_spec")))) {
    stop(
      "Delay distribution can only be concatenated with other delay ",
      "distributions."
    )
  }
  ## transpose delays
  delays <- purrr::transpose(delays)
  ## convert back to arrays
  delays <- purrr::map(delays, function(x) array(unlist(x)))
  sum_args <- grep("^n($|_)", names(delays))
  delays[sum_args] <- purrr::map(delays[sum_args], sum)
  attr(delays, "class") <- c("list", "dist_spec")
  return(delays)
}

##' Returns the mean of one or more delay distribution
##'
##' This works out the mean of all the (parametric / nonparametric) delay
##' distributions combined in the passed [dist_spec()].
##'
##' @param x The `<dist_spec>` to use
##' @param ... Not used
##' @return A vector of means.
##' @author Sebastian Funk
##' @method mean dist_spec
##' @importFrom utils head
##' @export
#' @examples
#' # A fixed lognormal distribution with mean 5 and sd 1.
#' lognormal <- dist_spec(
#'  mean = 5, sd = 1, max = 20, distribution = "lognormal"
#' )
#' mean(lognormal)
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' gamma <- dist_spec(
#'  mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 20,
#'  distribution = "gamma"
#' )
#' mean(gamma)
#'
#' # The mean of the sum of two distributions
#' mean(lognormal + gamma)
mean.dist_spec <- function(x, ...) {
  ret <- rep(0, x$n)
  ## nonparametric
  if (x$n_np > 0) {
    init_id <- c(1, head(cumsum(x$np_pmf_length) + 1, n = -1))
    ret[x$fixed == 1L] <- vapply(seq_along(init_id), function(id) {
      pmf <- x$np_pmf[seq(init_id[id], cumsum(x$np_pmf_length)[id])]
      return(sum((seq_len(x$np_pmf_length[id]) - 1) * pmf))
    }, 0)
  }
  ## parametric
  if (x$n_p > 0) {
    ret[x$fixed == 0L] <- vapply(seq_along(which(x$fixed == 0L)), function(id) {
      if (x$dist[id] == "lognormal") {
        return(exp(x$mean_mean[id] + x$sd_mean[id] / 2))
      } else if (x$dist[id] == "gamma") {
        return(x$mean_mean[id])
      } else {
        stop("Unknown distribution: ", x$dist[id])
      }
    }, 0)
  }
  return(ret)
}

#' Prints the parameters of one or more delay distributions
#'
#' This displays the parameters of the uncertain and probability mass
#' functions of fixed delay distributions combined in the passed [dist_spec()].
#' @param x The `<dist_spec>` to use
#' @param ... Not used
#' @return invisible
#' @author Sebastian Funk
#' @method print dist_spec
#' @export
#' @examples
#' #' # A fixed lognormal distribution with mean 5 and sd 1.
#' lognormal <- dist_spec(
#'   mean = 1.5, sd = 0.5, max = 20, distribution = "lognormal"
#' )
#' print(lognormal)
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' gamma <- dist_spec(
#'   mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 20,
#'   distribution = "gamma"
#' )
#' print(gamma)
print.dist_spec <- function(x, ...) {
  cat("\n")
  if (x$n == 0) {
    cat("Empty `dist_spec` distribution.\n")
    return(invisible(NULL))
  } else if (x$n > 1) {
    cat("Combination of delay distributions:\n")
  }
  fixed_id <- 1
  fixed_pos <- 1
  variable_id <- 1
  for (i in 1:x$n) {
    cat("  ")
    if (!is.null(x$names) && nchar(x$names[i]) > 0) {
      cat(x$names[i], ": ", sep = "")
    }
    if (x$fixed[i] == 0) {
      dist <- x$dist[variable_id]
      cat(
        "Uncertain ", dist, " distribution with (untruncated) ",
        ifelse(dist == "lognormal", "log", ""),
        "mean ", signif(x$mean_mean[variable_id], digits = 2),
        " (SD ", signif(x$mean_sd[variable_id], digits = 2), ") and ",
        ifelse(dist == "lognormal", "log", ""),
        "SD ", signif(x$sd_mean[variable_id], 2),
        " (SD ", signif(x$sd_sd[variable_id], 2), ")\n",
        sep = ""
      )
      variable_id <- variable_id + 1
    } else {
      cat(
        "Fixed distribution with PMF [",
        paste(signif(
          x$np_pmf[seq(fixed_pos, fixed_pos + x$np_pmf_length[fixed_id] - 1)],
          digits = 2
        ), collapse = " "),
        "]\n",
        sep = ""
      )
      fixed_id <- fixed_id + 1
      fixed_pos <- fixed_pos + x$np_pmf_length[i]
    }
  }
  cat("\n")
}

#' Plot PMF and CDF for a dist_spec object
#'
#' This function takes a `<dist_spec>` object and plots its probability mass
#' function (PMF) and cumulative distribution function (CDF) using `{ggplot2}`.
#' Note that currently uncertainty in distributions is not plot.
#'
#' @param x A `<dist_spec>` object
#' @param ... Additional arguments to pass to `{ggplot}`.
#' @importFrom ggplot2 aes geom_col geom_step facet_wrap vars theme_bw
#' @export
#' @author Sam Abbott
#' @examples
#' #' # A fixed lognormal distribution with mean 5 and sd 1.
#' lognormal <- dist_spec(
#'   mean = 1.6, sd = 0.5, max = 20, distribution = "lognormal"
#' )
#' plot(lognormal)
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' gamma <- dist_spec(
#'   mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 20,
#'   distribution = "gamma"
#' )
#' plot(gamma)
#'
#' # Multiple distributions
#' plot(lognormal + gamma + lognormal)
#'
#' # A combination of the two fixed distributions
#' plot(lognormal + lognormal)
plot.dist_spec <- function(x, ...) {
  distribution <- cdf <- NULL
  # Get the PMF and CDF data
  pmf_data <- data.frame(
    value = numeric(), pmf = numeric(),
    distribution = factor()
  )
  cdf_data <- data.frame(
    value = numeric(), cdf = numeric(),
    distribution = factor()
  )
  variable_id <- 1
  fixed_id <- 1
  group_starts <- c(1L, cumsum(x$np_pmf_length) + 1L)
  for (i in 1:x$n) {
    if (x$fixed[i] == 0) {
      # Uncertain distribution
      mean <- x$mean_mean[variable_id]
      sd <- x$sd_mean[variable_id]
      c_dist <- dist_spec(
        mean = mean, sd = sd, max = x$max[variable_id],
        distribution = x$dist[variable_id]
      )
      pmf <- c_dist$np_pmf
      variable_id <- variable_id + 1
      dist_name <- paste0("Uncertain ", x$dist[variable_id], " (ID: ", i, ")")
    } else {
      # Fixed distribution
      pmf <- x$np_pmf[seq(group_starts[i], group_starts[i + 1L] - 1L)]
      dist_name <- paste0("Fixed", " (ID: ", i, ")")
      fixed_id <- fixed_id + 1
    }
    pmf_data <- rbind(
      pmf_data,
      data.frame(
        value = seq_along(pmf), pmf = pmf, distribution = dist_name
      )
    )
    cumsum_pmf <- cumsum(pmf)
    cdf_data <- rbind(
      cdf_data,
      data.frame(
        value = seq_along(pmf), cdf = cumsum_pmf / sum(pmf),
        distribution = dist_name
      )
    )
  }

  # Plot PMF and CDF as facets in the same plot
  plot <- ggplot() +
    aes(x = value, y = pmf) +
    geom_col(data = pmf_data) +
    geom_step(data = cdf_data, aes(y = cdf)) +
    facet_wrap(vars(distribution)) +
    labs(x = "Day", y = "Probability density") +
    theme_bw()
  return(plot)
}

##' Fix the parameters of a `<dist_spec>` object
##'
##' If the given `<dist_spec>` has any uncertainty, it is removed and the
##' corresponding distribution converted into a fixed one.
##' @return A `<dist_spec>` object without uncertainty
##' @author Sebastian Funk
##' @export
##' @param x A `<dist_spec>` object
##' @param strategy Character; either "mean" (use the mean estimates of the
##'   mean and standard deviation) or "sample" (randomly sample mean and
##'   standard deviation from uncertainty given in the `<dist_spec>`
##' @importFrom truncnorm rtruncnorm
##' @importFrom rlang arg_match
fix_dist <- function(x, strategy = c("mean", "sample")) {
  ## if x is fixed already we don't have to do anything
  if (x$fixed) return(x)
  ## match startegy argument to options
  strategy <- arg_match(strategy)
  ## apply stragey depending on choice
  if (strategy == "mean") {
    x <- dist_spec(
      mean = x$mean_mean,
      sd = x$sd_mean,
      mean_sd = 0,
      sd_sd = 0,
      distribution = x$dist,
      max = x$max
    )
  } else if (strategy == "sample") {
    lower_bound <- ifelse(x$dist == "gamma", 0, -Inf)
    mean <- rtruncnorm(
      n = 1, a = lower_bound, mean = x$mean_mean, sd = x$mean_sd
    )
    sd <- rtruncnorm(n = 1, a = 0, mean = x$sd_mean, sd = x$mean_sd)
    x <- dist_spec(
      mean = mean,
      sd = sd,
      mean_sd = 0,
      sd_sd = 0,
      distribution = x$dist,
      max = x$max
    )
  }
  return(x)
}
