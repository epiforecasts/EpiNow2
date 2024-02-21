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
  if (model == "exp") {
    # define support functions for exponential dist
    rdist <- function(n) {
      rexp(n, params[["rate"]])
    }
    pdist <- function(n) {
      pexp(n, params[["rate"]]) / pexp(max_value, params[["rate"]])
    }
    ddist <- function(n) {
      (pexp(n + 1, params[["rate"]]) -
        pexp(n, params[["rate"]])) /
        pexp(max_value + 1, params[["rate"]])
    }
  } else if (model == "gamma") {
    rdist <- function(n) {
      rgamma(n = n, shape = params[["shape"]], rate = params[["rate"]])
    }
    pdist <- function(n) {
      pgamma(q = n, shape = params[["shape"]], rate = params[["rate"]]) /
        pgamma(
          q = max_value + 1, shape = params[["shape"]], rate = params[["rate"]]
        )
    }
    ddist <- function(n) {
      (pgamma(q = n + 1, shape = params[["shape"]], rate = params[["rate"]]) -
        pgamma(q = n, shape = params[["shape"]], rate = params[["rate"]])) /
        pgamma(q = max_value + 1, params[["shape"]], rate = params[["rate"]])
    }
  } else if (model == "lognormal") {
    rdist <- function(n) {
      rlnorm(n, params[["meanlog"]], params[["sdlog"]])
    }
    pdist <- function(n) {
      plnorm(n, params[["meanlog"]], params[["sdlog"]]) /
        plnorm(max_value + 1, params[["meanlog"]], params[["sdlog"]])
    }
    ddist <- function(n) {
      (plnorm(n + 1, params[["meanlog"]], params[["sdlog"]]) -
        plnorm(n, params[["meanlog"]], params[["sdlog"]])) /
        plnorm(max_value + 1, params[["meanlog"]], params[["sdlog"]])
    }
  } else if (model == "normal") {
    rdist <- function(n) {
      rnorm(n, params[["mean"]], params[["sd"]])
    }
    pdist <- function(n) {
      pnorm(n, params[["mean"]], params[["sd"]]) /
        pnorm(max_value + 1, params[["mean"]], params[["sd"]])
    }
    ddist <- function(n) {
      (pnorm(n + 1, params[["mean"]], params[["sd"]]) -
        pnorm(n, params[["mean"]], params[["sd"]])) /
        pnorm(max_value + 1, params[["mean"]], params[["sd"]])
    }
  } else if (model == "fixed") {
    rdist <- function(n) {
      rep(params[["value"]], n)
    }
    pdist <- function(n) {
      as.integer(n > params[["value"]])
    }
    ddist <- function(n) {
      as.integer(n == params[["value"]])
    }
  }

  if (discrete) {
    cmf <- c(0, pdist(seq_len(max_value + 1)))
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
#' @author Sebastian Funk
#' @author Sam Abbott
#' @importFrom rlang warn arg_match
#' @export
dist_spec <- function(distribution = c(
                        "lognormal", "normal", "gamma", "fixed", "empty"
                      ),
                      params_mean = numeric(0), params_sd = numeric(0),
                      mean, sd = 0, mean_sd = 0, sd_sd = 0,
                      max = Inf, pmf = numeric(0), fixed = FALSE) {

  lifecycle::deprecate_warn(
    "2.0.0",
    "dist_spec()",
    details = c(
      paste0(
        "Please use distribution functions such as `Gamma()` or `Lognormal()` ",
        "instead."
      ),
      "The function will become internal only in version 2.1.0."
    )
  )
  ## check for deprecated parameters
  if (!missing(fixed)) {
    lifecycle::deprecate_warn(
      "2.0.0",
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
      "2.0.0",
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
      params_mean <- temp_dist$params_mean
      params_sd <- temp_dist$params_sd
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

#' Creates a delay distribution as the sum of two other delay distributions.
#'
#' @return A delay distribution representing the sum of the two delays
#' (with class [dist_spec()])
#' @param e1 The first delay distribution (of type [dist_spec()]) to
#' combine.
#'
#' @param e2 The second delay distribution (of type [dist_spec()]) to
#' combine.
#' @author Sebastian Funk
#' @method + dist_spec
#' @export
#' @examples
#' # A fixed lognormal distribution with mean 5 and sd 1.
#' dist1 <- LogNormal(
#'   meanlog = 1.6, sdlog = 1, max = 20
#' )
#' dist1 + dist1
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' dist2 <- Gamma(
#'   mean = Normal(3, 0.5), sd = Normal(2, 0.5), max = 20
#' )
#' dist1 + dist2
`+.dist_spec` <- function(e1, e2) {
  c(e1, e2)
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
#' @export
c.dist_spec <- function(...) {
  ## process delay distributions
  dist_specs <- list(...)
  if (!(all(vapply(dist_specs, is, FALSE, "dist_spec")))) {
    stop(
      "Distribution can only be concatenated with other delay ",
      "distributions."
    )
  }
  dist_specs <- do.call(c, lapply(dist_specs, unclass))
  attr(dist_specs, "class") <- c("dist_spec", "list")
  return(dist_specs)
}

#' Returns the mean of one or more delay distribution
#'
#' This works out the mean of all the (parametric / nonparametric) delay
#' distributions combined in the passed [dist_spec()] (ignoring any uncertainty
#' in parameters)
#'
#' @param x The `<dist_spec>` to use
#' @param ... Not used
#' @param ignore_uncertainty Logical; whether to ignore any uncertainty in
#'   parameters. If set to FALSE (the default) then the mean of any uncertain
#'   parameters will be returned as NA.
#' @author Sebastian Funk
#' @method mean dist_spec
#' @importFrom utils head
#' @export
#' @examples
#' # A fixed lognormal distribution with mean 5 and sd 1.
#' dist1 <- LogNormal(mean = 5, sd = 1, max = 20)
#' mean(dist1)
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' dist2 <- Gamma(
#'  mean = Normal(3, 0.5), sd = Normal(2, 0.5), max = 20
#' )
#' mean(dist2)
#'
#' # The mean of the sum of two distributions
#' mean(dist1 + dist2)
mean.dist_spec <- function(x, ..., ignore_uncertainty = FALSE) {
  ret <- vapply(x, function(y) {
    if (is.numeric(y)) {
      return(y)
    }
    ## y is a dist_spec
    if (y$distribution == "nonparametric") {
      ## nonparametric
      return(sum((seq_along(y$pmf) - 1) * y$pmf))
    } else {
      if (!all(vapply(y$parameters, is.numeric, logical(1)))) {
        if (ignore_uncertainty) {
          y$parameters <- lapply(y$parameters, mean, ignore_uncertainty = TRUE)
        } else {
          return(NA_real_)
        }
      }
      if (y$distribution == "lognormal") {
        return(exp(y$parameters$meanlog + y$parameters$sdlog**2 / 2))
      } else if (y$distribution == "gamma") {
        return(y$parameters$shape / y$parameters$rate)
      } else if (y$distribution == "normal") {
        return(y$parameters$mean)
      } else if (y$distribution == "fixed") {
        return(y$parameters$value)
      } else {
        stop(
          "Don't know how to calculate mean of ", y$distribution,
          " distribution."
        )
      }
    }
  }, numeric(1))
  return(ret)
}

#' Returns the standard deviation of one or more delay distribution
#'
#' This works out the standard deviation of all the (parametric /
#' nonparametric) delay distributions combined in the passed [dist_spec()].
#'
#' @param x The [dist_spec()] to use
#' @return A vector of standard deviations.
#' @author Sebastian Funk
#' @importFrom utils head
#' @keywords internal
#' @examples
#' \dontrun{
#' # A fixed lognormal distribution with sd 5 and sd 1.
#' dist1 <- LogNormal(mean = 5, sd = 1, max = 20)
#' sd_dist(dist1)
#'
#' # A gamma distribution with mean 3 and sd 2
#' dist2 <- Gamma(mean = 3, sd = 2)
#' sd_dist(dist2)
#'
#' # The sd of the sum of two distributions
#' sd_dist(dist1 + dist2)
#' }
sd_dist <- function(x) {
  ret <- vapply(x, function(y) {
    if (is.numeric(y)) {
      return(0)
    }
    ## y is a dist_spec
    if (y$distribution == "nonparametric") {
      ## nonparametric
      mean_pmf <- sum((seq_along(y$pmf) - 1) * y$pmf)
      return(sum((seq_along(y$pmf) - 1)**2 * y$pmf) - mean_pmf^2)
    } else {
      ## parametric
      if (!all(vapply(y$parameters, is.numeric, logical(1)))) {
        return(NA_real_)
      }
      if (y$distribution == "lognormal") {
        sqrt(exp(y$parameters$sdlog**2) - 1) *
          exp(y$parameters$meanlog + 0.5 * y$parameters$sdlog**2)
      } else if (y$distribution == "gamma") {
        sqrt(y$parameters$shape / y$parameters$rate**2)
      } else if (y$distribution == "normal") {
        y$parameters$sd
      } else if (y$distribution == "fixed") {
        0
      } else {
        stop(
          "Don't know how to calculate standard deviation of ",
          y$distribution, " distribution."
        )
      }
    }
  }, numeric(1))
  return(ret)
}

#' Returns the maximum of one or more delay distribution
#'
#' This works out the maximum of all the (parametric / nonparametric) delay
#' distributions combined in the passed [dist_spec()] (ignoring any uncertainty
#' in parameters)
#'
#' @param x The [dist_spec()] to use
#' @param ... Not used
#' @return A vector of means.
#' @author Sebastian Funk
#' @method max dist_spec
#' @export
#' @examples
#' # A fixed gamma distribution with mean 5 and sd 1.
#' dist1 <- Gamma(mean = 5, sd = 1, max = 20)
#' max(dist1)
#'
#' # An uncertain lognormal distribution with mean 3 and sd 2
#' dist2 <- LogNormal(mean = Normal(3, 0.5), sd = Normal(2, 0.5), max = 20)
#' max(dist2)
#'
#' # The maxf the sum of two distributions
#' max(dist1 + dist2)
max.dist_spec <- function(x, ...) {
  ret <- vapply(x, function(y) {
    ## y is a dist_spec
    if (y$distribution == "nonparametric") {
      ## nonparametric
      return(length(y$pmf) - 1)
    } else if (y$distribution == "fixed") {
      return(y$parameters$value)
    } else {
      return(y$max)
    }
  }, numeric(1))
  return(ret)
}
##
#' Discretise a <dist_spec>
#'
#' By default it will discretise all the distributions it can discretise
#' (i.e. those with finite support and constant parameters).
#' @title Discretise a <dist_spec>
#' @param x A `<dist_spec>`
#' @param silent Logical; if `TRUE` then any distribution that can't be
#'   discretised will be returned as is. If `FALSE` then an error will be
#'   thrown.
#' @return A `<dist_spec>` where all distributions with constant parameters are
#'   nonparametric.
#' @author Sebastian Funk
#' @export
discretise <- function(x, silent = TRUE) {
  if (!is(x, "dist_spec")) {
    stop("Can only discretise a <dist_spec>.")
  }
  ## check max
  max_x <- max(x)
  if (any(is.infinite(max_x)) && !silent) {
    stop("Cannot discretise a distribution with infinite support.")
  }
  ## discretise
  ret <- lapply(seq_along(x), function(id) {
    y <- x[[id]]
    if (y$distribution == "nonparametric") {
      return(y)
    } else {
      if (all(vapply(y$parameters, is.numeric, logical(1))) &&
          is.finite(max_x[id])) {
        z <- list(pmf = dist_skel(
          n = seq_len(max_x[id] + 1) - 1, dist = TRUE, cum = FALSE,
          model = y$distribution, params = y$parameters,
          max_value = max_x[id], discrete = TRUE
        ))
        z$distribution <- "nonparametric"
        return(z)
      } else if (silent) {
        return(y)
      } else {
        stop(
          "Cannot discretise a distribution with uncertain parameters."
        )
      }
    }
  })
  attr(ret, "class") <- c("dist_spec", "list")
  return(ret)
}

#' Collapse nonparametric distributions in a <dist_spec>
#'
#' This convolves any consecutive nonparametric distributions contained
#' in the <dist_spec>.
#' @param x A `<dist_spec>`
#' @return A `<dist_spec>` where consecutive nonparametric distributions
#' have been convolved
#' @importFrom stats convolve
#' @author Sebastian Funk
#' @export
collapse <- function(x) {
  if (!is(x, "dist_spec")) {
    stop("Can only convolve distributions in a <dist_spec>.")
  }
  ## get nonparametric distributions
  nonparametric <- unname(unlist(map(x, "distribution"))) == "nonparametric"
  ## find consecutive nonparametric distributions
  consecutive <- rle(nonparametric)
  ids <- unique(c(1, cumsum(consecutive$lengths[-length(consecutive$lengths)])))
  ## find ids of nonparametric distributions that are collapsable
  ## (i.e. have other nonparametric distributions followign them)
  collapseable <- ids[consecutive$values & (consecutive$length > 1)]
  ## identify ids of distributions that follow the collapseable distributions
  next_ids <- lapply(collapseable, function(id) {
    ids[id] + seq_len(consecutive$lengths[id] - 1)
  })
  for (id in collapseable) {
    ## collapse distributions
    for (next_id in next_ids[id]) {
      x[[ids[id]]]$pmf <- convolve(
        x[[ids[id]]]$pmf, rev(x[[next_id]]$pmf), type = "open"
      )
    }
  }
  ## remove collapsed pmfs
  x[unlist(next_ids)] <- NULL

  return(x)
}

#' Applies a threshold to all nonparametric distributions in a <dist_spec>
#'
#' This removes any part of the tail of the nonparametric distributions in the
#' <dist_spec> where the probability mass is below the threshold level.
#' @param x A `<dist_spec>`
#' @param tolerance Numeric; the desired tolerance level.
#' @return A `<dist_spec>` where probability masses below the threshold level
#' have been removed
#' @author Sebastian Funk, Sam Abbott
#' @export
apply_tolerance <- function(x, tolerance) {
  if (!is(x, "dist_spec")) {
    stop("Can only apply tolerance to distributions in a <dist_spec>.")
  }
  x <- lapply(x, function(x) {
    if (x$distribution == "nonparametric") {
      cmf <- cumsum(x$pmf)
      new_pmf <- x$pmf[c(TRUE, (1 - cmf[-length(cmf)]) >= tolerance)]
      x$pmf <- new_pmf / sum(new_pmf)
      return(x)
    } else {
      return(x)
    }
  })

  attr(x, "class") <- c("dist_spec", "list")
  return(x)
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
#' dist1 <- LogNormal(mean = 1.5, sd = 0.5, max = 20)
#' print(dist1)
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' dist2 <- Gamma(
#'   mean = Normal(3, 0.5), sd = Normal(2, 0.5), max = 20
#' )
#' print(dist2)
print.dist_spec <- function(x, ...) {
  .print.dist_spec(x, indent = 0, ...)
}

.print.dist_spec <- function(x, indent, ...) {
  indent_str <- strrep(" ", indent)
  if (length(x) > 1) {
    cat(indent_str, "Composite distribution:\n", sep = "")
  }
  for (i in seq_along(x)) {
    if (x[[i]]$distribution == "nonparametric") {
      ## nonparametric
      cat(
        indent_str, "- nonparametric distribution\n", indent_str, "  PMF: [",
        paste(signif(x[[i]]$pmf, digits = 2), collapse = " "), "]\n",
        sep = ""
      )
    } else if (x[[i]]$distribution == "fixed") {
      ## fixed
      cat(indent_str, "- fixed value:\n", sep = "")
      if (is.numeric(x[[i]]$parameters$value)) {
        cat(indent_str, "  ", x[[i]]$parameters$value, "\n", sep = "")
      } else {
        .print.dist_spec(x[[i]]$parameters$value, indent = indent + 4)
      }
    } else {
      ## parametric
      cat(indent_str, "- ",  x[[i]]$distribution, " distribution", sep = "")
      if (is.finite(x[[i]]$max)) {
        cat(" (max: ", x[[i]]$max, ")", sep = "")
      }
      cat(":\n")
      ## loop over natural parameters and print
      for (param in names(x[[i]]$parameters)) {
        cat(
          indent_str, "  ", param, ":\n", sep = ""
        )
        if (is.numeric(x[[i]]$parameters[[param]])) {
          cat(
            indent_str, "    ",
            signif(x[[i]]$parameters[[param]], digits = 2), "\n",
            sep = ""
          )
        } else {
          .print.dist_spec(x[[i]]$parameters[[param]], indent = indent + 4)
        }
      }
    }
  }
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
#' dist1 <- LogNormal(mean = 1.6, sd = 0.5, max = 20)
#' plot(dist1)
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' dist2 <- Gamma(
#'   mean = Normal(3, 0.5), sd = Normal(2, 0.5), max = 20
#' )
#' plot(dist2)
#'
#' # Multiple distributions
#' plot(dist1 + dist2 + dist1)
#'
#' # A combination of the two fixed distributions
#' plot(dist1 + dist1)
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
  dist_sd <- sd_dist(x)
  for (i in seq_along(x)) {
    if (x[[i]]$distribution == "nonparametric") {
      # Fixed distribution
      pmf <- x[[i]]$pmf
      dist_name <- paste0("Nonparametric", " (ID: ", i, ")")
    } else {
      # Uncertain distribution
      c_dist <- discretise(fix_dist(extract_single_dist(x, i)))
      pmf <- c_dist[[1]]$pmf
      dist_name <- paste0(
        ifelse(is.na(dist_sd[i]), "Uncertain ", ""),
        x[[i]]$distribution, " (ID: ", i, ")"
      )
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

#' Extract a single element of a composite `<dist_spec>`
#'
#' @param x A composite `dist_spec` object
#' @param i The index to extract
#' @return A single `dist_spec` object
#' @keywords internal
#' @author Sebastian Funk
extract_single_dist <- function(x, i) {
  if (i > length(x)) {
    stop("i can't be greater than the number of distributions.")
  }
  ret <- list(x[[i]])
  attr(ret, "class") <- c("dist_spec", class(ret))
  return(ret)
}

#' Fix the parameters of a `<dist_spec>`
#'
#' If the given `<dist_spec>` has any uncertainty, it is removed and the
#' corresponding distribution converted into a fixed one.
#' @return A `<dist_spec>` object without uncertainty
#' @author Sebastian Funk
#' @export
#' @param x A `<dist_spec>`
#' @param strategy Character; either "mean" (use the mean estimates of the
#'   mean and standard deviation) or "sample" (randomly sample mean and
#'   standard deviation from uncertainty given in the `<dist_spec>`
#' @importFrom truncnorm rtruncnorm
#' @importFrom rlang arg_match
fix_dist <- function(x, strategy = c("mean", "sample")) {
  if (!is(x, "dist_spec")) {
    stop("Can only fix distributions in a <dist_spec>.")
  }
   ## match strategy argument to options
  strategy <- arg_match(strategy)

  ret <- lapply(x, function(x) {
    ## if x is fixed already we don't have to do anything
    if (
      x$distribution == "nonparametric" ||
      all(vapply(x$parameters, is.numeric, logical(1)))
    ) {
      return(x)
    }
    ## apply strategy depending on choice
    if (strategy == "mean") {
      x$parameters <- lapply(x$parameters, mean)
    } else if (strategy == "sample") {
      lower_bound <-
        lower_bounds(x$distribution)[natural_params(x$distribution)]
      mean <- as.list(rtruncnorm(
        n = 1, a = lower_bound,
        mean = vapply(x$parameters, mean, numeric(1)),
        sd = vapply(x$parameters, sd_dist, numeric(1))
      ))
      names(mean) <- names(x$parameters)
      x$parameters <- mean
    }
    return(x)
  })

  attr(ret, "class") <- c("dist_spec", "list")
  return(ret)
}

#' @details
#' Probability distributions are ubiquitous in EpiNow2, usually representing
#' epidemiological delays (e.g., the generation time for delays between
#' becoming infecting and infecting others; or reporting delays)
#'
#' They are generated using functions that have a name corresponding to the
#' probability distribution that is being used. They generated `dist_spec`
#' objects that are then passed to the models underlying EpiNow2.
##
#' All parameters can be given either as fixed values (a numeric value) or as
#' uncertain values (a `dist_sepc`). If given as uncertain values, currently
#' only normally distributed parameters (generated using `Normal()`) are
#' supported.
#'
#' Each distribution has a representation in terms of "natural" parameters (the
#' ones used in stan) but can sometimes also be specified using other
#' parameters such as the mean or standard deviation of the distribution. If
#' not given as natural parameters then these will be calculated from the given
#' parameters. If they have uncertainty, this will be done by random sampling
#' from the given uncertainty and converting resulting parameters to their
#' natural representation.
#'
#' Currently available distributions are lognormal, gamma, normal, fixed
#' (delta) and nonparametric. The nonparametric is a special case where the
#' probability mass function is given directly as a numeric vector.
#'
#' @inheritParams stats::Lognormal
#' @param mean,sd mean and standard deviation of the distribution
#' @param max Numeric, maximum value of the distribution. The distribution will
#' be truncated at this value. Default: `Inf`, i.e. no maximum.
#' @return A `dist_spec` representing a distribution of the given
#'   specification.
#' @author Sebastian Funk
#' @export
#' @rdname Distributions
#' @name Distributions
#' @order 1
#' @examples
#' LogNormal(mean = 4, sd = 1)
#' LogNormal(mean = 4, sd = 1, max = 10)
#' LogNormal(mean = Normal(4, 1), sd = 1, max = 10)
LogNormal <- function(meanlog, sdlog, mean, sd, max = Inf) {
  params <- as.list(environment())
  return(new_dist_spec(params, "lognormal"))
}

#' @inheritParams stats::GammaDist
#' @author Sebastian Funk
#' @rdname Distributions
#' @title Probability distributions
#' @order 2
#' @export
#' @examples
#' Gamma(mean = 4, sd = 1)
#' Gamma(shape = 16, rate = 4)
#' Gamma(shape = Normal(16, 2), rate = Normal(4, 1))
#' Gamma(shape = Normal(16, 2), scale = Normal(1/4, 1))
Gamma <- function(shape, rate, scale, mean, sd, max = Inf) {
  params <- as.list(environment())
  return(new_dist_spec(params, "gamma"))
}

#' @rdname Distributions
#' @order 3
#' @author Sebastian Funk
#' @export
#' @examples
#' Normal(mean = 4, sd = 1)
#' Normal(mean = 4, sd = 1, max = 10)
Normal <- function(mean, sd, max = Inf) {
  params <- as.list(environment())
  return(new_dist_spec(params, "normal"))
}

#' @rdname Distributions
#' @order 4
#' @param value Value of the fixed (delta) distribution
#' @author Sebastian Funk
#' @export
#' @examples
#' Fixed(value = 3)
#' Fixed(value = 3.5)
Fixed <- function(value, max = Inf) {
  params <- as.list(environment())
  return(new_dist_spec(params, "fixed"))
}

#' Generates a nonparametric distribution.
#'
#' @param mass Probability mass of the given distribution; this is
#'   passed as a zero-indexed numeric vector (i.e. the fist entry represents
#'   the probability mass of zero). If not summing to one it will be normalised
#'   to sum to one internally.
#' @author Sebastian Funk
#' @rdname Distributions
#' @order 5
#' @export
#' @examples
#' pmf(c(0.1, 0.3, 0.2, 0.4))
#' pmf(c(0.1, 0.3, 0.2, 0.1, 0.1))
pmf <- function(mass) {
  params <- list(pmf = mass / sum(mass))
  return(new_dist_spec(params, "nonparametric"))
}

#' Get the names of the natural parameters of a distribution
#'
#' These are the parameters used in the stan models. All other parameter
#' representations are converted to these using [convert_to_natural()] before
#' being passed to the stan models.
#' @param distribution Character; the distribution to use.
#' @return A character vector, the natural parameters.
#' @author Sebastian Funk
#' @keywords internal
#' @examples
#' \dontrun{
#' natural_params("gamma")
#' }
natural_params <- function(distribution) {
  if (distribution == "gamma") {
    ret <- c("shape", "rate")
  } else if (distribution == "lognormal") {
    ret <- c("meanlog", "sdlog")
  } else if (distribution == "normal") {
    ret <- c("mean", "sd")
  } else if (distribution == "fixed") {
    ret <- "value"
  }
  return(ret)
}

#' Get the lower bounds of the parameters of a distribution
#'
#' This is used to avoid sampling parameter values that have no support.
#' @return A numeric vector, the lower bounds.
#' @inheritParams natural_params
#' @author Sebastian Funk
#' @keywords internal
#' @examples
#' \dontrun{
#' lower_bounds("lognormal")
#' }
lower_bounds <- function(distribution) {
  if (distribution == "gamma") {
    ret <- c(shape = 0, rate = 0, scale = 0, mean = 0, sd = 0)
  } else if (distribution == "lognormal") {
    ret <- c(meanlog = -Inf, sdlog = 0, mean = 0, sd = 0)
  } else if (distribution == "normal") {
    ret <- c(mean = -Inf, sd = 0)
  } else if (distribution == "fixed") {
    ret <- c(value = 1)
  }
  return(ret)
}

#' Internal function for extracting given parameter names of a distribution
#' from the environment
#'
#' @param params Given parameters (obtained using `as.list(environment())`)
#' @return A character vector of parameters and their values.
#' @inheritParams natural_params
#' @author Sebastian Funk
#' @keywords internal
extract_params <- function(params, distribution) {
  params <- params[!vapply(params, inherits, "name", FUN.VALUE = TRUE)]
  n_params <- length(natural_params(distribution))
  if (length(params) != n_params) {
    stop(
      "Exactly ", n_params, " parameters of the ", distribution,
      " distribution must be specified."
    )
  }
  return(params)
}

#' Internal function for generating a `dist_spec` given parameters and a
#' distribution.
#'
#' This will convert all parameters to natural parameters before generating
#' a `dist_spec`. If they have uncertainty this will be done using sampling.
#' @param params Parameters of the distribution (including `max`)
#' @inheritParams extract_params
#' @importFrom purrr walk
#' @return A `dist_spec` of the given specification.
#' @author Sebastian Funk
#' @keywords internal
new_dist_spec <- function(params, distribution) {
  if (distribution == "nonparametric") {
    ## nonparametric distribution
    ret <- list(
      pmf = params$pmf,
      distribution = "nonparametric"
    )
  } else {
    ## process min/max first
    max <- params$max
    params$max <- NULL
    ## extract parameters and convert all to dist_spec
    params <- extract_params(params, distribution)
    ## fixed distribution
    if (distribution == "fixed") {
      ret <- list(
        parameters = params,
        distribution = "fixed"
      )
    } else {
      ## parametric probability distribution
      ## check bounds
      for (param_name in names(params)) {
        lb <- lower_bounds(distribution)[param_name]
        if (is.numeric(params[[param_name]]) && params[[param_name]] < lb) {
          stop(
            "Parameter ", param_name, " is less than its lower bound ", lb,
            "."
          )
        } else if (
          is(params[[param_name]], "dist") && params[[param_name]]$max < lb
          ) {
          stop(
            "Maximum of parameter ", param_name, " is less than its ",
            "lower bound ", lb, "."
          )
        }
      }

      ## convert any unnatural parameters
      unnatural_params <- setdiff(names(params), natural_params(distribution))
      if (length(unnatural_params) > 0) {
        if (length(unnatural_params) < length(params)) {
          stop(
            "Incompatible combination of parameters of a ", distribution,
            " distribution specified."
          )
        }
        ## sample parameters if they are uncertain
        if (any(vapply(params, sd_dist, numeric(1)) > 0)) {
          warning(
            "Uncertain ", distribution, " distribution specified in terms of ",
            "parameters that are not the \"natural\" parameters of the ",
            "distribution (", toString(natural_params(distribution)),
            "). Converting using a crude and very approximate method ",
            "that is likely to produce biased results. If possible, ",
            "it is preferable to specify the distribution directly ",
            "in terms of the natural parameters."
          )
        }
        ## generate natural parameters
        params <- convert_to_natural(params, distribution)
      }
      ## convert normal with sd == 0 to fixed
      if (distribution == "normal" && is.numeric(params$sd) && params$sd == 0) {
        ret <- list(
          parameters = list(value = params$mean), distribution = "fixed"
        )
      } else {
        ret <- list(parameters = params, distribution = distribution)
      }
    }
    ret <- c(ret, list(max = max))
  }
  ## join and wrap in another list to make concatenating easier
  ret <- list(ret)
  attr(ret, "class") <- c("dist_spec", "list")

  ## now we have a distribution with natural parameters - return dist_spec
  return(ret)
}

#' Internal function for converting parameters to natural parameters.
#'
#' This is used for preprocessing before generating a `dist_spec` object
#' from a given set of parameters and distribution
#' @param params A numerical named parameter vector
#' @inheritParams natural_params
#' @return A list with two elements, `params_mean` and `params_sd`, containing
#' mean and sd of natural parameters.
#' @author Sebastian Funk
convert_to_natural <- function(params, distribution) {
  ## unnatural parameter means
  ux <- lapply(params, mean)
  ## estimate relative uncertainty of parameters
  rel_unc <- mean(vapply(params, sd_dist, numeric(1))^2 / unlist(ux))
  ## store natural parameters
  x <- list()
  if (distribution == "gamma") {
    if ("mean" %in% names(ux) && "sd" %in% names(ux)) {
      x$shape <- ux$mean**2 / ux$sd**2
      x$rate <- x$shape / ux$mean
    } else if (!("rate" %in% names(ux)) && ("scale" %in% names(ux))) {
      x$shape <- ux$shape
      x$rate <- 1 / ux$scale
    }
  } else if (distribution == "lognormal" &&
             "mean" %in% names(params) && "sd" %in% names(params)) {
    x$meanlog <- log(ux$mean^2 / sqrt(ux$sd^2 + ux$mean^2))
    x$sdlog <- convert_to_logsd(ux$mean, ux$sd)
  }
  if (rel_unc > 0) {
    params <- lapply(names(x), function(param_name) {
      Normal(mean = x[[param_name]], sd = sqrt(abs(x[[param_name]]) * rel_unc))
    })
    names(params) <- names(x)
  } else {
    params <- x
  }
  return(params)
}
