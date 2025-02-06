#' Distribution Skeleton
#'
#' @description `r lifecycle::badge("deprecated")`
#' This function acts as a skeleton for a truncated distribution defined by
#' model type, maximum value and model parameters.
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
#' @keywords internal
#' @export
dist_skel <- function(n, dist = FALSE, cum = TRUE, model,
                      discrete = FALSE, params, max_value = 120) {
  lifecycle::deprecate_stop(
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
    cmf <- c(
      0, updist(1),
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
#' @importFrom cli cli_abort
#' @keywords internal
#' @export
apply_tolerance <- function(x, tolerance) {
  lifecycle::deprecate_stop(
    "1.6.0", "apply_tolerance()", "bound_dist()"
  )
  if (!is(x, "dist_spec")) {
    cli_abort(
      c(
        "!" = "Can only apply tolerance to distributions in a {.cls dist_spec}."
      )
    )
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

#' Remove uncertainty in the parameters of a `<dist_spec>`
#'
#' @description `r lifecycle::badge("deprecated")`
#' This function has been renamed to [fix_parameters()] as a more appropriate
#'   name.
#' @return A `<dist_spec>` object without uncertainty
#' @keywords internal
#' @export
#' @importFrom cli cli_abort
#' @param x A `<dist_spec>`
#' @param strategy Character; either "mean" (use the mean estimates of the
#'   mean and standard deviation) or "sample" (randomly sample mean and
#'   standard deviation from uncertainty given in the `<dist_spec>`
#' @keywords internal
#' @export
fix_dist <- function(x, strategy = c("mean", "sample")) {
  lifecycle::deprecate_stop(
    "1.6.0", "fix_dist()", "fix_parameters()"
  )
  if (!is(x, "dist_spec")) {
    cli_abort("!" = "Can only fix distributions in a <dist_spec>.")
  }
  fix_parameters(x, strategy)
}
