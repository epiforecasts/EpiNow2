#' Discretised probability mass function
#'
#' @description `r lifecycle::badge("questioning")`
#' This function returns the probability mass function of a discretised and
#' truncated distribution defined by distribution type, maximum value and model
#' parameters.
#'
#' # Methodological details
#'
#' The probability mass function of the discretised probability distribution is
#'   a vector where the first entry corresponds to the integral over the (0,1]
#'   interval of the corresponding continuous distribution (probability of
#'   integer 0), the second entry corresponds to the (0,2] interval (probability
#'   mass of integer 1), the third entry corresponds to the (1, 3] interval
#'   (probability mass of integer 2), etc. This approximates the true
#'   probability mass function of a double censored distribution which arises
#'   from the difference of two censored events.
#'
#' @references
#' Charniga, K., et al. “Best practices for estimating and reporting
#'   epidemiological delay distributions of infectious diseases using public
#'   health surveillance and healthcare data”, *arXiv e-prints*, 2024.
#'   <https://doi.org/10.48550/arXiv.2405.08841>
#' Park,  S. W.,  et al.,  "Estimating epidemiological delay distributions for
#'   infectious diseases", *medRxiv*, 2024.
#'   <https://doi.org/10.1101/2024.01.12.24301247>
#'
#' @param distribution A character string representing the distribution to be
#'   used (one of "exp", "gamma", "lognormal", "normal" or "fixed")
#'
#' @param params A list of parameters values (by name) required for each model.
#' For the exponential model this is a rate parameter and for the gamma model
#' this is alpha and beta.
#'
#' @param max_value Numeric, the maximum value to allow.
#' Samples outside of this range are resampled.
#'
#' @param width Numeric, the width of each discrete bin.
#
#' @return A vector representing a probability distribution.
#' @keywords internal
#' @inheritParams bound_dist
#' @importFrom stats pexp pgamma plnorm pnorm qexp qgamma qlnorm qnorm
#' @importFrom rlang arg_match
discrete_pmf <- function(distribution =
                           c("exp", "gamma", "lognormal", "normal", "fixed"),
                         params, max_value, cdf_cutoff, width) {
  distribution <- arg_match(distribution)
  ## define unnormalised support function and cumulative density function
  if (distribution == "exp") {
    updist <- function(n) {
      pexp(n, params[["rate"]])
    }
    qdist <- qexp
  } else if (distribution == "gamma") {
    updist <- function(n) {
      pgamma(n, params[["shape"]], params[["rate"]])
    }
    qdist <- qgamma
  } else if (distribution == "lognormal") {
    updist <- function(n) {
      plnorm(n, params[["meanlog"]], params[["sdlog"]])
    }
    qdist <- qlnorm
  } else if (distribution == "normal") {
    updist <- function(n) {
      pnorm(n, params[["mean"]], params[["sd"]])
    }
    qdist <- qnorm
  } else if (distribution == "fixed") {
    updist <- function(n) {
      as.integer(n > params[["value"]])
    }
    qdist <- function(p, value) return(value)
  }

  ## apply CDF cutoff if given
  if (!missing(cdf_cutoff)) {
    ## max from CDF cutoff
    cdf_cutoff_max <- do.call(qdist, c(list(p = 1 - cdf_cutoff), params))
    if (missing(max_value) || cdf_cutoff_max < max_value) {
      max_value <- cdf_cutoff_max
    }
  }

  ## determine pmf
  max_value <- ceiling(max_value)
  if (max_value < width) {
    cmf <- c(0, 1)
  } else {
    x <- seq(width, max_value, by = width)
    cmf <- c(0, updist(width), (updist(x) + updist(x + width))) /
      (updist(max_value) + updist(max_value + width))
  }

  pmf <- diff(cmf)

  return(pmf)
}

#' Creates a delay distribution as the sum of two other delay distributions.
#'
#' @description `r lifecycle::badge("experimental")`
#' @return A delay distribution representing the sum of the two delays
#' @param e1 The first delay distribution (of type <dist_spec>) to
#' combine.
#'
#' @param e2 The second delay distribution (of type <dist_spec>) to
#' combine.
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
#' @description `r lifecycle::badge("experimental")`
#' This combines the parameters so that they can be fed as multiple delay
#' distributions to [epinow()] or [estimate_infections()].
#'
#' Note that distributions that already are combinations of other distributions
#' cannot be combined with other combinations of distributions.
#'
#' @param ... The delay distributions to combine
#' @importFrom cli cli_abort
#' @return Combined delay distributions (with class `<dist_spec>`)
#' @method c dist_spec
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
#' c(dist1, dist2)
c.dist_spec <- function(...) {
  ## process delay distributions
  dist_specs <- list(...)
  if (length(dist_specs) == 1) return(dist_specs[[1]])
  if (!(all(vapply(dist_specs, is, "dist_spec", FUN.VALUE = logical(1))))) {
    cli_abort(
      c(
        "!" = "All distributions must be of class {.cls dist_spec}."
      )
    )
  }
  convolutions <- vapply(
    dist_specs, is, "multi_dist_spec", FUN.VALUE = logical(1)
  )
  ## can only have one `multi_dist_spec`
  if (sum(convolutions) > 0) {
    if (sum(convolutions) > 1) {
      cli_abort(
        c(
          "!" = "Can't convolve convolutions with other convolutions"
        )
      )
    }
    ## preserve convolution attribute
    convolution_attributes <- attributes(dist_specs[[which(convolutions)]])
    dist_specs[!convolutions] <- lapply(dist_specs[!convolutions], list)
    dist_specs <- unlist(dist_specs, recursive = FALSE)
    attributes(dist_specs) <- convolution_attributes
  } else {
    attr(dist_specs, "class") <- c("multi_dist_spec", "dist_spec", "list")
  }

  return(dist_specs)
}

#' Returns the mean of one or more delay distribution
#'
#' @description `r lifecycle::badge("experimental")`
#' This works out the mean of all the (parametric / nonparametric) delay
#' distributions combined in the passed <dist_spec>.
#'
#' @param x The `<dist_spec>` to use
#' @param ... Not used
#' @param ignore_uncertainty Logical; whether to ignore any uncertainty in
#'   parameters. If set to FALSE (the default) then the mean of any uncertain
#'   parameters will be returned as NA.
#' @importFrom cli cli_abort
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
  if (get_distribution(x) == "nonparametric") {
    ## nonparametric
    pmf <- get_pmf(x)
    return(sum((seq_along(pmf) - 1) * pmf))
  } else {
    params <- get_parameters(x)
    if (!all(vapply(params, is.numeric, logical(1)))) {
      if (ignore_uncertainty) {
        params <- lapply(params, mean, ignore_uncertainty = TRUE)
      } else {
        return(NA_real_)
      }
    }
    dist <- get_distribution(x)
    if (dist == "lognormal") {
      return(exp(params$meanlog + params$sdlog**2 / 2))
    } else if (dist == "gamma") {
      return(params$shape / params$rate)
    } else if (dist == "normal") {
      return(params$mean)
    } else if (dist == "fixed") {
      return(params$value)
    } else {
      cli_abort(
        c(
          "!" = "Don't know how to calculate mean of {dist} distribution."
        )
      )
    }
  }
}

#' @method mean multi_dist_spec
#' @export
mean.multi_dist_spec <- function(x, ..., ignore_uncertainty = FALSE) {
  ret <- vapply(x, mean, ignore_uncertainty = ignore_uncertainty, numeric(1))
  return(ret)
}


sd <- function(x, ...) {
  UseMethod("sd")
}
#' Returns the standard deviation of one or more delay distribution
#'
#' @name sd
#' @description `r lifecycle::badge("experimental")`
#' This works out the standard deviation of all the (parametric /
#' nonparametric) delay distributions combined in the passed <dist_spec>.
#' If any of the parameters are themselves uncertain then `NA` is returned.
#'
#' @param x The <dist_spec> to use
#' @return A vector of standard deviations.
#' @importFrom utils head
#' @importFrom cli cli_abort
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' # A fixed lognormal distribution with sd 5 and sd 1.
#' dist1 <- LogNormal(mean = 5, sd = 1, max = 20)
#' sd(dist1)
#'
#' # A gamma distribution with mean 3 and sd 2
#' dist2 <- Gamma(mean = 3, sd = 2)
#' sd(dist2)
#'
#' # The sd of the sum of two distributions
#' sd(dist1 + dist2)
#' }
sd.dist_spec <- function(x, ...) {
  if (x$distribution == "nonparametric") {
    ## nonparametric
    mean_pmf <- sum((seq_along(x$pmf) - 1) * x$pmf)
    return(sum((seq_along(x$pmf) - 1)**2 * x$pmf) - mean_pmf^2)
  } else {
    ## parametric
    if (!all(vapply(x$parameters, is.numeric, logical(1)))) {
      return(NA_real_)
    }
    if (x$distribution == "lognormal") {
      sqrt(exp(x$parameters$sdlog**2) - 1) *
        exp(x$parameters$meanlog + 0.5 * x$parameters$sdlog**2)
    } else if (x$distribution == "gamma") {
      sqrt(x$parameters$shape / x$parameters$rate**2)
    } else if (x$distribution == "normal") {
      x$parameters$sd
    } else if (x$distribution == "fixed") {
      0
    } else {
      cli_abort(
        c(
          "!" = "Don't know how to calculate standard deviation of
        {x$distribution} distribution."
        )
      )
    }
  }
}

#' @export
sd.multi_dist_spec <- function(x, ...) {
  ret <- vapply(x, sd, numeric(1))
  return(ret)
}
#' @export
sd.default <- function(x, ...) {
  stats::sd(x)
}

#' Returns the maximum of one or more delay distribution
#'
#' @description `r lifecycle::badge("experimental")`
#' This works out the maximum of all the (parametric / nonparametric) delay
#' distributions combined in the passed <dist_spec> (ignoring any uncertainty
#' in parameters)
#'
#' @param x The <dist_spec> to use
#' @param ... Not used
#' @return A vector of means.
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
#' # The max the sum of two distributions
#' max(dist1 + dist2)
max.dist_spec <- function(x, ...) {
  ## try to discretise (which applies cdf cutoff and max)
  x <- discretise(x, strict = FALSE)
  if (get_distribution(x) == "nonparametric") {
    ## nonparametric
    return(length(get_pmf(x)) - 1)
  } else if (get_distribution(x) == "fixed") {
    return(get_parameters(x)$value)
  } else {
    if (is.null(attr(x, "max"))) {
      return(Inf)
    } else {
      return(attr(x, "max"))
    }
  }
}

#' @export
max.multi_dist_spec <- function(x, ...) {
  ret <- vapply(x, max, numeric(1))
  return(ret)
}

#' @export
discretise <- function(x, ...) {
  UseMethod("discretise")
}
#' Discretise a <dist_spec>
#'
#' @name discretise
#' @description `r lifecycle::badge("experimental")`
#'
#' @inherit discrete_pmf sections references
#' @param x A `<dist_spec>`
#' @param strict Logical; If `TRUE` (default) an error will be thrown if a
#' distribution cannot be discretised (e.g., because no finite maximum has been
#' specified or parameters are uncertain). If `FALSE` then any distribution
#' that cannot be discretised will be returned as is.
#' @param ... ignored
#' @importFrom cli cli_abort
#' @return A `<dist_spec>` where all distributions with constant parameters are
#'   nonparametric.
#' @export
#' @method discretise dist_spec
#' @examples
#' # A fixed gamma distribution with mean 5 and sd 1.
#' dist1 <- Gamma(mean = 5, sd = 1, max = 20)
#'
#' # An uncertain lognormal distribution with mean 3 and sd 2
#' dist2 <- LogNormal(mean = Normal(3, 0.5), sd = Normal(2, 0.5), max = 20)
#'
#' # The maxf the sum of two distributions
#' discretise(dist1 + dist2, strict = FALSE)
discretise.dist_spec <- function(x, strict = TRUE, ...) {
  ## discretise
  if (!is_constrained(x) && strict) {
    cli_abort(
      c(
        "!" = "Cannot discretise a distribution with infinite support.",
        "i" = "Either set a finite maximum or a tolerance greater than 0."
      )
    )
  }
  if (get_distribution(x) == "nonparametric") {
    return(x)
  } else {
    if (!is.na(sd(x)) && is_constrained(x)) {
        cdf_cutoff <- attr(x, "cdf_cutoff")
        if (is.null(cdf_cutoff)) {
          cdf_cutoff <- 0
        }
        max <- attr(x, "max")
        if (is.null(max)) {
          max <- Inf
        }
      y <- list(
        pmf = discrete_pmf(
          get_distribution(x), get_parameters(x), max, cdf_cutoff, width = 1
        )
      )
      y$distribution <- "nonparametric"
      preserve_attributes <- setdiff(
        names(attributes(x)), c("cdf_cutoff", "max", "names")
      )
      for (attribute in preserve_attributes) {
        attributes(y)[attribute] <- attributes(x)[attribute]
      }
      return(y)
    } else if (strict) {
      cli_abort(
        c(
          "!" = "Cannot discretise a distribution with uncertain parameters."
        )
      )
    } else {
      return(x)
    }
  }
}
#' @method discretise multi_dist_spec
#' @export
discretise.multi_dist_spec <- function(x, strict = TRUE, ...) {
  ret <- lapply(x, discretise, strict = strict)
  attributes(ret) <- attributes(x)
  return(ret)
}
#' @rdname discretise
#' @export
discretize <- discretise

#' @export
collapse <- function(x, ...) {
  UseMethod("collapse")
}
#' Collapse nonparametric distributions in a <dist_spec>
#'
#' @name collapse
#' @description `r lifecycle::badge("experimental")`
#' This convolves any consecutive nonparametric distributions contained
#' in the <dist_spec>.
#' @param x A `<dist_spec>`
#' @param ... ignored
#' @return A `<dist_spec>` where consecutive nonparametric distributions
#' have been convolved
#' @importFrom stats convolve
#' @importFrom cli cli_abort
#' @method collapse dist_spec
#' @export
#' @examples
#' # A fixed gamma distribution with mean 5 and sd 1.
#' dist1 <- Gamma(mean = 5, sd = 1, max = 20)
#'
#' # An uncertain lognormal distribution with mean 3 and sd 2
#' dist2 <- LogNormal(mean = 3, sd = 2, max = 20)
#'
#' # The maxf the sum of two distributions
#' collapse(discretise(dist1 + dist2))
collapse.dist_spec <- function(x, ...) {
  return(x)
}
#' @method collapse multi_dist_spec
#' @export
collapse.multi_dist_spec <- function(x, ...) {
  ## get nonparametric distributions
  nonparametric <- vapply(
    seq_along(x), get_distribution, x = x, character(1)
  ) == "nonparametric"
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
        get_pmf(x[[ids[id]]]), rev(get_pmf(x[[next_id]])), type = "open"
      )
    }
  }
  ## remove collapsed pmfs
  x[unlist(next_ids)] <- NULL
  ## if wev have collapsed all we turn into a single dist_spec
  if ((length(x) == 1) && is(x[[1]], "dist_spec")) x <- x[[1]]

  return(x)
}

#' Prints the parameters of one or more delay distributions
#'
#' @description `r lifecycle::badge("experimental")`
#' This displays the parameters of the uncertain and probability mass
#' functions of fixed delay distributions combined in the passed <dist_spec>.
#' @param x The `<dist_spec>` to use
#' @param ... Not used
#' @importFrom cli cli_abort
#' @return invisible
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
#' @keywords internal
.print.dist_spec <- function(x, indent, ...) {
  indent_str <- strrep(" ", indent)
  if (ndist(x) > 1) {
    cat(indent_str, "Composite distribution:\n", sep = "")
  }
  for (i in seq_len(ndist(x))) {
    if (get_distribution(x, i) == "nonparametric") {
      ## nonparametric
      cat(
        indent_str, "- nonparametric distribution\n", indent_str, "  PMF: [",
        paste(signif(get_pmf(x, i), digits = 2), collapse = " "), "]\n",
        sep = ""
      )
    } else if (get_distribution(x, i) == "fixed") {
      ## fixed
      cat(indent_str, "- fixed value:\n", sep = "")
      if (is.numeric(get_parameters(x, i)$value)) {
        cat(indent_str, "  ", get_parameters(x, i)$value, "\n", sep = "")
      } else {
        .print.dist_spec(get_parameters(x, i)$value, indent = indent + 4)
      }
    } else {
      ## parametric
      cat(indent_str, "- ",  get_distribution(x, i), " distribution", sep = "")
      dist <- extract_single_dist(x, i)
      constrain_str <- character(0)
      if (!is.null(attr(dist, "max")) && is.finite(attr(dist, "max"))) {
        constrain_str["max"] <- paste("max:", max(dist))
      }
      if (!is.null(attr(dist, "cdf_cutoff"))) {
        constrain_str["cdf_cutoff"] <-
          paste("cdf_cutoff:", attr(dist, "cdf_cutoff"))
      }
      if (length(constrain_str) > 0) {
        cat(" (", toString(constrain_str), ")", sep = "")
      }
      cat(":\n")
      ## loop over natural parameters and print
      for (param in names(get_parameters(x, i))) {
        cat(
          indent_str, "  ", param, ":\n", sep = ""
        )
        if (is.numeric(get_parameters(x, i)[[param]])) {
          cat(
            indent_str, "    ",
            signif(get_parameters(x, i)[[param]], digits = 2), "\n",
            sep = ""
          )
        } else {
          .print.dist_spec(get_parameters(x, i)[[param]], indent = indent + 4)
        }
      }
    }
  }
}

#' Plot PMF and CDF for a dist_spec object
#'
#' @description `r lifecycle::badge("experimental")`
#' This function takes a `<dist_spec>` object and plots its probability mass
#' function (PMF) and cumulative distribution function (CDF) using `{ggplot2}`.
#'
#' @param x A `<dist_spec>` object
#' @param samples Integer; Number of samples to generate for distributions
#' with uncertain parameters (default: 50).
#' @param res Numeric; Resolution of the PMF and CDF (default: 1, i.e. integer
#'   discretisation).
#' @param cumulative Logical; whether to plot the cumulative distribution in
#'   addition to the probability mass function
#' @param ... ignored
#' @importFrom ggplot2 aes geom_col geom_step facet_wrap vars theme_bw
#' @importFrom data.table data.table rbindlist
#' @importFrom cli cli_abort
#' @export
#' @examples
#' # A fixed lognormal distribution with mean 5 and sd 1.
#' dist1 <- LogNormal(mean = 1.6, sd = 0.5, max = 20)
#' # Plot discretised distribution with 1 day discretisation window
#' plot(dist1)
#' # Plot discretised distribution with 0.01 day discretisation window
#' plot(dist1, res = 0.01, cumulative = FALSE)
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' dist2 <- Gamma(
#'   mean = Normal(3, 0.5), sd = Normal(2, 0.5), max = 20
#' )
#' plot(dist2)
#'
#' # Multiple distributions with 0.1 discretisation window and do not plot the
#' # cumulative distribution
#' plot(dist1 + dist2, res = 0.1, cumulative = FALSE)
plot.dist_spec <- function(x, samples = 50L, res = 1, cumulative = TRUE, ...) {
  # Get the PMF and CDF data
  pmf_data <- lapply(seq_len(ndist(x)), function(i) {
    if (get_distribution(x, i) == "nonparametric") {
      # nonparametric
      pmf <- get_pmf(x, i)
      values <- seq_along(pmf) - 1
      dist_name <- paste0("Nonparametric", " (ID: ", i, ")")
      pmf_dt <- data.table(
        sample = 1, x = values, p = pmf, distribution = dist_name
      )
    } else {
      # parametric
      uncertain <- vapply(get_parameters(x, i), function(y) {
        if (is.numeric(y)) return(FALSE)
        sd_dist <- sd(y)
        return(is.na(sd_dist) || sd_dist > 0)
      }, logical(1))
      if (!any(uncertain)) {
        samples <- 1 ## only need 1 sample if fixed
      }
      dists <- lapply(seq_len(samples), function(y) {
        fix_parameters(extract_single_dist(x, i), strategy = "sample")
      })
      cdf_cutoff <- attr(x, "cdf_cutoff")
      if (is.null(cdf_cutoff)) {
        cdf_cutoff <- 0
      }
      pmf_dt <- lapply(dists, function(y) {
        if (is.infinite(attr(y, "max"))) {
          cli_abort(
            c(
              "!" = "All distributions in {.var x} must have a finite
              maximum value.",
              "i" = "You can set a finite maximum either as an
              argument to {.fn plot} or when defining the distribution."
            )
          )
        }
        x <- discrete_pmf(
          distribution = get_distribution(x, i), params = get_parameters(y),
          max_value = attr(y, "max"), cdf_cutoff = cdf_cutoff, width = res
        )
        return(data.table(x = (seq_along(x) - 1) * res, p = x))
      })
      pmf_dt <- rbindlist(pmf_dt, idcol = "sample")

      dist_name <- paste0(
        ifelse(any(uncertain), "Uncertain ", ""),
        get_distribution(x, i), " (ID: ", i, ")"
      )
      pmf_dt <- pmf_dt[, distribution := dist_name]
    }
    return(pmf_dt)
  })
  pmf_data <- rbindlist(pmf_data)[, `:=`(
    type = factor("pmf", levels = c("pmf", "cmf")),
    distribution = factor(distribution, levels = unique(distribution))
  )]

  # Plot PMF and CDF as facets in the same plot
  plot <- ggplot(
    pmf_data, mapping = aes(x = x, y = p, group = sample, linetype = type)
  ) +
    geom_line() +
    facet_wrap(vars(distribution)) +
    labs(x = "x", y = "Probability") +
    theme_bw()
  if (cumulative) {
    cmf_data <- pmf_data[,
      list(x = x, p = cumsum(p)), by = list(sample, distribution)
    ][,
      type := factor("cmf", levels = c("pmf", "cmf"))
    ]
    plot <- plot +
      geom_step(data = cmf_data)
  }
  return(plot)
}

#' Extract a single element of a composite `<dist_spec>`
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x A composite `dist_spec` object
#' @param i The index to extract
#' @importFrom cli cli_abort
#' @return A single `dist_spec` object
#' @keywords internal
#' @examples
#' dist1 <- LogNormal(mean = 1.6, sd = 0.5, max = 20)
#'
#' # An uncertain gamma distribution with mean 3 and sd 2
#' dist2 <- Gamma(
#'   mean = Normal(3, 0.5), sd = Normal(2, 0.5), max = 20
#' )
#'
#' # Multiple distributions
#' \dontrun{
#'   dist <- dist1 + dist2
#'   extract_single_dist(dist, 2)
#' }
extract_single_dist <- function(x, i) {
  if (i > ndist(x)) {
    cli_abort(
      c(
        "!" = "i must be less than the number of distributions.",
        "i" = "The number of distributions is {ndist(x)} whiles i is {i}."
      )
    )
  }
  if (ndist(x) == 1) {
    return(x)
  } else {
    return(x[[i]])
  }
}

#' @export
fix_parameters <- function(x, ...) {
  UseMethod("fix_parameters")
}
#' Fix the parameters of a `<dist_spec>`
#'
#' @name fix_parameters
#' @description `r lifecycle::badge("experimental")`
#' If the given `<dist_spec>` has any uncertainty, it is removed and the
#' corresponding distribution converted into a fixed one.
#' @return A `<dist_spec>` object without uncertainty
#' @export
#' @param x A `<dist_spec>`
#' @param strategy Character; either "mean" (use the mean estimates of the
#'   mean and standard deviation) or "sample" (randomly sample mean and
#'   standard deviation from uncertainty given in the `<dist_spec>`
#' @param ... ignored
#' @importFrom truncnorm rtruncnorm
#' @importFrom rlang arg_match
#' @method fix_parameters dist_spec
#' @examples
#' # An uncertain gamma distribution with mean 3 and sd 2
#' dist <- LogNormal(
#'   meanlog = Normal(3, 0.5), sdlog = Normal(2, 0.5), max = 20
#' )
#'
#' fix_parameters(dist)
fix_parameters.dist_spec <- function(x, strategy = c("mean", "sample"), ...) {
  ## match strategy argument to options
  strategy <- arg_match(strategy)

  ## if x is fixed already we don't have to do anything
  if (get_distribution(x) == "nonparametric" ||
      all(vapply(get_parameters(x), is.numeric, logical(1)))) {
    return(x)
  }
  ## apply strategy depending on choice
  if (strategy == "mean") {
    x$parameters <- lapply(get_parameters(x), mean)
  } else if (strategy == "sample") {
    lower_bound <-
      lower_bounds(get_distribution(x))[natural_params(get_distribution(x))]
    params_mean <- vapply(get_parameters(x), mean, numeric(1))
    params_sd <- vapply(get_parameters(x), sd, numeric(1))
    params_sd[is.na(params_sd)] <- 0
    sampled <- as.list(rtruncnorm(
      n = 1, a = lower_bound,
      mean = params_mean, sd = params_sd
    ))
    names(sampled) <- names(get_parameters(x))
    x$parameters <- sampled
  }
  return(x)
}

#' @export
#' @method fix_parameters multi_dist_spec
fix_parameters.multi_dist_spec <- function(x, strategy =
                                                c("mean", "sample"), ...) {
  for (i in seq_len(ndist(x))) {
    x[[i]] <- fix_parameters(x[[i]])
  }
  return(x)
}

#' @export
is_constrained <- function(x, ...) {
  UseMethod("is_constrained")
}
#' Check if a <dist_spec> is constrained, i.e. has a finite maximum or nonzero
#' CDF cutoff.
#'
#' @name is_constrained
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x A `<dist_spec>`
#' @param ... ignored
#' @return Logical; TRUE if `x` is constrained
#' @export
#' @method is_constrained dist_spec
#' @examples
#' # A fixed gamma distribution with mean 5 and sd 1.
#' dist1 <- Gamma(mean = 5, sd = 1, max = 20)
#'
#' # An uncertain lognormal distribution with mean 3 and sd 2
#' dist2 <- LogNormal(mean = Normal(3, 0.5), sd = Normal(2, 0.5), max = 20)
#'
#' # both distributions are constrained and therefore so is the sum
#' is_constrained(dist1 + dist2)
is_constrained.dist_spec <- function(x, ...) {
  if (get_distribution(x) %in% c("nonparametric", "fixed")) {
    return(TRUE)
  }
  cdf_cutoff <- attr(x, "cdf_cutoff")
  tol_constrained <- !is.null(cdf_cutoff) && cdf_cutoff > 0
  max <- attr(x, "max")
  max_constrained <- !is.null(max) && is.finite(max)
  return(tol_constrained || max_constrained)
}
#' @method is_constrained multi_dist_spec
#' @export
is_constrained.multi_dist_spec <- function(x, ...) {
  constrained <- vapply(x, is_constrained, logical(1))
  return(all(constrained))
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
#' @param ... arguments to define the limits of the distribution that will be
#' passed to [bound_dist()]
#' @return A `dist_spec` representing a distribution of the given
#' specification.
#' @export
#' @rdname Distributions
#' @name Distributions
#' @order 1
#' @examples
#' LogNormal(mean = 4, sd = 1)
#' LogNormal(mean = 4, sd = 1, max = 10)
#' LogNormal(mean = Normal(4, 1), sd = 1, max = 10)
LogNormal <- function(meanlog, sdlog, mean, sd, ...) {
  params <- as.list(environment())
  return(new_dist_spec(params, "lognormal", ...))
}

#' @inheritParams stats::GammaDist
#' @rdname Distributions
#' @title Probability distributions
#' @order 2
#' @export
#' @examples
#' Gamma(mean = 4, sd = 1)
#' Gamma(shape = 16, rate = 4)
#' Gamma(shape = Normal(16, 2), rate = Normal(4, 1))
#' Gamma(shape = Normal(16, 2), scale = Normal(1/4, 1))
Gamma <- function(shape, rate, scale, mean, sd, ...) {
  params <- as.list(environment())
  return(new_dist_spec(params, "gamma", ...))
}

#' @rdname Distributions
#' @order 3
#' @export
#' @examples
#' Normal(mean = 4, sd = 1)
#' Normal(mean = 4, sd = 1, max = 10)
Normal <- function(mean, sd, ...) {
  params <- as.list(environment())
  return(new_dist_spec(params, "normal", ...))
}

#' @rdname Distributions
#' @order 4
#' @param value Value of the fixed (delta) distribution
#' @export
#' @examples
#' Fixed(value = 3)
#' Fixed(value = 3.5)
Fixed <- function(value, ...) {
  params <- as.list(environment())
  return(new_dist_spec(params, "fixed"))
}

#' Generates a nonparametric distribution.
#'
#' @param pmf Probability mass of the given distribution; this is
#'   passed as a zero-indexed numeric vector (i.e. the fist entry represents
#'   the probability mass of zero). If not summing to one it will be normalised
#'   to sum to one internally.
#' @rdname Distributions
#' @order 5
#' @export
#' @examples
#' NonParametric(c(0.1, 0.3, 0.2, 0.4))
#' NonParametric(c(0.1, 0.3, 0.2, 0.1, 0.1))
NonParametric <- function(pmf, ...) {
  check_sparse_pmf_tail(pmf)
  params <- list(pmf = pmf / sum(pmf))
  return(new_dist_spec(params, "nonparametric"))
}

#' Get the names of the natural parameters of a distribution
#'
#' @description `r lifecycle::badge("experimental")`
#' These are the parameters used in the stan models. All other parameter
#' representations are converted to these using [convert_to_natural()] before
#' being passed to the stan models.
#' @param distribution Character; the distribution to use.
#' @return A character vector, the natural parameters.
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
#' @description `r lifecycle::badge("experimental")`
#' This is used to avoid sampling parameter values that have no support.
#' @return A numeric vector, the lower bounds.
#' @inheritParams natural_params
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

#' Define bounds of a `<dist_spec>`
#'
#' @description `r lifecycle::badge("experimental")`
#' This sets attributes for further processing
#' @param x A `<dist_spec>`.
#' @param max Numeric, maximum value of the distribution. The distribution will
#' be truncated at this value. Default: `Inf`, i.e. no maximum.
#' @param cdf_cutoff Numeric; the desired CDF cutoff. Any part of the
#' cumulative distribution function beyond 1 minus the value of this argument is
#' removed. Default: `0`, i.e. use the full distribution.
#' @importFrom cli cli_abort
#' @return a `<dist_spec>` with relevant attributes set that define its bounds
#' @export
bound_dist <- function(x, max = Inf, cdf_cutoff = 0) {
  if (!is(x, "dist_spec")) {
    cli_abort(
      c(
        "!" = "{.var x} must be of class {.cls dist_spec}.",
        "i" = "It is currently of class {.cls class(x)}."
      )
    )
  }
  ## if it is a single nonparametric distribution we apply the bounds directly
  if (ndist(x) == 1 && get_distribution(x) == "nonparametric") {
    pmf <- get_pmf(x)
    if (cdf_cutoff > 0) {
      cmf <- cumsum(pmf)
      pmf <- pmf[c(TRUE, (1 - cmf[-length(cmf)]) >= cdf_cutoff)]
    }
    if (is.finite(max) && (max + 1) > length(x$pmf)) {
      pmf <- pmf[seq(1, max + 1)]
    }
    x$pmf <- pmf / sum(pmf)
  } else {
    if (is.finite(max)) attr(x, "max") <- max
    if (cdf_cutoff > 0) attr(x, "cdf_cutoff") <- cdf_cutoff
  }
  return(x)
}

#' Extract parameter names
#' @description `r lifecycle::badge("experimental")`
#' Internal function for extracting given parameter names of a distribution
#' from the environment. Called by `new_dist_spec`
#'
#' @param params Given parameters (obtained using `as.list(environment())`)
#' @return A character vector of parameters and their values.
#' @inheritParams natural_params
#' @importFrom cli cli_abort
#' @keywords internal
extract_params <- function(params, distribution) {
  params <- params[!vapply(params, inherits, "name", FUN.VALUE = TRUE)]
  n_params <- length(natural_params(distribution))
  if (length(params) != n_params) {
    cli_abort(
      c(
        "!" = "Exactly {n_params} parameters of the {distribution}
        distribution must be specified.",
        "i" = "You have specified {length(params)} parameters, which is not
        equal to {n_params}."
      )
    )
  }
  return(params)
}

#' Internal function for generating a `dist_spec` given parameters and a
#' distribution.
#'
#' @description `r lifecycle::badge("experimental")`
#' This will convert all parameters to natural parameters before generating
#' a `dist_spec`. If they have uncertainty this will be done using sampling.
#' @param params Parameters of the distribution (including `max`)
#' @inheritParams extract_params
#' @inheritParams bound_dist
#' @importFrom purrr walk
#' @importFrom cli cli_abort cli_warn
#' @return A `dist_spec` of the given specification.
#' @export
#' @examples
#' new_dist_spec(
#'   params = list(mean = 2, sd = 1),
#'   distribution = "normal"
#' )
new_dist_spec <- function(params, distribution, max = Inf, cdf_cutoff = 0) {
  if (distribution == "nonparametric") {
    ## nonparametric distribution
    ret <- list(
      pmf = params$pmf,
      distribution = "nonparametric"
    )
  } else {
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
          cli_abort(
            c(
              "!" = "Parameter {param_name} must be greater than its
              lower bound {lb}.",
              "i" = "It is currently set to less than the lower bound."
            )
          )
        }
      }

      ## convert any unnatural parameters
      unnatural_params <- setdiff(names(params), natural_params(distribution))
      if (length(unnatural_params) > 0) {
        ## sample parameters if they are uncertain
        uncertain <- vapply(params, function(x) {
          if (is.numeric(x)) return(FALSE)
          sd_dist <- sd(x)
          return(is.na(sd_dist) || sd_dist > 0)
        }, logical(1))
        if (any(uncertain)) {
          #nolint start: duplicate_argument_linter
          cli_warn(
            c(
              "!" = "Uncertain {distribution} distribution specified in
              terms of parameters that are not the \"natural\" parameters of
              the distribution {natural_params(distribution)}.",
              "i" = "Converting using a crude and very approximate method
            that is likely to produce biased results.",
              "i" = "If possible it is preferable to specify the
            distribution directly in terms of the natural parameters."
            )
          )
          #nolint end
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
  }
  ## add class attribute
  attr(ret, "class") <- c("dist_spec", "list")

  ## apply bounds
  ret <- bound_dist(ret, max, cdf_cutoff)

  ## now we have a distribution with natural parameters - return dist_spec
  return(ret)
}

#' Internal function for converting parameters to natural parameters.
#'
#' @description `r lifecycle::badge("experimental")`
#' This is used for preprocessing before generating a `dist_spec` object
#' from a given set of parameters and distribution
#' @param params A numerical named parameter vector
#' @inheritParams natural_params
#' @importFrom cli cli_abort
#' @return A list with two elements, `params_mean` and `params_sd`, containing
#' mean and sd of natural parameters.
#' @keywords internal
#' @examples
#' \dontrun{
#' convert_to_natural(
#'   params = list(mean = 2, sd = 1),
#'   distribution = "gamma"
#' )
#' }
convert_to_natural <- function(params, distribution) {
  ## unnatural parameter means
  ux <- lapply(params, mean)
  if (anyNA(ux)) {
    cli_abort(
      c(
        "!" = "Cannot nest uncertainty in a distributions that is not
      specified with its natural parameters.",
        "i" = "Specify the distribution in terms of its natural
      parameters if you want to nest uncertainty."
      )
    )
  }
  ## estimate relative uncertainty of parameters
  sds <- vapply(params, sd, numeric(1))
  sds[is.na(sds)] <- 0
  rel_unc <- mean(sds^2 / unlist(ux))
  ## store natural parameters
  x <- list()
  if (distribution == "gamma") {
    ## given as mean and sd
    if ("mean" %in% names(ux) && "sd" %in% names(ux)) {
      x$shape <- ux$mean**2 / ux$sd**2
      x$rate <- x$shape / ux$mean
    } else {
      ## convert scale => rate
      if ("scale" %in% names(ux)) {
        x$rate <- 1 / ux$scale
      } else {
        x$rate <- ux$rate
      }
      x$shape <- ux$shape
    }
  } else if (distribution == "lognormal") {
    if ("mean" %in% names(params) && "sd" %in% names(params)) {
      x$meanlog <- log(ux$mean^2 / sqrt(ux$sd^2 + ux$mean^2))
      x$sdlog <- convert_to_logsd(ux$mean, ux$sd)
    } else {
      x$meanlog <- ux$meanlog
      x$sdlog <- ux$sdlog
    }
  }
  ## sort
  x <- x[natural_params(distribution)]
  if (anyNA(names(x))) {
    cli_abort(
      c(
        "!" = "Incompatible combination of parameters of a {distribution}
      distribution specified: {names(params)}."
      )
    )
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

##' Extracts an element of a `<dist_spec>`
##'
##' @param x A `<dist_spec>`.
##' @param id Integer; the id of the distribution to use (if x is a composite
##' distribution). If `x` is a single distribution this is ignored and can be
##' left at its default value of `NULL`.
##' @param element The element, i.e. "parameters", "pmf" or "distribution".
##' @importFrom cli cli_abort
##' @return The id to use.
##' @keywords internal
get_element <- function(x, id = NULL, element) {
  if (!is.null(id) && id > ndist(x)) {
    cli_abort(
      c(
        "!" = "{.var id} cannot be greater than the number of distributions
      ({length(x)}).",
        "i" = "{.var id} currently has length {length(id)}."
      )
    )
  }
  if (ndist(x) > 1) {
    if (is.null(id)) {
      cli_abort(
        c(
         "!" = "{.var id} must be specified when {.var x} is a composite
          distribution."
        )
      )
    }
    return(x[[id]][[element]])
  } else {
    return(x[[element]])
  }
}

##' Get parameters of a parametric distribution
##'
##' @inheritParams get_element
##' @description `r lifecycle::badge("experimental")`
##' @importFrom cli cli_abort
##' @return A list of parameters of the distribution.
##' @export
##' @examples
##' dist <- Gamma(shape = 3, rate = 2)
##' get_parameters(dist)
get_parameters <- function(x, id = NULL) {
  if (!is(x, "dist_spec")) {
    cli_abort(
      c(
        "!" = "Object must be of class {.cls dist_spec}",
        "i" = "You have supplied an object of class {.cls {class(x)}}."
      )
    )
  }
  if (get_distribution(x, id) == "nonparametric") {
    cli_abort(
      c(
        "!" = "To get parameters, distribution cannot not be
        \"nonparametric\".",
        "i" = "Distribution must be one of
        {col_blue(\"gamma\")}, {col_blue(\"lognormal\")},
        {col_blue(\"normal\")} or {col_blue(\"fixed\")}."
      )
    )
  }
  return(get_element(x, id, "parameters"))
}

##' Get the probability mass function of a nonparametric distribution
##'
##' @inheritParams get_element
##' @description `r lifecycle::badge("experimental")`
##' @return The pmf of the distribution
##' @importFrom cli cli_abort
##' @export
##' @examples
##' dist <- discretise(Gamma(shape = 3, rate = 2, max = 10))
##' get_pmf(dist)
get_pmf <- function(x, id = NULL) {
  if (!is(x, "dist_spec")) {
    cli_abort(
      c(
        "!" = "Can only get pmf of a {.cls dist_spec}.",
        "i" = "You have supplied an object of class {.cls {class(x)}}."
      )
    )
  }
  if (get_distribution(x, id) != "nonparametric") {
    cli_abort(
      c(
        "!" = "To get PMF, distribution must be \"nonparametric\"."
      )
    )
  }
  return(get_element(x, id, "pmf"))
}

##' Get the distribution of a `<dist_spec>`
##'
##' @inheritParams get_element
##' @description `r lifecycle::badge("experimental")`
##' @importFrom cli cli_abort
##' @return A character string naming the distribution (or "nonparametric")
##' @export
##' @examples
##' dist <- Gamma(shape = 3, rate = 2, max = 10)
##' get_distribution(dist)
get_distribution <- function(x, id = NULL) {
  if (!is(x, "dist_spec")) {
    cli_abort(
      c(
        "!" = "To get distribution of x, it must be a {.cls dist_spec}.",
        "i" = "You have supplied an object of class {.cls {class(x)}}."
      )
    )
  }
  return(get_element(x, id, "distribution"))
}

##' Calculate the number of distributions in a `<dist_spec>`
##'
##' @param x A `<dist_spec>` object.
##' @return The number of distributions.
##' @keywords internal
ndist <- function(x) {
  if (is(x, "multi_dist_spec")) {
    return(length(x))
  } else {
    return(1L)
  }
}
