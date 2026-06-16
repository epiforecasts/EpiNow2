#' Time-varying parameter processes
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These constructors mark a model parameter as varying over time, driven by a
#' stochastic process. `GP()` uses an approximate Gaussian process and `RW()` a
#' random walk. They wrap a `<dist_spec>` giving the prior on the value the
#' parameter reverts to (`mean`, a stationary / mean-reverting process) or on its
#' initial value (`init`, a process on first differences). Exactly one of `mean`
#' or `init` must be supplied.
#'
#' The wrapped prior may itself be a known trajectory (e.g. a `NonParametric()`
#' built from a data column), in which case the process fits deviations around
#' that mean. The link function applied to the resulting trajectory (e.g. log for
#' positive parameters, logit for probabilities) is a property of the parameter
#' and is set where the parameter is registered, not here.
#'
#' @details
#' The process specification deliberately carries only the value/process part of
#' a time-varying parameter. The level (whether the overall value is estimated or
#' `Fixed()`) and the link function are handled where the parameter is used.
#'
#' These functions define the user interface for time-varying parameters. Wiring
#' them into the models is ongoing; passing a process specification where a model
#' parameter is expected currently raises an informative error.
#'
#' @param mean A `<dist_spec>` giving the prior on the (stationary) mean the
#'   process reverts to, or a numeric vector giving a known mean trajectory the
#'   process fits deviations around. Supply either `mean` or `init`, not both.
#' @param init A `<dist_spec>` giving the prior on the initial value of a process
#'   on first differences, or a numeric vector giving known initial value(s).
#'   Supply either `mean` or `init`, not both.
#' @return A `<process_spec>` object describing the time-varying process.
#' @name process
#' @rdname process
NULL

#' Construct a process specification
#'
#' @param type Character, the process type (`"gp"` or `"rw"`).
#' @param mean,init A `<dist_spec>`; exactly one must be supplied.
#' @param settings A list of additional process settings (e.g. a `<gp_opts>`
#'   object for `"gp"`, or the step standard deviation prior for `"rw"`).
#' @return A `<process_spec>` object.
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_class
#' @keywords internal
new_process_spec <- function(type, mean, init, settings = list()) {
  has_mean <- !missing(mean) && !is.null(mean)
  has_init <- !missing(init) && !is.null(init)
  if (has_mean + has_init != 1) {
    cli_abort(
      c(
        "!" = "Exactly one of {.arg mean} or {.arg init} must be supplied.",
        "i" = "Use {.arg mean} for a mean-reverting (stationary) process or
        {.arg init} for a process on first differences."
      )
    )
  }
  anchor <- if (has_mean) "mean" else "init"
  prior <- if (has_mean) mean else init
  ## the anchor may be a prior (a <dist_spec>) or a known trajectory supplied as
  ## a numeric vector (the process then fits deviations around it)
  if (!is(prior, "dist_spec") && !is.numeric(prior)) {
    cli_abort(
      c(
        "!" = "{.arg {anchor}} must be a {.cls dist_spec} or a numeric vector.",
        "i" = "Supply a prior (e.g. {.fn Normal}) or a known trajectory as a
        numeric vector."
      )
    )
  }

  process <- list(
    type = type,
    anchor = anchor,
    prior = prior,
    settings = settings
  )
  class(process) <- c(
    paste0(type, "_process"), "process_spec", "list"
  )
  process
}

#' @rdname process
#' @param ... Additional Gaussian process settings passed to [gp_opts()] (e.g.
#'   `ls`, `alpha`, `kernel`).
#' @export
#' @examples
#' # mean-reverting Gaussian process
#' GP(mean = Normal(mean = 5, sd = 1))
#' # Gaussian process on first differences
#' GP(init = Normal(mean = 5, sd = 1))
GP <- function(mean, init, ...) {
  new_process_spec("gp", mean, init, settings = gp_opts(...))
}

#' @rdname process
#' @param sd A `<dist_spec>` giving the prior on the random walk step standard
#'   deviation. Defaults to a half-normal `Normal(mean = 0, sd = 0.1)` (the lower
#'   limit of 0 is enforced where the parameter is used).
#' @importFrom checkmate assert_class
#' @export
#' @examples
#' # random walk with an initial-value prior
#' RW(init = Normal(mean = 5, sd = 1))
#' # mean-reverting random walk with a custom step size prior
#' RW(mean = Normal(mean = 5, sd = 1), sd = Normal(mean = 0, sd = 0.05))
RW <- function(mean, init, sd = Normal(mean = 0, sd = 0.1)) {
  assert_class(sd, "dist_spec")
  new_process_spec("rw", mean, init, settings = list(sd = sd))
}

#' Test whether an object is a time-varying process specification
#'
#' @param x An object to test.
#' @return Logical, `TRUE` if `x` is a `<process_spec>`.
#' @keywords internal
is_process_spec <- function(x) {
  inherits(x, "process_spec")
}

#' @export
print.process_spec <- function(x, ...) {
  type <- if (x$type == "gp") "Gaussian process" else "random walk"
  variant <- if (x$anchor == "mean") "mean-reverting" else "on first differences"
  cat(
    "Time-varying parameter: ", type, " (", variant, ")\n", sep = ""
  )
  if (is.numeric(x$prior)) {
    label <- if (x$anchor == "mean") {
      "known mean trajectory"
    } else {
      "known initial value(s)"
    }
    cat("- ", label, ": ", paste(x$prior, collapse = " "), "\n", sep = "")
  } else {
    label <- if (x$anchor == "mean") "mean prior" else "initial-value prior"
    cat("- ", label, ":\n", sep = "")
    print(x$prior)
  }
  if (x$type == "rw") {
    cat("- step sd prior:\n", sep = "")
    print(x$settings$sd)
  }
  invisible(x)
}
