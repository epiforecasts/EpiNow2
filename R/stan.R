#' Load and compile an EpiNow2 cmdstanr model
#'
#' The function has been adapted from a similar function in the epinowcast
#' package (Copyright holder: epinowcast authors, under MIT License).
#'
#' @param model A character string indicating the model to use. Needs to be
#' present in `dir` (with extension `.stan`). Defaults to "estimate_infections".
#'
#' @param dir A character string specifying the path to any stan
#' files to include in the model. If missing the package default is used.
#'
#' @param verbose Logical, defaults to `TRUE`. Should verbose
#' messages be shown.
#'
#' @param ... Additional arguments passed to [cmdstanr::cmdstan_model()].
#'
#' @importFrom cli cli_inform col_blue
#' @return A `cmdstanr` model.
#' @export
epinow2_cmdstan_model <- function(model = "estimate_infections",
                                  dir = system.file(
                                    "stan",
                                    package = "EpiNow2"
                                  ),
                                  verbose = FALSE,
                                  ...) {
  model_file <- file.path(
    dir, paste0(model, ".stan")
  )
  if (verbose) {
    cli_inform("Using model {col_blue(model)}.")
    cli_inform("{.var dir} is {.file {dir}}.")
  }

  monitor <- suppressMessages
  if (verbose) {
    monitor <- function(x) {
      x
    }
  }
  model <- monitor(cmdstanr::cmdstan_model(
    model_file,
    include_paths = dir,
    dir = tempdir(),
    ...
  ))
  model
}

#' Load an EpiNow2 rstan model.
#'
#' The models are pre-compiled upon package install and is returned here.
#'
#' @param model A character string indicating the model to use. Needs to be
#' amongst the compiled models shipped with "EpiNow2" (see the `stan` directory
#' for a list). Defaults to "estimate_infections".
#'
#' @return An `rstan` model.
#' @keywords internal
epinow2_rstan_model <- function(model = "estimate_infections") {
  stanmodels[[model]]
}

##' Return a stan model object for the appropriate backend
##'
##' @param model A character string indicating the model to use. One of
##' "estimate_infections" (default), "simulate_infections",
##' "estimate_secondary", "simulate_secondary", "estimate_truncation",
##' "estimate_dist", or "dist_fit".
##' @inheritParams stan_opts
##' @return A stan model object (either \code{rstan::stanmodel} or
##'   \code{cmdstanr::CmdStanModel}, depending on the backend)
##' @importFrom rlang arg_match
##' @keywords internal
epinow2_stan_model <- function(backend = c("rstan", "cmdstanr"),
                               model = c(
                                 "estimate_infections",
                                 "simulate_infections",
                                 "estimate_secondary",
                                 "simulate_secondary",
                                 "estimate_truncation",
                                 "estimate_dist",
                                 "dist_fit"
                               )) {
  backend <- arg_match(backend)
  model <- arg_match(model)
  if (backend == "cmdstanr") {
    object <- epinow2_cmdstan_model(model = model)
  } else {
    object <- epinow2_rstan_model(model = model)
  }
  object
}

#' Fit a model using the chosen backend.
#'
#' Internal function for dispatch to fitting with NUTS or VB.
#' @inheritParams fit_model_with_nuts
#' @importFrom cli cli_abort
#' @keywords internal
fit_model <- function(args, id = "stan") {
  if (args$method == "sampling") {
    fit <- fit_model_with_nuts(
      args,
      future = args$future,
      max_execution_time = args$max_execution_time, id = id
    )
  } else if (args$method %in% c("vb", "laplace", "pathfinder")) {
    fit <- fit_model_approximate(args, id = id)
  } else {
    cli_abort(
      c(
        "!" = "You supplied method {args$method}, which is unknown.",
        "i" = "Use one of {col_blue(\"sampling\")}, {col_blue(\"vb\")},
      {col_blue(\"laplace\")}, or {col_blue(\"pathfinder\")}."
      )
    )
  }
  fit
}

#' Build Stan arguments and fit an estimation model
#'
#' @description
#' Shared fitting step for the `estimate_*()` functions. Builds the Stan
#' arguments with [create_stan_args()], warns if the truncation distribution
#' is longer than the observed time period, and fits the model with
#' [fit_model()].
#'
#' @param time_points Integer length of the observed time period passed to
#'   [check_truncation_length()]. If `NULL` (the default) the check is
#'   skipped.
#' @param id Character, identifier used in logging. Defaults to `model`.
#' @inheritParams create_stan_args
#' @return A fitted model object as returned by [fit_model()].
#' @keywords internal
fit_estimate_model <- function(stan, data, init, model, id = model,
                               verbose = FALSE, time_points = NULL) {
  stan_args <- create_stan_args(
    stan = stan, data = data, init = init, model = model, verbose = verbose
  )
  if (!is.null(time_points)) {
    check_truncation_length(stan_args, time_points = time_points)
  }
  fit_model(stan_args, id = id)
}

#' Construct a fitted model object
#'
#' @description
#' Combines the components of a fitted model into a list with class
#' `c(class, "epinowfit", "list")`.
#'
#' @param ... Named components of the object, in the order they should
#'   appear.
#' @param class Character, the model-specific class.
#' @return A list of class `c(class, "epinowfit", "list")`.
#' @keywords internal
new_epinowfit <- function(..., class) {
  out <- list(...)
  class(out) <- c(class, "epinowfit", class(out))
  out
}
