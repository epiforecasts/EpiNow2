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

#' Load an EpiNow2 stanr model
#'
#' Builds a model with [stanr::stan_model()]. The `compiled` backend
#' translates the model to C++ and compiles it, caching the result. The
#' `stanli` backend interprets the model over precompiled kernels instead, so
#' it needs no C++ toolchain and preparing a model takes milliseconds.
#'
#' @inheritParams epinow2_cmdstan_model
#'
#' @param backend A character string giving the `stanr` model backend to use,
#' either "compiled" (default) or "stanli".
#'
#' @importFrom cli cli_inform col_blue
#' @importFrom rlang arg_match
#' @return A `StanModel` object.
#' @export
epinow2_stanr_model <- function(model = "estimate_infections",
                                dir = system.file(
                                  "stan",
                                  package = "EpiNow2"
                                ),
                                backend = c("compiled", "stanli"),
                                verbose = FALSE) {
  check_stanr_available()
  backend <- arg_match(backend)
  model_file <- file.path(dir, paste0(model, ".stan"))
  if (verbose) {
    cli_inform("Using model {col_blue(model)}.")
    cli_inform("{.var dir} is {.file {dir}}.")
  }
  stanr::stan_model(
    stan_file = model_file,
    include_paths = dir,
    backend = backend,
    quiet = !verbose
  )
}

#' Check that the stanr backends can be used
#'
#' Errors informatively if the `stanr` package is not installed.
#'
#' @importFrom cli cli_abort col_blue
#' @return Invisibly returns `TRUE` if `stanr` is available.
#' @keywords internal
check_stanr_available <- function() {
  if (!requireNamespace("stanr", quietly = TRUE)) {
    cli_abort(
      c(
        "x" = "The {col_blue('stanr')} R package is not installed.",
        "i" = "Install it with
        {.code install.packages(\"stanr\", repos =
        c(\"https://andrjohns.r-universe.dev\",
        \"https://cloud.r-project.org\"))}."
      )
    )
  }
  invisible(TRUE)
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
##' @return A stan model object (one of \code{rstan::stanmodel},
##'   \code{cmdstanr::CmdStanModel} or \code{stanr::StanModel}, depending on
##'   the backend)
##' @importFrom rlang arg_match
##' @keywords internal
epinow2_stan_model <- function(
  backend = c("rstan", "cmdstanr", "stanr", "stanli"),
  model = c(
    "estimate_infections",
    "simulate_infections",
    "estimate_secondary",
    "simulate_secondary",
    "estimate_truncation",
    "estimate_dist",
    "dist_fit"
  )
) {
  backend <- arg_match(backend)
  model <- arg_match(model)
  object <- switch(backend,
    cmdstanr = epinow2_cmdstan_model(model = model),
    stanr = epinow2_stanr_model(model = model, backend = "compiled"),
    stanli = epinow2_stanr_model(model = model, backend = "stanli"),
    epinow2_rstan_model(model = model)
  )
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
