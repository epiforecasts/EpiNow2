#' Load and compile an EpiNow2 cmdstanr model
#'
#' The function has been adapted from a similar function in the epinowcast
#' package (Copyright holder: epinowcast authors, under MIT License).
#'
#' @param model A character string indicating the model to use. Needs to be
#' present in `dir`
#'
#' @param dir A character string specifying the path to any stan
#' files to include in the model. If missing the package default is used.
#'
#' @param verbose Logical, defaults to `TRUE`. Should verbose
#' messages be shown.
#'
#' @param ... Additional arguments passed to [cmdstanr::cmdstan_model()].
#'
#' @return A `cmdstanr` model.
#' @keywords internal
epinow2_cmdstan_model <- function(model,
                                  dir = system.file(
                                    "stan", package = "EpiNow2"
                                  ),
                                  verbose = FALSE,
                                  ...) {
  model_file <- file.path(
    dir, paste0(model, ".stan")
  )
  if (verbose) {
    message(sprintf("Using model %s.", model))
    message(sprintf("dir is %s.", toString(dir)))
  }

  monitor <- suppressMessages
  if (verbose) {
    monitor <- function(x) {
      return(x)
    }
  }
  model <- monitor(cmdstanr::cmdstan_model(
    model_file,
    include_paths = dir,
    ...
  ))
  return(model)
}

#' Load an EpiNow2 rstan model.
#'
#' The models are pre-compiled upon package install and is returned here.
#'
#' @param model A character string indicating the model to use. Needs to be
#' amongst the compiled models shipped with "EpiNow2".
#'
#' @return An `rstan` model.
#' @keywords internal
epinow2_rstan_model <- function(model) {
  return(stanmodels[[model]])
}

##' Return a stan model object for the appropriate backend
##'
##' @param model A character string indicating the model to use. One of
##' "estimate_infections" (default), "simulate_infections",
##' "estimate_secondary", "simulate_secondary", "estimate_truncation" or
##' "dist_fit".
##' @param ... Additional arguments passed to [epinow2_cmdstan_model()] or
##' [epinow2_rstan_model()], depending on the backend.
##' @inheritParams stan_opts
##' @return A stan model object (either \code{rstan::stanmodel} or
##'   \code{cmdstanr::CmdStanModel}, depending on the backend)
##' @importFrom rlang arg_match
##' @keywords internal
epinow2_stan_model <- function(backend = c("rstan", "cmdstanr"),
                               model = c("estimate_infections",
                                         "simulate_infections",
                                         "estimate_secondary",
                                         "simulate_secondary",
                                         "estimate_truncation",
                                         "dist_fit"),
                              ...) {
  backend <- arg_match(backend)
  model <- arg_match(model)
  if (backend == "cmdstanr") {
    object <- epinow2_cmdstan_model(model = model, ...)
  } else {
    object <- epinow2_rstan_model(model = model, ...)
  }
  return(object)
}

#' Fit a model using the chosen backend.
#'
#' Internal function for dispatch to fitting with NUTS or VB.
#' @inheritParams fit_model_with_nuts
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
    stop("method ", args$method, " unknown")
  }
  return(fit)
}
