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

#' Expand `#include` directives in a Stan file
#'
#' Stan's `#include` directives are resolved by the Stan compiler using a set
#' of include paths. The `stanli` backend takes model source as a single
#' string and has no include path mechanism, so the directives are resolved
#' here instead.
#'
#' Directives are matched as a whole line of the form `#include path`, with
#' the path optionally wrapped in quotes or angle brackets. Each target is
#' searched for relative to the directory of the including file first and
#' then relative to each of `include_paths` in turn.
#'
#' @param file A character string giving the path to the Stan file to expand.
#'
#' @param include_paths A character vector of directories to search for
#' included files.
#'
#' @param parents A character vector of files currently being expanded, used
#' to detect circular includes. Not intended to be set by the user.
#'
#' @importFrom cli cli_abort
#' @return A character vector of lines with all includes expanded.
#' @keywords internal
expand_stan_includes <- function(file, include_paths, parents = character()) {
  file <- normalizePath(file, mustWork = TRUE)
  if (file %in% parents) {
    cli_abort(
      c(
        "!" = "Circular {.code #include} detected in {.file {file}}.",
        "i" = "Include chain: {.file {c(parents, file)}}."
      )
    )
  }
  src_lines <- readLines(file, warn = FALSE)
  pattern <- "^\\s*#include\\s+[\"<]?([^\"> ]+)[\">]?\\s*$"
  expanded <- lapply(src_lines, function(line) {
    matched <- regmatches(line, regexec(pattern, line))[[1]]
    if (length(matched) != 2L) {
      return(line)
    }
    target <- matched[2L]
    search_dirs <- c(dirname(file), include_paths)
    candidates <- file.path(search_dirs, target)
    found <- candidates[file.exists(candidates)]
    if (length(found) == 0L) {
      cli_abort(
        c(
          "!" = "Could not find included file {.file {target}}.",
          "i" = "Searched in {.file {search_dirs}}."
        )
      )
    }
    expand_stan_includes(found[1L], include_paths, c(parents, file))
  })
  unlist(expanded, use.names = FALSE)
}

#' Load an EpiNow2 stanli model
#'
#' Reads the Stan source for a model, resolves its `#include` directives with
#' [expand_stan_includes()] and hands the result to [stanli::cstan_model()].
#' Unlike the `rstan` and `cmdstanr` backends this does not compile the model;
#' `stanli` interprets the model against precompiled kernels and prepares it
#' when sampling starts.
#'
#' @inheritParams epinow2_cmdstan_model
#'
#' @importFrom cli cli_inform cli_abort col_blue
#' @return A `stanli_cstanmodel` model object.
#' @export
epinow2_stanli_model <- function(model = "estimate_infections",
                                 dir = system.file(
                                   "stan",
                                   package = "EpiNow2"
                                 ),
                                 verbose = FALSE) {
  check_stanli_available()
  model_file <- file.path(dir, paste0(model, ".stan"))
  if (verbose) {
    cli_inform("Using model {col_blue(model)}.")
    cli_inform("{.var dir} is {.file {dir}}.")
  }
  code <- paste(expand_stan_includes(model_file, dir), collapse = "\n")
  stanli::cstan_model(code)
}

#' Check that the stanli backend can be used
#'
#' Errors informatively if the `stanli` package is not installed or if its
#' runtime library has not been downloaded.
#'
#' @importFrom cli cli_abort col_blue
#' @return Invisibly returns `TRUE` if `stanli` is usable.
#' @keywords internal
check_stanli_available <- function() {
  if (!requireNamespace("stanli", quietly = TRUE)) {
    cli_abort(
      c(
        "x" = "The {col_blue('stanli')} R package is not installed.",
        "i" = "Install it with
        {.code install.packages(\"stanli\", repos =
        \"https://seantalts.r-universe.dev\")} to use the
        {col_blue('stanli')} backend."
      )
    )
  }
  if (!stanli::stanli_available()) {
    cli_abort(
      c(
        "x" = "The {col_blue('stanli')} runtime is not installed.",
        "i" = "Run {.code stanli::stanli_install()} to download it."
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
##'   \code{cmdstanr::CmdStanModel} or \code{stanli_cstanmodel}, depending on
##'   the backend)
##' @importFrom rlang arg_match
##' @keywords internal
epinow2_stan_model <- function(backend = c("rstan", "cmdstanr", "stanli"),
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
  object <- switch(backend,
    cmdstanr = epinow2_cmdstan_model(model = model),
    stanli = epinow2_stanli_model(model = model),
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
