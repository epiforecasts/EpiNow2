#' Fit a Stan Model using the NUTs sampler
#'
#' @description `r lifecycle::badge("maturing")`
#' Fits a stan model using [rstan::sampling()]. Provides the optional ability to
#' run chains using `future` with error catching, timeouts and merging of
#' completed chains.
#'
#' @param args List of stan arguments.
#'
#' @param future Logical, defaults to `FALSE`. Should `future` be used to run
#' stan chains in parallel.
#'
#' @param max_execution_time Numeric, defaults to Inf. What is the maximum
#' execution time per chain in seconds. Results will still be returned as long
#' as at least 2 chains complete successfully within the timelimit.
#'
#' @param id A character string used to assign logging information on error.
#' Used by [regional_epinow()] to assign errors to regions. Alter the default to
#' run with error catching.
#'
#' @importFrom futile.logger flog.debug flog.info flog.error
#' @importFrom R.utils withTimeout
#' @importFrom purrr compact
#' @importFrom rstan sflist2stanfit sampling
#' @importFrom rlang abort cnd_muffle
#' @return A stan model object
#' @keywords internal
fit_model_with_nuts <- function(args, future = FALSE, max_execution_time = Inf,
                                id = "stan") {
  method <- args$method
  args$method <- NULL
  args$max_execution_time <- NULL
  args$future <- NULL

  log_msg <- create_sampling_log_message(args, method)
  futile.logger::flog.debug(
    log_msg,
    id,
    name = "EpiNow2.epinow.estimate_infections.fit"
  )

  if (exists("stuck_chains", args)) {
    stuck_chains <- args$stuck_chains
    args$stuck_chains <- NULL
  } else {
    stuck_chains <- 0
  }

  fit_chain <- function(chain, stan_args, max_time, catch = FALSE) {
    stan_args$chain_id <- chain
    if (inherits(stan_args$object, "stanmodel")) {
      sample_func <- rstan::sampling
    } else if (inherits(stan_args$object, "CmdStanModel")) {
      sample_func <- stan_args$object$sample
      stan_args$object <- NULL
    }
    if (catch) {
      fit <- tryCatch(
        withCallingHandlers(
          R.utils::withTimeout(do.call(sample_func, stan_args),
            timeout = max_time,
            onTimeout = "silent"
          ),
          warning = function(w) {
            futile.logger::flog.warn(
              "%s (chain: %s): %s - %s", id, chain, w$message, toString(w$call),
              name = "EpiNow2.epinow.estimate_infections.fit"
            )
            rlang::cnd_muffle(w)
          }
        ),
        error = function(e) {
          error_text <- sprintf(
            "%s (chain: %s): %s - %s", id, chain, e$message, toString(e$call)
          )
          futile.logger::flog.error(error_text,
            name = "EpiNow2.epinow.estimate_infections.fit"
          )
          NULL
        }
      )
    } else {
      fit <- R.utils::withTimeout(do.call(sample_func, stan_args),
        timeout = max_time,
        onTimeout = "silent"
      )
    }

    if ((inherits(fit, "stanfit") && fit@mode != 2L) ||
          inherits(fit, "CmdStanMCMC")) {
      fit
    } else {
      NULL
    }
  }

  if (future) {
    chains <- args$chains
    args$chains <- 1
    args$cores <- 1
    fits <- lapply_func(seq_len(chains),
      fit_chain,
      stan_args = args,
      max_time = max_execution_time,
      catch = TRUE
    )
    if (stuck_chains > 0) {
      fits[seq_len(stuck_chains)] <- NULL
    }
    fit <- purrr::compact(fits)
    if (length(fit) == 0) {
      fit <- NULL
      if (is.null(fit)) {
        rlang::abort(
          "all chains failed - try inspecting the output for errors or",
          " increasing the max_execution_time"
        )
      }
    } else {
      failed_chains <- chains - length(fit)
      if (failed_chains > 0) {
        futile.logger::flog.warn(
          "%s: %s chains failed or were timed out.", id, failed_chains,
          name = "EpiNow2.fit"
        )
        if ((chains - failed_chains) < 2) {
          rlang::abort(
            "model fitting failed as too few chains were returned to assess",
            " convergence (2 or more required)"
          )
        }
      }
      fit <- rstan::sflist2stanfit(fit)
    }
  } else {
    fit <- fit_chain(seq_len(args$chains),
      stan_args = args, max_time = max_execution_time,
      catch = !id %in% c("estimate_infections", "epinow")
    )
    if (stuck_chains > 0) {
      fit <- NULL
    }
    if (is.null(fit)) {
      rlang::abort("model fitting was timed out or failed")
    }
  }
  return(fit)
}

#' Fit a Stan Model using an approximate method
#'
#' @description `r lifecycle::badge("maturing")`
#' Fits a stan model using variational inference.
#'
#' @inheritParams fit_model_with_nuts
#' @importFrom futile.logger flog.debug flog.info flog.error
#' @importFrom purrr safely
#' @importFrom rstan vb
#' @importFrom rlang abort
#' @importFrom cli cli_abort
#' @importFrom utils capture.output
#' @return A stan model object
#' @keywords internal
fit_model_approximate <- function(args, future = FALSE, id = "stan") {
  method <- args$method
  args$method <- NULL
  log_msg <- create_sampling_log_message(args, method)
  futile.logger::flog.debug(
    log_msg,
    id,
    name = "EpiNow2.epinow.estimate_infections.fit"
  )

  if (exists("trials", args)) {
    trials <- args$trials
    args$trials <- NULL
  } else {
    trials <- 1
  }

  fit_approximate <- function(stan_args) {
    if (inherits(stan_args$object, "stanmodel")) {
      if (method == "vb") {
        sample_func <- rstan::vb
      } else {
        cli_abort(
          c(
            "!" = "Laplace approximation only available in the cmdstanr
            backend.",
            "i" = "You've supplied {.strong {method}}."
          )
        )
      }
    } else if (inherits(stan_args$object, "CmdStanModel")) {
      if (method == "vb") {
        sample_func <- stan_args$object$variational
      } else if (method == "laplace") {
        sample_func <- stan_args$object$laplace
      } else {
        sample_func <- stan_args$object$pathfinder
      }
      stan_args$object <- NULL
    }
    fit <- do.call(sample_func, stan_args)

    if (length(names(fit)) == 0) {
      NULL
    } else {
      fit
    }
  }
  safe_fit <- purrr::safely(fit_approximate) # nolint
  fit <- NULL
  current_trials <- 0

  while (current_trials <= trials && is.null(fit)) {
    fit <- safe_fit(args)

    error <- fit[[2]]
    fit <- fit[[1]]
    if (is(fit, "CmdStanFit") && fit$return_codes() > 0) {
      error <- tail(capture.output(fit$output()), 1)
      fit <- NULL
    }
    current_trials <- current_trials + 1
  }

  if (is.null(fit)) {
    futile.logger::flog.error(
      paste(
        "%s: Fitting failed - try increasing stan_args$trials or inspecting",
        "the model input"
      ),
      id,
      name = "EpiNow2.fit"
    )
    rlang::abort(paste("Approximate inference failed due to:", error))
  }
  return(fit)
}

#' Create sampling log message
#'
#' @description Internal function that creates a formatted log message
#' describing the sampling parameters. The message format varies by method,
#' with different information shown for exact sampling vs approximate methods
#' (VB, Laplace, Pathfinder). Optionally includes time steps and forecast
#' horizon if present.
#'
#' @param args List of stan arguments containing:
#'   - object: Stan model object (CmdStanModel or stanmodel)
#'   - For sampling method: iter_sampling, iter_warmup (cmdstanr) or iter,
#'     warmup (rstan), chains
#'   - For vb method: iter, trials, output_samples
#'   - For laplace method: trials
#'   - For pathfinder method: trials, draws
#'   - data: List potentially containing t (time steps) and horizon (forecast)
#' @param method Character string indicating the sampling method. One of
#'   "sampling" (exact MCMC), "vb" (variational Bayes), "laplace" (Laplace
#'   approximation), or "pathfinder" (pathfinder algorithm).
#'
#' @return A character string containing the formatted log message with a %s
#'   placeholder for the id parameter (to be filled by sprintf or flog.debug)
#'
#' @keywords internal
create_sampling_log_message <- function(args, method) {
  # Build base message depending on method
  log_msg <- switch(method,
    "sampling" = {
      # Exact mode - calculate parameters based on backend
      if (inherits(args$object, "CmdStanModel")) {
        total_samples <- args$iter_sampling * args$chains
        warmup_iterations <- args$iter_warmup
      } else {
        total_samples <- (args$iter - args$warmup) * args$chains
        warmup_iterations <- args$warmup
      }

      paste0(
        "%s: Running in exact mode for ", total_samples,
        " samples (across ", args$chains,
        " chains each with a warm up of ", warmup_iterations,
        " iterations each)"
      )
    },
    "vb" = {
      # VB approximate mode
      paste0(
        "%s: Running in approximate mode for ", args$iter,
        " iterations (with ", args$trials, " attempts). Extracting ",
        args$output_samples, " approximate posterior samples"
      )
    },
    "laplace" = {
      # Laplace approximate mode
      paste0(
        "%s: Running in approximate mode using Laplace approximation (with ",
        args$trials, " attempts)"
      )
    },
    "pathfinder" = {
      # Pathfinder approximate mode
      paste0(
        "%s: Running in approximate mode using pathfinder (with ",
        args$trials, " attempts). Extracting ",
        args$draws, " approximate posterior samples"
      )
    }
  )

  # Add time steps if present (for all methods)
  if (!is.null(args$data$t)) {
    log_msg <- paste0(log_msg, " for ", args$data$t, " time steps")
    # Add forecast horizon if present
    if (!is.null(args$data$horizon)) {
      log_msg <- paste0(
        log_msg, " of which ", args$data$horizon, " are a forecast"
      )
    }
  }

  log_msg
}
