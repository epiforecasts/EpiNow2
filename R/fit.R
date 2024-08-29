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
#' @importFrom future.apply future_lapply
#' @importFrom purrr compact
#' @importFrom rstan sflist2stanfit sampling
#' @importFrom rlang abort cnd_muffle
#' @return A stan model object
#' @keywords internal
fit_model_with_nuts <- function(args, future = FALSE, max_execution_time = Inf,
                                id = "stan") {
  args$method <- NULL
  args$max_execution_time <- NULL
  args$future <- NULL

  futile.logger::flog.debug(
    paste0(
      "%s: Running in exact mode for ",
      ceiling(args$iter - args$warmup) * args$chains,
      " samples (across ", args$chains,
      " chains each with a warm up of ", args$warmup, " iterations each) and ",
      args$data$t, " time steps of which ", args$data$horizon, " are a forecast"
    ),
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
          return(NULL)
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
      return(fit)
    } else {
      return(NULL)
    }
  }

  if (future) {
    chains <- args$chains
    args$chains <- 1
    args$cores <- 1
    fits <- future.apply::future_lapply(1:chains,
      fit_chain,
      stan_args = args,
      max_time = max_execution_time,
      catch = TRUE,
      future.seed = TRUE
    )
    if (stuck_chains > 0) {
      fits[1:stuck_chains] <- NULL
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
#' @return A stan model object
#' @keywords internal
fit_model_approximate <- function(args, future = FALSE, id = "stan") {
  method <- args$method
  args$method <- NULL
  futile.logger::flog.debug(
    paste0(
      "%s: Running in approximate mode for ", args$iter,
      " iterations (with ", args$trials, " attempts). Extracting ",
      args$output_samples, " approximate posterior samples for ",
      args$data$t, " time steps of which ",
      args$data$horizon, " are a forecast"
    ),
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
      return(NULL)
    } else {
      return(fit)
    }
    return(fit)
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
