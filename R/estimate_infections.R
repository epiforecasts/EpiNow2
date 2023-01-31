#' Estimate Infections, the Time-Varying Reproduction Number and the Rate of
#' Growth
#'
#' @description `r lifecycle::badge("maturing")`
#' Uses a non-parametric approach to reconstruct cases by date of infection
#' from reported cases. It uses either a generative Rt model or non-parametric
#' back calculation to estimate underlying latent infections and then maps
#' these infections to observed cases via uncertain reporting delays and a
#' flexible observation model. See the examples and function arguments for the
#' details of all options. The default settings may not be sufficient for your
#' use case so the number of warmup samples (`stan_args = list(warmup)`) may
#' need to be increased as may the overall number of samples. Follow the links
#' provided by any warnings messages to diagnose issues with the MCMC fit. It
#' is recommended to explore several of the Rt estimation approaches supported
#' as not all of them may be suited to users own use cases. See
#' [here](https://gist.github.com/seabbs/163d0f195892cde685c70473e1f5e867)
#' for an example of using `estimate_infections` within the `epinow` wrapper to
#' estimate Rt for Covid-19 in a country from the ECDC data source.
#'
#' @param reported_cases A data frame of confirmed cases (confirm) by date
#' (date). confirm must be integer and date must be in date format.
#'
#' @param generation_time A call to `generation_time_opts()` defining the
#' generation time distribution used. For backwards compatibility a list of
#' summary parameters can also be passed.
#'
#' @param delays A call to `delay_opts()` defining delay distributions and
#' options. See the documentation of `delay_opts()` and the examples below for
#' details.
#'
#' @param horizon Numeric, defaults to 7. Number of days into the future to
#' forecast.
#'
#' @param verbose Logical, defaults to `TRUE` when used interactively and
#' otherwise `FALSE`. Should verbose debug progress messages be printed.
#' Corresponds to the "DEBUG" level from `futile.logger`. See `setup_logging`
#' for more detailed logging options.
#'
#' @export
#' @return A list of output including: posterior samples, summarised posterior
#' samples, data used to fit the model, and the fit object itself.
#'
#' @author Sam Abbott
#' @seealso epinow regional_epinow forecast_infections simulate_infections
#' @inheritParams create_stan_args
#' @inheritParams create_stan_data
#' @inheritParams create_gp_data
#' @inheritParams fit_model_with_nuts
#' @inheritParams create_clean_reported_cases
#' @inheritParams calc_CrIs
#' @importFrom data.table data.table copy merge.data.table as.data.table setorder rbindlist melt .N setDT
#' @importFrom purrr transpose
#' @importFrom lubridate days
#' @importFrom purrr transpose
#' @importFrom futile.logger flog.threshold flog.warn flog.debug
#' @examples
#' \donttest{
#' # set number of cores to use
#' old_opts <- options()
#' options(mc.cores = ifelse(interactive(), 4, 1))
#'
#' # get example case counts
#' reported_cases <- example_confirmed[1:60]
#'
#' # set up example generation time
#' generation_time <- generation_time_opts(
#'   disease = "SARS-CoV-2", source = "ganyani", fixed = TRUE
#' )
#' # set delays between infection and case report
#' incubation_period <- get_incubation_period(
#'   disease = "SARS-CoV-2", source = "lauer"
#' )
#' reporting_delay <- list(
#'   mean = convert_to_logmean(2, 1), mean_sd = 0,
#'   sd = convert_to_logsd(2, 1), sd_sd = 0, max = 10
#' )
#'
#' # default settings but assuming that delays are fixed rather than uncertain
#' def <- estimate_infections(reported_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay, fixed = TRUE),
#'   rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
#'   stan = stan_opts(control = list(adapt_delta = 0.95))
#' )
#' # real time estimates
#' summary(def)
#' # summary plot
#' plot(def)
#'
#' # decreasing the accuracy of the approximate Gaussian to speed up computation.
#' # These settings are an area of active research. See ?gp_opts for details.
#' agp <- estimate_infections(reported_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay, fixed = TRUE),
#'   rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
#'   gp = gp_opts(ls_min = 10, basis_prop = 0.1),
#'   stan = stan_opts(control = list(adapt_delta = 0.95))
#' )
#' summary(agp)
#' plot(agp)
#'
#' # Adjusting for future susceptible depletion
#' dep <- estimate_infections(reported_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay, fixed = TRUE),
#'   rt = rt_opts(
#'     prior = list(mean = 2, sd = 0.1),
#'     pop = 1000000, future = "latest"
#'   ),
#'   gp = gp_opts(ls_min = 10, basis_prop = 0.1), horizon = 21,
#'   stan = stan_opts(control = list(adapt_delta = 0.95))
#' )
#' plot(dep)
#'
#' # Adjusting for truncation of the most recent data
#' # See estimate_truncation for an approach to estimating this from data
#' trunc_dist <- trunc_opts(dist = list(
#'   mean = convert_to_logmean(0.5, 0.5), mean_sd = 0.1,
#'   sd = convert_to_logsd(0.5, 0.5), sd_sd = 0.1,
#'   max = 3
#' ))
#' trunc <- estimate_infections(reported_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay, fixed = TRUE),
#'   truncation = trunc_dist,
#'   rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
#'   gp = gp_opts(ls_min = 10, basis_prop = 0.1),
#'   stan = stan_opts(control = list(adapt_delta = 0.95))
#' )
#' plot(trunc)
#'
#' # using back calculation (combined here with under reporting)
#' # this model is in the order of 10 ~ 100 faster than the gaussian process
#' # method
#' # it is likely robust for retrospective Rt but less reliable for real time
#' # estimates
#' # the width of the prior window controls the reliance on observed data and
#' # can be optionally switched off using backcalc_opts(prior = "none"),
#' # see ?backcalc_opts for other options
#' backcalc <- estimate_infections(reported_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay, fixed = TRUE),
#'   rt = NULL, backcalc = backcalc_opts(),
#'   obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
#'   horizon = 0
#' )
#' plot(backcalc)
#'
#' # Rt projected into the future using the Gaussian process
#' project_rt <- estimate_infections(reported_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay, fixed = TRUE),
#'   rt = rt_opts(
#'     prior = list(mean = 2, sd = 0.1),
#'     future = "project"
#'   )
#' )
#' plot(project_rt)
#'
#' # default settings on a later snapshot of data
#' snapshot_cases <- example_confirmed[80:130]
#' snapshot <- estimate_infections(snapshot_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay, fixed = TRUE),
#'   rt = rt_opts(prior = list(mean = 1, sd = 0.1))
#' )
#' plot(snapshot)
#'
#' # stationary Rt assumption (likely to provide biased real-time estimates)
#' # with uncertain reporting delays
#' stat <- estimate_infections(reported_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay),
#'   rt = rt_opts(prior = list(mean = 2, sd = 0.1), gp_on = "R0")
#' )
#' plot(stat)
#'
#' # no gaussian process (i.e fixed Rt assuming no breakpoints)
#' # with uncertain reporting delays
#' fixed <- estimate_infections(reported_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay),
#'   gp = NULL
#' )
#' plot(fixed)
#'
#' # no delays
#' no_delay <- estimate_infections(reported_cases, generation_time = generation_time)
#' plot(no_delay)
#'
#' # break point but otherwise static Rt
#' # with uncertain reporting delays
#' bp_cases <- data.table::copy(reported_cases)
#' bp_cases <- bp_cases[, breakpoint := ifelse(date == as.Date("2020-03-16"), 1, 0)]
#' bkp <- estimate_infections(bp_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay),
#'   rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
#'   gp = NULL
#' )
#' # break point effect
#' summary(bkp, type = "parameters", params = "breakpoints")
#' plot(bkp)
#'
#' # weekly random walk
#' # with uncertain reporting delays
#' rw <- estimate_infections(reported_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay),
#'   rt = rt_opts(prior = list(mean = 2, sd = 0.1), rw = 7),
#'   gp = NULL
#' )
#'
#' # random walk effects
#' summary(rw, type = "parameters", params = "breakpoints")
#' plot(rw)
#'
#' options(old_opts)
#' }
estimate_infections <- function(reported_cases,
                                generation_time = generation_time_opts(),
                                delays = delay_opts(),
                                truncation = trunc_opts(),
                                rt = rt_opts(),
                                backcalc = backcalc_opts(),
                                gp = gp_opts(),
                                obs = obs_opts(),
                                stan = stan_opts(),
                                horizon = 7,
                                CrIs = c(0.2, 0.5, 0.9),
                                filter_leading_zeros = TRUE,
                                zero_threshold = Inf,
                                id = "estimate_infections",
                                verbose = interactive()) {
  set_dt_single_thread()

  # store dirty reported case data
  dirty_reported_cases <- data.table::copy(reported_cases)

  if (!is.null(rt) && !rt$use_rt) {
    rt <- NULL
  }

  # Check verbose settings and set logger to match
  if (verbose) {
    futile.logger::flog.threshold(futile.logger::DEBUG,
      name = "EpiNow2.epinow.estimate_infections"
    )
  }
  if (is.null(delays$delays)) {
    stop("A call to delay_opts must be passed to delays")
  }
  # Make sure there are no missing dates and order cases
  reported_cases <- create_clean_reported_cases(
    reported_cases, horizon,
    filter_leading_zeros = filter_leading_zeros,
    zero_threshold = zero_threshold
  )

  # Record earliest date with data
  start_date <- min(reported_cases$date, na.rm = TRUE)

  # Create mean shifted reported cases as prior
  reported_cases <- data.table::rbindlist(list(
    data.table::data.table(
      date = seq(min(reported_cases$date) - delays$seeding_time - backcalc$prior_window,
        min(reported_cases$date) - 1,
        by = "days"
      ),
      confirm = 0, breakpoint = 0
    ),
    reported_cases
  ))

  shifted_cases <- create_shifted_cases(
    reported_cases,
    delays$seeding_time,
    backcalc$prior_window,
    horizon
  )
  reported_cases <- reported_cases[-(1:backcalc$prior_window)]

  # Define stan model parameters
  data <- create_stan_data(
    reported_cases = reported_cases,
    generation_time = generation_time,
    delays = delays,
    truncation = truncation,
    rt = rt,
    gp = gp,
    obs = obs,
    backcalc = backcalc,
    shifted_cases = shifted_cases$confirm,
    horizon = horizon
  )

  # Set up default settings
  args <- create_stan_args(
    stan = stan,
    data = data,
    init = create_initial_conditions(data),
    verbose = verbose
  )

  # Initialise fitting by using a previous fit or fitting to cumulative cases
  if (!is.null(args$init_fit)) {
    if (!("stanfit" %in% class(args$init_fit))) {
      if (args$init_fit %in% "cumulative") {
        args$init_fit <- init_cumulative_fit(args,
          warmup = 50, samples = 50,
          id = id, verbose = FALSE
        )
      }
    }
    args$init <- extract_inits(args$init_fit,
      current_inits = args$init,
      exclude_list = c("initial_infections", "initial_growth"),
      samples = 50
    )
    args$init_fit <- NULL
  }
  # Fit model
  if (args$method == "sampling") {
    fit <- fit_model_with_nuts(args,
      future = args$future,
      max_execution_time = args$max_execution_time, id = id
    )
  } else if (args$method == "vb") {
    fit <- fit_model_with_vb(args, id = id)
  }
  # Extract parameters of interest from the fit
  out <- extract_parameter_samples(fit, data,
    reported_inf_dates = reported_cases$date,
    reported_dates = reported_cases$date[-(1:data$seeding_time)]
  )

  ## Add prior infections
  if (delays$delays > 0) {
    out$prior_infections <- shifted_cases[
      ,
      .(
        parameter = "prior_infections", time = 1:.N,
        date, value = confirm, sample = 1
      )
    ]
  }
  # Format output
  format_out <- format_fit(
    posterior_samples = out,
    horizon = horizon,
    shift = data$seeding_time,
    burn_in = 0,
    start_date = start_date,
    CrIs = CrIs
  )

  ## Join stan fit if required
  if (stan$return_fit) {
    format_out$fit <- fit
    format_out$args <- data
  }
  format_out$observations <- dirty_reported_cases
  class(format_out) <- c("estimate_infections", class(format_out))
  return(format_out)
}


#' Generate initial conditions by fitting to cumulative cases
#'
#' @description `r lifecycle::badge("experimental")`
#' Fits a model to cumulative cases. This may be a useful approach to
#' initialising a full model fit for certain data sets where the sampler gets
#' stuck or cannot easily be initialised as fitting to cumulative cases changes
#' the shape of the posterior distribution. In `estimate_infections()`,
#' `epinow()` and `regional_epinow()` this option can be engaged by setting
#' `stan_opts(init_fit = "cumulative")`.
#'
#' This implementation is based on the approach taken in
#' [epidemia](https://github.com/ImperialCollegeLondon/epidemia/) authored by
#' James Scott.
#'
#' @param samples Numeric, defaults to 50. Number of posterior samples.
#'
#' @param warmup Numeric, defaults to 50. Number of warmup samples.
#'
#' @param verbose Logical, should fitting progress be returned. Defaults to
#' `FALSE`.
#'
#' @inheritParams create_initial_conditions
#' @importFrom rstan sampling
#' @importFrom futile.logger flog.debug
#' @importFrom utils capture.output
#' @inheritParams fit_model_with_nuts
#' @return A stanfit object
#' @author Sam Abbott
init_cumulative_fit <- function(args, samples = 50, warmup = 50,
                                id = "init", verbose = FALSE) {
  futile.logger::flog.debug("%s: Fitting to cumulative data to initialise chains", id,
    name = "EpiNow2.epinow.estimate_infections.fit"
  )
  # copy main run settings and override to use only 100 iterations and a single chain
  initial_args <- list(
    object = args$object,
    data = args$data,
    init = args$init,
    iter = samples + warmup,
    warmup = warmup,
    chains = 1,
    cores = 2,
    open_progress = FALSE,
    show_messages = FALSE,
    control = list(adapt_delta = 0.9, max_treedepth = 13),
    refresh = ifelse(verbose, 50, -1)
  )
  # change observations to be cumulative in order to protect against noise and give
  # an approximate fit (though for Rt constrained to be > 1)
  initial_args$data$cases <- cumsum(initial_args$data$cases)
  initial_args$data$shifted_cases <- cumsum(initial_args$data$shifted_cases)

  # initial fit
  if (verbose) {
    fit <- do.call(rstan::sampling, initial_args)
  } else {
    out <- tempfile(tmpdir = tempdir(check = TRUE))
    capture.output(
      {
        fit <- do.call(rstan::sampling, initial_args)
      },
      type = c("output", "message"),
      split = FALSE,
      file = out
    )
  }
  return(fit)
}

#' Fit a Stan Model using the NUTs sampler
#'
#' @description `r lifecycle::badge("maturing")`
#' Fits a stan model using `rstan::sampling`. Provides the optional ability to
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
#' Used by `regional_epinow` to assign errors to regions. Alter the default to
#' run with error catching.
#'
#' @importFrom futile.logger flog.debug flog.info flog.error
#' @importFrom R.utils withTimeout
#' @importFrom future.apply future_lapply
#' @importFrom purrr compact
#' @importFrom rstan sflist2stanfit sampling
#' @importFrom rlang abort cnd_muffle
#' @return A stan model object
#' @author Sam Abbott
fit_model_with_nuts <- function(args, future = FALSE, max_execution_time = Inf, id = "stan") {
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
    if (catch) {
      fit <- tryCatch(
        withCallingHandlers(
          R.utils::withTimeout(do.call(rstan::sampling, stan_args),
            timeout = max_time,
            onTimeout = "silent"
          ),
          warning = function(w) {
            futile.logger::flog.warn("%s (chain: %s): %s - %s", id, chain, w$message, toString(w$call),
              name = "EpiNow2.epinow.estimate_infections.fit"
            )
            rlang::cnd_muffle(w)
          }
        ),
        error = function(e) {
          error_text <- sprintf("%s (chain: %s): %s - %s", id, chain, e$message, toString(e$call))
          futile.logger::flog.error(error_text,
            name = "EpiNow2.epinow.estimate_infections.fit"
          )
          return(NULL)
        }
      )
    } else {
      fit <- R.utils::withTimeout(do.call(rstan::sampling, stan_args),
        timeout = max_time,
        onTimeout = "silent"
      )
    }

    if (is.null(fit) || length(names(fit)) == 0) {
      return(NULL)
    } else {
      return(fit)
    }
  }

  if (!future) {
    fit <- fit_chain(1,
      stan_args = args, max_time = max_execution_time,
      catch = !id %in% c("estimate_infections", "epinow")
    )
    if (stuck_chains > 0) {
      fit <- NULL
    }
    if (is.null(fit)) {
      rlang::abort("model fitting was timed out or failed")
    }
  } else {
    chains <- args$chains
    args$chains <- 1
    args$cores <- 1
    fits <- future.apply::future_lapply(1:chains, fit_chain,
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
        rlang::abort("all chains failed - try inspecting the output for errors or increasing the max_execution_time")
      }
    } else {
      failed_chains <- chains - length(fit)
      if (failed_chains > 0) {
        futile.logger::flog.warn("%s: %s chains failed or were timed out.", id, failed_chains,
          name = "EpiNow2.epinow.estimate_infections.fit"
        )
        if ((chains - failed_chains) < 2) {
          rlang::abort(
            "model fitting failed as too few chains were returned to assess convergence (2 or more required)"
          )
        }
      }
      fit <- rstan::sflist2stanfit(fit)
    }
  }
  return(fit)
}

#' Fit a Stan Model using Variational Inference
#'
#' @description `r lifecycle::badge("maturing")`
#' Fits a stan model using variational inference.
#'
#' @inheritParams fit_model_with_nuts
#' @importFrom futile.logger flog.debug flog.info flog.error
#' @importFrom purrr safely
#' @importFrom rstan vb
#' @importFrom rlang abort
#' @return A stan model object
#' @author Sam Abbott
fit_model_with_vb <- function(args, future = FALSE, id = "stan") {
  args$method <- NULL
  futile.logger::flog.debug(
    paste0(
      "%s: Running in approximate mode for ", args$iter, " iterations (with ", args$trials, " attempts). Extracting ",
      args$output_samples, " approximate posterior samples for ", args$data$t, " time steps of which ",
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

  fit_vb <- function(stan_args) {
    fit <- do.call(rstan::vb, stan_args)

    if (length(names(fit)) == 0) {
      return(NULL)
    } else {
      return(fit)
    }
    return(fit)
  }
  safe_vb <- purrr::safely(fit_vb)
  fit <- NULL
  current_trials <- 0

  while (current_trials <= trials & is.null(fit)) {
    fit <- safe_vb(args)

    error <- fit[[2]]
    fit <- fit[[1]]
    current_trials <- current_trials + 1
  }

  if (is.null(fit)) {
    if (is.null(fit)) {
      futile.logger::flog.error("%s: Fitting failed - try increasing stan_args$trials or inspecting the model input",
        id,
        name = "EpiNow2.epinow.estimate_infections.fit"
      )
      rlang::abort("Variational Inference failed due to: ", error)
    }
  }
  return(fit)
}


#' Format Posterior Samples
#'
#' @description `r lifecycle::badge("stable")`
#' Summaries posterior samples and adds additional custom variables.
#'
#' @param posterior_samples A list of posterior samples as returned by
#' `extract_parameter_samples`.
#'
#' @param horizon Numeric, forecast horizon.
#'
#' @param shift Numeric, the shift to apply to estimates.
#'
#' @param burn_in Numeric, number of days to discard estimates for.
#'
#' @param start_date Date, earliest date with data.
#'
#' @inheritParams calc_summary_measures
#' @importFrom data.table fifelse rbindlist
#' @importFrom lubridate days
#' @importFrom futile.logger flog.info
#' @return A list of samples and summarised posterior parameter estimates.
#' @author Sam Abbott
format_fit <- function(posterior_samples, horizon, shift, burn_in, start_date,
                       CrIs) {
  format_out <- list()
  # bind all samples together
  format_out$samples <- data.table::rbindlist(posterior_samples, fill = TRUE, idcol = "variable")

  if (is.null(format_out$samples$strat)) {
    format_out$samples <- format_out$samples[, strat := NA]
  }
  # add type based on horizon
  format_out$samples <- format_out$samples[
    ,
    type := data.table::fifelse(
      date > (max(date, na.rm = TRUE) - horizon),
      "forecast",
      data.table::fifelse(
        date > (max(date, na.rm = TRUE) - horizon - shift),
        "estimate based on partial data",
        "estimate"
      )
    )
  ]

  # remove burn in period if specified
  if (burn_in > 0) {
    futile.logger::flog.info("burn_in is depreciated as of EpiNow2 1.3.0 - if using
                             this feature please contact the developers",
      name = "EpiNow2.epinow.estimate_infections"
    )
    format_out$samples <- 
      format_out$samples[is.na(date) | 
        date >= (start_date + lubridate::days(burn_in))]
  }

  # summarise samples
  format_out$summarised <- calc_summary_measures(format_out$samples,
    summarise_by = c("date", "variable", "strat", "type"),
    order_by = c("variable", "date"),
    CrIs = CrIs
  )
  return(format_out)
}
