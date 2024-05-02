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
#' @param data A `<data.frame>` of confirmed cases (confirm) by date
#' (date). `confirm` must be numeric and `date` must be in date format.
#'
#' @param reported_cases Deprecated; use `data` instead.
#'
#' @param generation_time A call to [generation_time_opts()] defining the
#' generation time distribution used. For backwards compatibility a list of
#' summary parameters can also be passed.
#'
#' @param delays A call to [delay_opts()] defining delay distributions and
#' options. See the documentation of [delay_opts()] and the examples below for
#' details.
#'
#' @param truncation A call to [trunc_opts()] defining the truncation of
#' the observed data. Defaults to [trunc_opts()], i.e. no truncation.  See the
#' [estimate_truncation()] help file for an approach to estimating this from
#' data where the `dist` list element returned by [estimate_truncation()] is
#' used as the `truncation` argument here, thereby propagating the uncertainty
#' in the estimate.
#'
#' @param horizon Numeric, defaults to 7. Number of days into the future to
#' forecast.
#'
#' @param weigh_delay_priors Logical. If TRUE (default), all delay distribution
#' priors will be weighted by the number of observation data points, in doing so
#' approximately placing an independent prior at each time step and usually
#' preventing the posteriors from shifting. If FALSE, no weight will be applied,
#' i.e. delay distributions will be treated as a single parameters.
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
#' @seealso [epinow()] [regional_epinow()] [forecast_infections()]
#' [estimate_truncation()]
#' @inheritParams create_stan_args
#' @inheritParams create_stan_data
#' @inheritParams create_stan_data
#' @inheritParams create_gp_data
#' @inheritParams fit_model_with_nuts
#' @inheritParams create_clean_reported_cases
#' @inheritParams calc_CrIs
#' @importFrom data.table data.table copy merge.data.table as.data.table
#' @importFrom data.table setorder rbindlist melt .N setDT
#' @importFrom lubridate days
#' @importFrom futile.logger flog.threshold flog.warn flog.debug
#' @importFrom checkmate assert_class assert_numeric assert_logical
#' assert_string
#' @examples
#' \donttest{
#' # set number of cores to use
#' old_opts <- options()
#' options(mc.cores = ifelse(interactive(), 4, 1))
#'
#' # get example case counts
#' reported_cases <- example_confirmed[1:60]
#'
#' # set an example generation time. In practice this should use an estimate
#' # from the literature or be estimated from data
#' generation_time <- Gamma(
#'   shape = Normal(1.3, 0.3),
#'   rate = Normal(0.37, 0.09),
#'   max = 14
#' )
#' # set an example incubation period. In practice this should use an estimate
#' # from the literature or be estimated from data
#' incubation_period <- LogNormal(
#'    meanlog = Normal(1.6, 0.06),
#'    sdlog = Normal(0.4, 0.07),
#'    max = 14
#' )
#' # set an example reporting delay. In practice this should use an estimate
#' # from the literature or be estimated from data
#' reporting_delay <- LogNormal(mean = 2, sd = 1, max = 10)
#'
#' # for more examples, see the "estimate_infections examples" vignette
#' def <- estimate_infections(reported_cases,
#'   generation_time = generation_time_opts(generation_time),
#'   delays = delay_opts(incubation_period + reporting_delay),
#'   rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
#'   stan = stan_opts(control = list(adapt_delta = 0.95))
#' )
#' # real time estimates
#' summary(def)
#' # summary plot
#' plot(def)
#' options(old_opts)
#' }
estimate_infections <- function(data,
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
                                weigh_delay_priors = TRUE,
                                id = "estimate_infections",
                                verbose = interactive(),
                                reported_cases) {
  # Deprecate reported_cases in favour of data
  if (!missing(reported_cases)) {
     if (!missing(data)) {
      stop("Can't have `reported_cases` and `data` arguments. ",
           "Use `data` instead."
      )
     }
    lifecycle::deprecate_warn(
      "1.5.0",
      "estimate_infections(reported_cases)",
      "estimate_infections(data)",
      "The argument will be removed completely in the next version."
    )
    data <- reported_cases
  }
  # Validate inputs
  check_reports_valid(data, model = "estimate_infections")
  assert_class(generation_time, "generation_time_opts")
  assert_class(delays, "delay_opts")
  assert_class(truncation, "trunc_opts")
  assert_class(rt, "rt_opts", null.ok = TRUE)
  assert_class(backcalc, "backcalc_opts")
  assert_class(gp, "gp_opts", null.ok = TRUE)
  assert_class(obs, "obs_opts")
  assert_class(stan, "stan_opts")
  assert_numeric(horizon, lower = 0)
  assert_numeric(CrIs, lower = 0, upper = 1)
  assert_logical(filter_leading_zeros)
  assert_numeric(zero_threshold, lower = 0)
  assert_logical(weigh_delay_priors)
  assert_string(id)
  assert_logical(verbose)

  set_dt_single_thread()

  # store dirty reported case data
  dirty_reported_cases <- data.table::copy(data)

  if (!is.null(rt) && !rt$use_rt) {
    rt <- NULL
  }

  # Check verbose settings and set logger to match
  if (verbose) {
    futile.logger::flog.threshold(futile.logger::DEBUG,
      name = "EpiNow2.epinow.estimate_infections"
    )
  }
  # Order cases
  reported_cases <- create_clean_reported_cases(
    data, horizon,
    filter_leading_zeros = filter_leading_zeros,
    zero_threshold = zero_threshold
  )

  # Record earliest date with data
  start_date <- min(reported_cases$date, na.rm = TRUE)

  seeding_time <- get_seeding_time(delays, generation_time, rt)

  # Create mean shifted reported cases as prior
  reported_cases <- data.table::rbindlist(list(
    data.table::data.table(
      date = seq(
        min(reported_cases$date) - seeding_time - backcalc$prior_window,
        min(reported_cases$date) - 1,
        by = "days"
      ),
      confirm = 0, breakpoint = 0
    ),
    reported_cases
  ))

  shifted_cases <- create_shifted_cases(
    reported_cases,
    seeding_time,
    backcalc$prior_window,
    horizon
  )
  reported_cases <- reported_cases[-(1:backcalc$prior_window)]

  # Define stan model parameters
  stan_data <- create_stan_data(
    reported_cases,
    seeding_time = seeding_time,
    rt = rt,
    gp = gp,
    obs = obs,
    backcalc = backcalc,
    shifted_cases = shifted_cases$confirm,
    horizon = horizon
  )

  stan_data <- c(stan_data, create_stan_delays(
    gt = generation_time,
    delay = delays,
    trunc = truncation,
    time_points = stan_data$t - stan_data$seeding_time - stan_data$horizon
  ))

  # Set up default settings
  args <- create_stan_args(
    stan = stan,
    data = stan_data,
    init = create_initial_conditions(stan_data),
    verbose = verbose
  )

  # Initialise fitting by using a previous fit or fitting to cumulative cases
  if (!is.null(args$init_fit)) {
    if (!inherits(args$init_fit, "stanfit") &&
          args$init_fit == "cumulative") {
      args$init_fit <- init_cumulative_fit(args,
        warmup = 50, samples = 50,
        id = id, verbose = FALSE, stan$backend
      )
    }
    args$init <- extract_inits(args$init_fit,
      current_inits = args$init,
      exclude_list = c("initial_infections", "initial_growth"),
      samples = 50
    )
    args$init_fit <- NULL
  }
  # Fit model
  fit <- fit_model(args, id = id)

  # Extract parameters of interest from the fit
  out <- extract_parameter_samples(fit, stan_data,
    reported_inf_dates = reported_cases$date,
    reported_dates = reported_cases$date[-(1:stan_data$seeding_time)]
  )

  ## Add prior infections
  if (length(delays) > 0) {
    out$prior_infections <- shifted_cases[
      ,
      .(
        parameter = "prior_infections", time = seq_len(.N),
        date, value = confirm, sample = 1
      )
    ]
  }
  # Format output
  format_out <- format_fit(
    posterior_samples = out,
    horizon = horizon,
    shift = stan_data$seeding_time,
    burn_in = 0,
    start_date = start_date,
    CrIs = CrIs
  )

  ## Join stan fit if required
  if (stan$return_fit) {
    format_out$fit <- fit
    format_out$args <- stan_data
  }
  format_out$observations <- dirty_reported_cases
  class(format_out) <- c("estimate_infections", class(format_out))
  return(format_out)
}

#' Format Posterior Samples
#'
#' @description `r lifecycle::badge("stable")`
#' Summaries posterior samples and adds additional custom variables.
#'
#' @param posterior_samples A list of posterior samples as returned by
#' [extract_parameter_samples()].
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
#' @importFrom data.table fcase rbindlist
#' @importFrom lubridate days
#' @importFrom futile.logger flog.info
#' @return A list of samples and summarised posterior parameter estimates.
#' @keywords internal
format_fit <- function(posterior_samples, horizon, shift, burn_in, start_date,
                       CrIs) {
  format_out <- list()
  # bind all samples together
  format_out$samples <- data.table::rbindlist(
    posterior_samples, fill = TRUE, idcol = "variable"
  )

  if (is.null(format_out$samples$strat)) {
    format_out$samples <- format_out$samples[, strat := NA]
  }
  # add type based on horizon
  format_out$samples <- format_out$samples[
    ,
    type := data.table::fcase(
      date > (max(date, na.rm = TRUE) - horizon),
      "forecast",
      date > (max(date, na.rm = TRUE) - horizon - shift),
      "estimate based on partial data",
      is.na(date), NA_character_,
      default = "estimate"
      )
  ]

  # remove burn in period if specified
  if (burn_in > 0) {
    futile.logger::flog.info(
      "burn_in is depreciated as of EpiNow2 1.3.0 - if using this feature",
      " please contact the developers",
      name = "EpiNow2.epinow.estimate_infections"
    )
    format_out$samples <-
      format_out$samples[is.na(date) ||
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
