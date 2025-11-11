#' Simulate infections using the renewal equation
#'
#' Simulations are done from given initial infections and, potentially
#' time-varying, reproduction numbers. Delays and parameters of the observation
#' model can be specified using the same options as in [estimate_infections()].
#'
#' In order to simulate, all parameters that are specified such as the mean and
#' standard deviation of delays or observation scaling, must be fixed.
#' Uncertain parameters are not allowed.
#'
#' @param R a data frame of reproduction numbers (column `R`) by date (column
#'   `date`). Column `R` must be numeric and `date` must be in date format. If
#'   not all days between the first and last day in the `date` are present,
#'   it will be assumed that R stays the same until the next given date.
#' @param initial_infections numeric; the initial number of infections (i.e.
#'   before `R` applies). Note that results returned start the day after, i.e.
#'   the initial number of infections is not reported again. See also
#'   `seeding_time`
#' @param day_of_week_effect either `NULL` (no day of the week effect) or a
#'   numerical vector of length specified in [obs_opts()] as `week_length`
#'   (default: 7) if `week_effect` is set to TRUE. Each element of the vector
#'   gives the weight given to reporting on this day (normalised to 1).
#'   The default is `NULL`.
#' @param seeding_time Integer; the number of days before the first time point
#'   of `R`; default is `NULL`, in which case it is set to the maximum of the
#'   generation time. The minimum is 1 , i.e. the first reproduction number
#'   given applies on the day after the index cases given by
#'   `initial_infections`. If the generation time is longer than 1 day on
#'   average, a seeding time of 1 will always lead to an initial decline (as
#'   there are no infections before the initial ones). Instead, if this is
#'   greater than 1, an initial part of the epidemic (before the first value of
#'   R given) of `seeding_time` days is assumed to have followed exponential
#'   growth roughly in line with the growth rate implied by the first value of
#'   R.
#' @inheritParams estimate_infections
#' @inheritParams rt_opts
#' @inheritParams stan_opts
#' @importFrom checkmate assert_data_frame assert_date assert_numeric
#'   assert_subset assert_integer
#' @importFrom data.table data.table merge.data.table nafill rbindlist
#' @importFrom cli cli_abort
#' @return A data.table of simulated infections (variable `infections`) and
#'   reported cases (variable `reported_cases`) by date.
#' @export
#' @examples
#' \donttest{
#' R <- data.frame(
#'   date = seq.Date(as.Date("2023-01-01"), length.out = 14, by = "day"),
#'   R = c(rep(1.2, 7), rep(0.8, 7))
#' )
#' sim <- simulate_infections(
#'   R = R,
#'   initial_infections = 100,
#'   generation_time = generation_time_opts(
#'     fix_parameters(example_generation_time)
#'   ),
#'   delays = delay_opts(fix_parameters(example_reporting_delay)),
#'   obs = obs_opts(family = "poisson")
#' )
#' }
simulate_infections <- function(R,
                                initial_infections,
                                day_of_week_effect = NULL,
                                generation_time = generation_time_opts(),
                                delays = delay_opts(),
                                truncation = trunc_opts(),
                                obs = obs_opts(),
                                CrIs = c(0.2, 0.5, 0.9),
                                backend = "rstan",
                                seeding_time = NULL,
                                pop = Fixed(0),
                                pop_period = c("forecast", "all"),
                                pop_floor = 1.0,
                                growth_method = c("infections",
                                                  "infectiousness")) {
  if (is.numeric(pop)) {
    lifecycle::deprecate_warn(
      "1.7.0",
      "simulate_infections(pop = 'must be a `<dist_spec>`')",
      details = "For specifying a fixed population size, use `Fixed(pop)`"
    )
    pop <- Fixed(pop)
  }
  pop_period <- arg_match(pop_period)
  if (pop_period == "all" && pop == Fixed(0)) {
    cli_abort(
      c(
        "!" = "pop_period = \"all\" but pop is fixed at 0."
      )
    )
  }

  ## check inputs
  assert_data_frame(R, any.missing = FALSE)
  assert_subset(c("date", "R"), colnames(R))
  assert_date(R$date)
  assert_numeric(R$R, lower = 0)
  assert_numeric(initial_infections, lower = 0)
  assert_numeric(day_of_week_effect, lower = 0, null.ok = TRUE)
  if (!is.null(seeding_time)) {
    assert_integerish(seeding_time, lower = 1)
  }
  assert_class(delays, "delay_opts")
  assert_class(truncation, "trunc_opts")
  assert_class(obs, "obs_opts")
  assert_class(generation_time, "generation_time_opts")
  assert_class(pop, "dist_spec")
  assert_number(pop_floor, lower = 0, finite = TRUE)
  growth_method <- arg_match(growth_method)

  ## create R for all dates modelled
  all_dates <- data.table(date = seq.Date(min(R$date), max(R$date), by = "day"))
  R <- merge.data.table(all_dates, R, by = "date", all.x = TRUE)
  R <- R[, R := nafill(R, type = "locf")]
  ## remove any initial NAs
  R <- R[!is.na(R)]

  if (missing(seeding_time)) {
    seeding_time <- sum(max(generation_time))
  }

  stan_data <- list(
    n = 1,
    t = nrow(R) + seeding_time,
    seeding_time = seeding_time,
    future_time = 0,
    initial_infections = array(log(initial_infections), dim = c(1, 1)),
    initial_as_scale = 0,
    R = array(R$R, dim = c(1, nrow(R))),
    use_pop = as.integer(pop != Fixed(0)) + as.integer(pop_period == "all"),
    pop_floor = pop_floor,
    growth_method = list(
      "infections" = 0, "infectiousness" = 1
    )[[growth_method]]
  )

  stan_data <- c(stan_data, create_stan_delays(
    gt = generation_time,
    delay = delays,
    trunc = truncation
  ))

  if (length(stan_data$delay_params_sd) > 0 &&
        any(stan_data$delay_params_sd > 0)) {
    cli_abort(
      c(
        "!" = "Cannot simulate from uncertain parameters.",
        "i" = "Use {.fn fix_parameters} to set the parameters of uncertain
        distributions using either the mean or a randomly sampled value."
      )
    )
  }
  stan_data$delay_params <- array(
    stan_data$delay_params_mean,
    dim = c(1, length(stan_data$delay_params_mean))
  )
  stan_data$delay_params_sd <- NULL

  stan_data <- c(stan_data, create_obs_model(
    obs,
    dates = R$date
  ))

  if (get_distribution(obs$scale) != "fixed") {
    cli_abort(
      c(
        "!" = "Cannot simulate from uncertain observation scaling.",
        "i" = "Use fixed scaling instead."
      )
    )
  }

  if (obs$family == "negbin") {
    if (get_distribution(obs$dispersion) != "fixed") {
      cli_abort(
        c(
          "!" = "Cannot simulate from uncertain dispersion.",
          "i" = "Use fixed dispersion instead."
        )
      )
    }
  } else {
    obs$dispersion <- NULL
  }

  params <- list(
    make_param("alpha", NULL),
    make_param("rho", NULL),
    make_param("R0", NULL),
    make_param("frac_obs", obs$scale, lower_bound = 0),
    make_param("dispersion", obs$dispersion, lower_bound = 0),
    make_param("pop", pop, lower_bound = 0)
  )

  stan_data <- c(stan_data, create_stan_params(params))

  ## set empty params matrix - variable parameters not supported here
  stan_data$params <- array(dim = c(1, 0))

  ## day of week effect
  if (is.null(day_of_week_effect)) {
    day_of_week_effect <- rep(1, stan_data$week_effect)
  }

  day_of_week_effect <- day_of_week_effect / sum(day_of_week_effect)
  stan_data$day_of_week_simplex <- array(
    day_of_week_effect,
    dim = c(1, stan_data$week_effect)
  )

  # Create stan arguments
  stan <- stan_opts(backend = backend, chains = 1, samples = 1, warmup = 1)
  stan_args <- create_stan_args(
    stan,
    data = stan_data, fixed_param = TRUE, model = "simulate_infections",
    verbose = FALSE
  )

  ## simulate
  sim <- fit_model(stan_args, id = "simulate_infections")

  ## join batches
  dates <- c(
    seq(min(R$date) - seeding_time, min(R$date) - 1, by = "day"),
    R$date
  )
  out <- format_simulation_output(sim, stan_data,
    reported_inf_dates = dates,
    reported_dates = dates[-(1:seeding_time)],
    imputed_dates = dates[-(1:seeding_time)],
    drop_length_1 = TRUE
  )

  out <- rbindlist(out[c("infections", "reported_cases")], idcol = "variable")
  out <- out[, c("sample", "parameter", "time") := NULL]

  return(out[])
}

#' Forecast infections from a given fit and trajectory of the time-varying
#' reproduction number
#'
#' @description `r lifecycle::badge("stable")`
#' This function simulates infections using an existing fit to observed cases
#' but with a modified time-varying reproduction number. This can be used to
#' explore forecast models or past counterfactuals. Simulations can be run in
#' parallel using [future::plan()].
#'
#' @param estimates The \code{estimates} element of an [epinow()] run that
#' has been done with output = "fit", or the result of
#' [estimate_infections()] with \code{return_fit} set to TRUE.
#'
#' @param model A compiled stan model as returned by [rstan::stan_model()].
#'
#' @param R A numeric vector of reproduction numbers; these will overwrite the
#' reproduction numbers contained in \code{estimates}, except elements set to
#' NA. Alternatively accepts a `<data.frame>` containing at least `date` and
#' `value` (integer) variables and optionally `sample`. More (or fewer) days
#' than in the original fit can be simulated.
#'
#' @param samples Numeric, number of posterior samples to simulate from. The
#' default is to use all samples in the `estimates` input.
#'
#' @param batch_size Numeric, defaults to 10. Size of batches in which to
#' simulate. May decrease run times due to reduced IO costs but this is still
#' being evaluated. If set to NULL then all simulations are done at once.
#'
#' @param verbose Logical defaults to [interactive()]. If the `progressr`
#' package is available, a progress bar will be shown.
#' @inheritParams stan_opts
#' @importFrom rstan extract sampling
#' @importFrom purrr list_transpose map safely compact
#' @importFrom data.table rbindlist as.data.table
#' @importFrom lubridate days
#' @importFrom checkmate assert_class assert_names test_numeric test_data_frame
#' assert_numeric assert_integerish assert_logical
#' @importFrom cli cli_abort
#' @return A `<forecast_infections>` object containing simulated infections and
#' cases from the specified scenario. The structure is similar to
#' [estimate_infections()] output but contains `samples` rather than `fit`.
#' @seealso [generation_time_opts()] [delay_opts()] [rt_opts()]
#' [estimate_infections()] [trunc_opts()] [stan_opts()] [obs_opts()]
#' [gp_opts()]
#' @export
#' @examples
#' \donttest{
#' # set number of cores to use
#' old_opts <- options()
#' options(mc.cores = ifelse(interactive(), 4, 1))
#'
#' # get example case counts
#' reported_cases <- example_confirmed[1:50]
#'
#' # fit model to data to recover Rt estimates
#' est <- estimate_infections(reported_cases,
#'   generation_time = generation_time_opts(example_generation_time),
#'   delays = delay_opts(example_incubation_period + example_reporting_delay),
#'   rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.1), rw = 7),
#'   obs = obs_opts(scale = Normal(mean = 0.1, sd = 0.01)),
#'   gp = NULL,
#'   forecast = forecast_opts(horizon = 0)
#' )
#'
#' # update Rt trajectory and simulate new infections using it
#' R <- c(rep(NA_real_, 26), rep(0.5, 10), rep(0.8, 14))
#' sims <- forecast_infections(est, R)
#' plot(sims)
#'
#' # with a data.frame input of samples
#' R_dt <- data.frame(
#'   date = seq(
#'     min(summary(est, type = "parameters", param = "R")$date),
#'     by = "day", length.out = length(R)
#'   ),
#'   value = R
#' )
#' sims <- forecast_infections(est, R_dt)
#' plot(sims)
#'
#' #' # with a data.frame input of samples
#' R_samples <- get_samples(est)[variable == "R"]
#' R_samples <- R_samples[
#'   ,
#'   .(date, sample, value)
#' ][sample <= 1000][date <= "2020-04-10"]
#' R_samples <- R_samples[date >= "2020-04-01", value := 1.1]
#' sims <- forecast_infections(est, R_samples)
#' plot(sims)
#'
#' options(old_opts)
#' }
forecast_infections <- function(estimates,
                                R = NULL,
                                model = NULL,
                                samples = NULL,
                                batch_size = 10,
                                backend = "rstan",
                                verbose = interactive()) {
  ## check inputs
  assert_class(estimates, "estimate_infections")
  assert_names(names(estimates), must.include = "fit")
  if (!(test_numeric(R, lower = 0, null.ok = TRUE) ||
          test_data_frame(R, null.ok = TRUE))) {
    cli_abort(
      c(
        "!" = "R must either be a {.cls numeric} vector or
        a {.cls data.frame}."
      )
    )
  }
  if (test_data_frame(R)) {
    assert_names(names(R), must.include = c("date", "value"))
    assert_numeric(R$value, lower = 0)
  }
  assert_class(model, "stanfit", null.ok = TRUE)
  assert_integerish(samples, lower = 1, null.ok = TRUE)
  assert_integerish(batch_size, lower = 2)
  assert_logical(verbose)
  ## extract samples from given stanfit object
  draws <- extract(estimates$fit,
    pars = c(
      "noise", "eta", "lp__", "infections",
      "reports", "imputed_reports", "r",
      "gt_mean", "gt_var"
    ),
    include = FALSE
  )

  # set samples if missing
  R_samples <- dim(draws$R)[1]
  if (is.null(samples)) {
    samples <- R_samples
  }
  # extract parameters from passed stanfit object
  shift <- estimates$args$seeding_time

  # if R is given, update trajectories in stanfit object
  if (!is.null(R)) {
    if (inherits(R, "data.frame") && is.null(R$sample)) {
      R <- R$value
    }
    if (inherits(R, "data.frame")) {
      R <- as.data.table(R)
      R <- R[, .(date, sample, value)]
      draws$R <- t(matrix(R$value, ncol = length(unique(R$sample))))
      # ignore samples and use data.frame max instead
      samples <- max(R$sample)
    } else {
      R_mat <- matrix(rep(R, each = samples),
        ncol = length(R), byrow = FALSE
      )
      orig_R <- draws$R[1:samples, ]
      draws$R <- R_mat
      draws$R[is.na(R_mat)] <- orig_R[is.na(R_mat)]
      draws$R <- matrix(draws$R, ncol = length(R))
    }
  }

  # sample from posterior if samples != posterior
  posterior_sample <- dim(draws$obs_reports)[1]
  if (posterior_sample < samples) {
    # nolint start
    posterior_samples <- sample(
      seq_len(posterior_sample), samples,
      replace = TRUE
    )
    R_draws <- draws$R
    draws <- map(draws, ~ as.matrix(.[posterior_samples, ]))
    # nolint end
    draws$R <- R_draws
  }

  # redefine time if Rt != data$t
  est_time <- estimates$args$t
  horizon <- estimates$args$horizon
  obs_time <- est_time - shift

  if (obs_time != dim(draws$R)[2]) {
    horizon <- dim(draws$R)[2] - est_time + horizon + shift
    horizon <- ifelse(horizon < 0, 0, horizon) # nolint
    est_time <- dim(draws$R)[2] + shift
    obs_time <- est_time - shift
    starting_day <- estimates$args$day_of_week[1]
    days <- max(estimates$args$day_of_week)
    day_of_week <- (
      (starting_day + rep(0:(days - 1), ceiling((obs_time) / days))) %% days
    )
    day_of_week <- day_of_week[1:(obs_time)]
    day_of_week <- ifelse(day_of_week == 0, days, day_of_week)

    estimates$args$horizon <- horizon
    estimates$args$t <- est_time
    estimates$args$day_of_week <- day_of_week
  }

  # define dates of interest
  summarised <- summary(estimates, type = "parameters")
  dates <- seq(
    min(na.omit(unique(summarised[variable == "R"]$date))) - days(shift),
    by = "day", length.out = dim(draws$R)[2] + shift
  )

  # Extract args for passing to parallel workers
  estimates_args <- estimates$args

  # Load model
  stan <- stan_opts(
    model = model, backend = backend, chains = 1, samples = 1, warmup = 1
  )

  ## set up batch simulation
  batch_simulate <- function(estimates_args, draws, model, stan,
                             shift, dates, nstart, nend) {
    # extract batch samples from draws
    draws <- map(draws, ~ matrix(.[nstart:nend, ], nrow = nend - nstart + 1))

    ## prepare data for stan command
    stan_data <- c(
      list(n = dim(draws$R)[1], initial_as_scale = 1), draws, estimates_args
    )

    ## allocate empty parameters
    stan_data <- allocate_empty(
      stan_data, c("delay_params", "params"),
      n = stan_data$n
    )

    stan_args <- create_stan_args(
      stan,
      data = stan_data, fixed_param = TRUE, model = "simulate_infections",
      verbose = FALSE
    )

    ## simulate
    sims <- fit_model(stan_args, id = "simulate_infections")

    format_simulation_output(sims, stan_data,
      reported_inf_dates = dates,
      reported_dates = dates[-(1:shift)],
      imputed_dates = dates[-(1:shift)],
      drop_length_1 = TRUE, merge = TRUE
    )
  }

  ## set up batching
  if (!is.null(batch_size)) {
    batch_no <- ceiling(samples / batch_size)
    nstarts <- seq(1, by = batch_size, length.out = batch_no)
    nends <- c(
      seq(batch_size, by = batch_size, length.out = batch_no - 1), samples
    )
    batches <- list_transpose(list(nstarts, nends), simplify = FALSE)
  } else {
    batches <- list(list(1, samples))
  }

  safe_batch <- safely(batch_simulate)

  process_batches <- function(p = NULL) {
    lapply_func(batches,
      function(batch) {
        if (!is.null(p)) {
          p()
        }
        safe_batch(
          estimates_args, draws, model, stan,
          shift, dates, batch[[1]],
          batch[[2]]
        )[[1]]
      },
      future.opts = list(
        future.seed = TRUE,
        future.globals = c(
          "estimates_args", "draws", "model", "stan", "shift", "dates",
          "safe_batch"
        )
      ),
      backend = backend
    )
  }

  ## simulate in batches
  if (verbose && requireNamespace("progressr", quietly = TRUE)) {
    p <- progressr::progressor(along = batches)
    progressr::with_progress({
      regional_out <- process_batches(p)
    })
  } else {
    regional_out <- process_batches()
  }

  ## join batches
  regional_out <- compact(regional_out)
  regional_out <- list_transpose(regional_out, simplify = FALSE)
  regional_out <- map(regional_out, rbindlist)

  ## format output
  format_out <- format_fit(
    posterior_samples = regional_out,
    horizon = estimates_args$horizon,
    shift = shift,
    CrIs = extract_CrIs(summarised) / 100
  )
  format_out$samples <- format_out$samples[, sample := seq_len(.N),
    by = c("variable", "time", "date", "strat")
  ]

  format_out$observations <- estimates$observations
  class(format_out) <- c("forecast_infections", class(format_out))
  return(format_out)
}
