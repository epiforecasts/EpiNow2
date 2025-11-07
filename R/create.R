#' Create Delay Shifted Cases
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This functions creates a data frame of reported cases that has been smoothed
#' using a centred partial rolling average (with a period set by
#' `smoothing_window`) and shifted back in time by some delay. It is used by
#' [estimate_infections()] to generate the mean shifted prior on which the back
#' calculation method (see [backcalc_opts()]) is based.
#'
#' @details
#' The function first shifts all the data back in time by `shift` days (thus
#' discarding the first `shift` days of data) and then applies a centred
#' rolling mean of length `smoothing_window` to the shifted data except for
#' the final period. The final period (the forecast horizon plus half the
#' smoothing window) is instead replaced by a log-linear model fit (with 1
#' added to the data for fitting to avoid zeroes and later subtracted again),
#' projected to the end of the forecast horizon. The initial part of the data
#' (corresponding to the length of the smoothing window) is then removed, and
#' any non-integer resulting values rounded up.
#'
#' @param smoothing_window Numeric, the rolling average smoothing window
#' to apply. Must be odd in order to be defined as a centred average.
#'
#' @param shift Numeric, mean delay shift to apply.
#'
#' @inheritParams estimate_infections
#' @inheritParams create_stan_data
#' @importFrom data.table copy shift frollmean fifelse .N
#' @importFrom stats lm
#' @importFrom runner mean_run
#' @return A `<data.frame>` for shifted reported cases
#' @keywords internal
#' @examples
#' \dontrun{
#' shift <- 7
#' horizon <- 7
#' smoothing_window <- 14
#' ## add NAs for horizon
#' cases <- add_horizon(example_confirmed[1:30], horizon)
#' ## add zeroes initially
#' cases <- data.table::rbindlist(list(
#'   data.table::data.table(
#'     date = seq(
#'       min(cases$date) - 10,
#'       min(cases$date) - 1,
#'       by = "days"
#'     ),
#'     confirm = 0, breakpoint = 0
#'   ),
#'   cases
#' ))
#' create_shifted_cases(cases, shift, smoothing_window, horizon)
#' }
create_shifted_cases <- function(data, shift,
                                 smoothing_window, horizon) {
  shifted_reported_cases <- copy(data)
  ## turn initial NAs into zeroes
  shifted_reported_cases[cumsum(!is.na(confirm)) == 0L, confirm := 0.0]
  ## pad with additional zeroes
  shifted_reported_cases <- pad_reported_cases(data, smoothing_window, 0.0)

  if ("accumulate" %in% colnames(data)) {
    shifted_reported_cases[
      is.na(confirm) & accumulate,
      confirm := 0
    ]
  }
  shifted_reported_cases[
    ,
    confirm := data.table::shift(confirm,
      n = shift,
      type = "lead", fill = NA
    )
  ][
    ,
    confirm := runner::mean_run(
      confirm,
      k = smoothing_window, lag = -floor(smoothing_window / 2)
    )
  ]

  ## Forecast trend on reported cases using the last week of data
  final_period <- shifted_reported_cases[!is.na(confirm)][
    max(1, .N - smoothing_window):.N
  ][
    ,
    t := seq_len(.N)
  ]
  lm_model <- stats::lm(log(confirm + 1) ~ t, data = final_period)
  ## Estimate unreported future infections using a log linear model
  shifted_reported_cases <- shifted_reported_cases[
    date >= min(final_period$date), t := seq_len(.N)
  ][
    ,
    confirm := data.table::fifelse(
      !is.na(t) & t >= 0,
      exp(lm_model$coefficients[1] + lm_model$coefficients[2] * t) - 1,
      confirm
    )
  ][, t := NULL]

  ## Drop median generation interval initial values
  shifted_reported_cases <- shifted_reported_cases[
    ,
    confirm := ceiling(confirm)
  ]
  shifted_reported_cases <- shifted_reported_cases[-(1:smoothing_window)]
  if (anyNA(shifted_reported_cases$confirm)) {
    cli::cli_abort(
      c(
        "!" = "Some values are missing after prior smoothing. Consider
        increasing the smoothing using the {.var prior_window} argument in
        {.fn backcalc_opts}."
      )
    )
  }
  shifted_reported_cases
}

#' Construct the Required Future Rt assumption
#'
#' @description `r lifecycle::badge("stable")`
#' Converts the `future` argument from [rt_opts()] into arguments that can be
#' passed to stan.
#'
#' @param future A character string or integer. This argument indicates how to
#' set future Rt values. Supported options are to project using the Rt model
#' ("project"), to use the latest estimate based on partial data ("latest"),
#' to use the latest estimate based on data that is over 50% complete
#' ("estimate"). If an integer is supplied then the Rt estimate from this many
#' days into the future (or past if negative) past will be used forwards in
#' time.
#'
#' @param delay Numeric mean delay
#' @importFrom rlang arg_match
#' @keywords internal
#' @return A list containing a logical called fixed and an integer called from
create_future_rt <- function(future = c("latest", "project", "estimate"),
                             delay = 0) {
  out <- list(fixed = FALSE, from = 0)
  if (is.character(future)) {
    future <- arg_match(future)
    if (future != "project") {
      out$fixed <- TRUE
      out$from <- ifelse(future == "latest", 0, -delay)
    }
  } else if (is.numeric(future)) {
    out$fixed <- TRUE
    out$from <- as.integer(future)
  }
  return(out)
}

#' Create Time-varying Reproduction Number Data
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output from [rt_opts()] and converts it into a list understood by
#' stan.
#'
#' @param rt A list of options as generated by [rt_opts()] defining Rt
#' estimation. Defaults to [rt_opts()]. To generate new infections using
#' the non-mechanistic model instead of the renewal equation model, use
#' `rt = NULL`. The non-mechanistic model internally uses the setting
#' `rt = rt_opts(use_rt = FALSE, future = "project", gp_on = "R0")`.
#'
#' @param breakpoints An integer vector (binary) indicating the location of
#' breakpoints.
#'
#' @param horizon Numeric, forecast horizon.
#' @importFrom cli cli_abort
#'
#' @seealso [rt_opts()]
#' @return A list of settings defining the time-varying reproduction number
#' @inheritParams create_future_rt
#' @keywords internal
#' @examples
#' \dontrun{
#' # default Rt data
#' create_rt_data()
#'
#' # settings when no Rt is desired
#' create_rt_data(rt = NULL)
#'
#' # using breakpoints
#' create_rt_data(rt_opts(use_breakpoints = TRUE), breakpoints = rep(1, 10))
#'
#' # using random walk
#' create_rt_data(rt_opts(rw = 7), breakpoints = rep(1, 10))
#' }
create_rt_data <- function(rt = rt_opts(), breakpoints = NULL,
                           delay = 0, horizon = 0) {
  # Define if GP is on or off
  if (is.null(rt)) {
    rt <- rt_opts(
      use_rt = FALSE,
      future = "project",
      gp_on = "R0",
      rw = 0
    )
  }
  # define future Rt arguments
  future_rt <- create_future_rt(
    future = rt$future,
    delay = delay
  )
  # apply random walk
  if (rt$rw != 0) {
    if (is.null(breakpoints)) {
      cli_abort(
        c(
          "!" = "breakpoints must be supplied when using random walk."
        )
      )
    }

    breakpoints <- seq_along(breakpoints)
    breakpoints <- floor(breakpoints / rt$rw)
    if (rt$future != "project") {
      max_bps <- length(breakpoints) - horizon + future_rt$from
      if (max_bps < length(breakpoints)) {
        breakpoints[(max_bps + 1):length(breakpoints)] <- breakpoints[max_bps]
      }
    }
  } else {
    breakpoints <- cumsum(breakpoints)
  }

  if (sum(breakpoints) == 0) {
    rt$use_breakpoints <- FALSE
  }
  # add a shift for 0 effect in breakpoints
  breakpoints <- breakpoints + 1

  # map settings to underlying gp stan requirements
  rt_data <- list(
    estimate_r = as.numeric(rt$use_rt),
    bp_n = ifelse(rt$use_breakpoints, max(breakpoints) - 1, 0),
    breakpoints = breakpoints,
    future_fixed = as.numeric(future_rt$fixed),
    fixed_from = future_rt$from,
    pop = rt$pop,
    stationary = as.numeric(rt$gp_on == "R0"),
    future_time = horizon - future_rt$from,
    growth_method = list(
      "infections" = 0, "infectiousness" = 1
    )[[rt$growth_method]]
  )
  return(rt_data)
}
#' Create Back Calculation Data
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output of [backcalc_opts()] and converts it into a list understood
#' by stan.
#'
#' @param backcalc A list of options as generated by [backcalc_opts()] to
#' define the back calculation. Defaults to [backcalc_opts()].
#'
#' @seealso [backcalc_opts()]
#' @importFrom data.table fcase
#' @return A list of settings defining the Gaussian process
#' @keywords internal
create_backcalc_data <- function(backcalc = backcalc_opts()) {
  list(
    rt_half_window = as.integer((backcalc$rt_window - 1) / 2),
    backcalc_prior = data.table::fcase(
      backcalc$prior == "none", 0,
      backcalc$prior == "reports", 1,
      backcalc$prior == "infections", 2,
      default = 0
    )
  )
}

#' Create Gaussian Process Data
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output of [gp_opts()] and converts it into a list understood by
#' stan.
#' @param gp A list of options as generated by [gp_opts()] to define the
#' Gaussian process. Defaults to [gp_opts()]. Set to `NULL` to disable the
#' Gaussian process.
#' @param data A list containing the following numeric values:
#' `t`, `seeding_time`, `horizon`.
#' @importFrom data.table fcase
#' @seealso [gp_opts()]
#' @return A list of settings defining the Gaussian process
#' @keywords internal
#' @examples
#' \dontrun{
#' # define input data required
#' data <- list(
#'   t = 30,
#'   seeding_time = 7,
#'   horizon = 7
#' )
#'
#' # default gaussian process data
#' create_gp_data(data = data)
#'
#' # settings when no gaussian process is desired
#' create_gp_data(NULL, data)
#'
#' # custom lengthscale
#' create_gp_data(gp_opts(ls_mean = 14), data)
#' }
create_gp_data <- function(gp = gp_opts(), data) {
  # Define if GP is on or off
  if (is.null(gp)) {
    fixed <- TRUE
    data$stationary <- 1
    gp <- gp_opts()
  } else {
    fixed <- FALSE
  }

  est_time <- data$t - data$seeding_time
  if (data$future_fixed > 0) {
    est_time <- est_time + data$fixed_from - data$horizon
  }
  if (data$stationary == 1) {
    est_time <- est_time - 1
  }

  # basis functions
  M <- ceiling(est_time * gp$basis_prop)

  # map settings to underlying gp stan requirements
  gp_data <- list(
    fixed = as.numeric(fixed),
    M = M,
    L = gp$boundary_scale,
    gp_type = data.table::fcase(
      gp$kernel == "se", 0,
      gp$kernel == "periodic", 1,
      gp$kernel == "matern" || gp$kernel == "ou", 2,
      default = 2
    ),
    nu = gp$matern_order,
    w0 = gp$w0
  )

  gp_data <- c(data, gp_data)
  return(gp_data)
}

#' Create Observation Model Settings
#'
#' @description `r lifecycle::badge("stable")`
#' Takes the output of [obs_opts()] and converts it into a list understood
#' by stan.
#' @param obs A list of options as generated by [obs_opts()] defining the
#' observation model. Defaults to [obs_opts()].
#' @param dates A vector of dates used to calculate the day of the week.
#' @seealso [obs_opts()]
#' @return A list of settings ready to be passed to stan defining
#' the Observation Model
#' @keywords internal
#' @examples
#' \dontrun{
#' dates <- seq(as.Date("2020-03-15"), by = "days", length.out = 15)
#' # default observation model data
#' create_obs_model(dates = dates)
#'
#' # Poisson observation model
#' create_obs_model(obs_opts(family = "poisson"), dates = dates)
#'
#' # Applying a observation scaling to the data
#' create_obs_model(
#'   obs_opts(scale = Normal(mean = 0.4, sd = 0.01)),
#'   dates = dates
#' )
#'
#' # Apply a custom week week length
#' create_obs_model(obs_opts(week_length = 3), dates = dates)
#' }
create_obs_model <- function(obs = obs_opts(), dates) {
  opts <- list(
    model_type = as.numeric(obs$family == "negbin"),
    week_effect = ifelse(obs$week_effect, obs$week_length, 1),
    obs_weight = obs$weight,
    obs_scale = as.integer(obs$scale != Fixed(1)),
    likelihood = as.numeric(obs$likelihood),
    return_likelihood = as.numeric(obs$return_likelihood)
  )

  opts$day_of_week <- add_day_of_week(dates, opts$week_effect)

  return(opts)
}

#' Create Stan Data Required for estimate_infections
#'
#' @description`r lifecycle::badge("stable")`
#' Takes the output of [stan_opts()] and converts it into a list understood by
#' stan. Internally calls the other `create_` family of functions to
#' construct a single list for input into stan with all data required
#' present.
#'
#' @param seeding_time Integer; seeding time, usually obtained using
#' [get_seeding_time()].
#'
#' @inheritParams estimate_infections
#' @inheritParams create_gp_data
#' @inheritParams create_obs_model
#' @inheritParams create_rt_data
#' @inheritParams create_backcalc_data
#' @inheritParams create_stan_params
#' @importFrom stats lm
#' @importFrom purrr safely
#' @return A list of stan data
#' @keywords internal
#' @examples
#' \dontrun{
#' create_stan_data(
#'   example_confirmed, 7, rt_opts(), gp_opts(), obs_opts(), 7,
#'   backcalc_opts(), create_shifted_cases(example_confirmed, 7, 14, 7)
#' )
#' }
create_stan_data <- function(data, seeding_time, rt, gp, obs, backcalc,
                             forecast, params) {
  cases <- data[(seeding_time + 1):.N]
  cases[, lookup := seq_len(.N)]
  case_times <- cases[!is.na(confirm), lookup]
  imputed_times <- cases[!(accumulate), lookup]
  accumulate <- cases$accumulate
  confirmed_cases <- cases[1:(.N - forecast$horizon)]$confirm
  if (is.null(rt)) {
    shifted_cases <- create_shifted_cases(
      data,
      shift = seeding_time,
      smoothing_window = backcalc$prior_window,
      horizon = forecast$horizon
    )
    shifted_confirmed_cases <- shifted_cases$confirm
  } else {
    shifted_confirmed_cases <- array(numeric(0))
  }


  stan_data <- list(
    cases = confirmed_cases[!is.na(confirmed_cases)],
    any_accumulate = as.integer(any(accumulate)),
    case_times = as.integer(case_times),
    imputed_times = as.integer(imputed_times),
    accumulate = as.integer(accumulate),
    lt = length(case_times),
    it = length(imputed_times),
    t = length(data$date),
    shifted_cases = shifted_confirmed_cases,
    burn_in = 0,
    seeding_time = seeding_time,
    horizon = forecast$horizon
  )
  # add Rt data
  stan_data <- c(
    stan_data,
    create_rt_data(rt,
      breakpoints = cases$breakpoint,
      delay = stan_data$seeding_time, horizon = stan_data$horizon
    )
  )
  # backcalculation settings
  stan_data <- c(stan_data, create_backcalc_data(backcalc))
  # gaussian process data
  stan_data <- create_gp_data(gp, stan_data)

  # observation model data
  stan_data <- c(
    stan_data,
    create_obs_model(obs, dates = cases$date)
  )

  # parameters
  stan_data <- c(
    stan_data,
    create_stan_params(params)
  )

  # rescale mean shifted prior for back calculation if observation scaling is
  # used
  stan_data$shifted_cases <-
    stan_data$shifted_cases / mean(obs$scale)
  stan_data
}

##' Create initial conditions for delays
##'
##' @inheritParams create_initial_conditions
##' @return A list of initial conditions for delays
##' @keywords internal
create_delay_inits <- function(stan_data) {
  out <- list()
  if (stan_data$delay_n_p > 0) {
    out$delay_params <- array(truncnorm::rtruncnorm(
      n = stan_data$delay_params_length, a = stan_data$delay_params_lower,
      mean = stan_data$delay_params_mean, sd = stan_data$delay_params_sd * 0.1
    ))
  } else {
    out$delay_params <- array(numeric(0))
  }
  out
}

#' Create Initial Conditions Generating Function
#' @description `r lifecycle::badge("stable")`
#' Uses the output of [create_stan_data()] to create a function which can be
#' used to sample from the prior distributions (or as close as possible) for
#' parameters. Used in order to initialise each stan chain within a range of
#' plausible values.
#' @param stan_data A list of data as produced by [create_stan_data()].
#' @inheritParams create_stan_params
#' @return An initial condition generating function
#' @importFrom purrr map2_dbl transpose
#' @importFrom truncnorm rtruncnorm
#' @importFrom data.table fcase
#' @keywords internal
create_initial_conditions <- function(stan_data, params) {
  function() {
    out <- create_delay_inits(stan_data)

    if (stan_data$fixed == 0) {
      out$eta <- array(rnorm(
        ifelse(stan_data$gp_type == 1, stan_data$M * 2, stan_data$M),
        mean = 0, sd = 0.1
      ))
    } else {
      out$eta <- array(numeric(0))
    }
    if (stan_data$estimate_r == 1) {
      out$initial_infections <- array(rnorm(1))
    } else {
      out$initial_infections <- array(numeric(0))
    }

    if (stan_data$bp_n > 0) {
      out$bp_sd <- array(truncnorm::rtruncnorm(1, a = 0, mean = 0, sd = 0.1))
      out$bp_effects <- array(rnorm(stan_data$bp_n, 0, 0.1))
    } else {
      out$bp_sd <- array(numeric(0))
      out$bp_effects <- array(numeric(0))
    }
    if (stan_data$week_effect > 0) {
      out$day_of_week_simplex <- array(
        rep(1 / stan_data$week_effect, stan_data$week_effect)
      )
    }
    tparams <- transpose(params)
    null <- vapply(tparams$dist, is.null, logical(1))
    fixed <- vapply(
      tparams$dist[!null], get_distribution, character(1)
    ) == "fixed"
    param_means <- vapply(
      tparams$dist[!null][!fixed],
      mean,
      ignore_uncertainty = FALSE,
      FUN.VALUE = numeric(1)
    )
    param_sds <- vapply(
      tparams$dist[!null][!fixed],
      sd,
      ignore_uncertainty = FALSE,
      FUN.VALUE = numeric(1)
    )
    out$params <- array(truncnorm::rtruncnorm(
      stan_data$n_params_variable,
      a = stan_data$params_lower,
      b = stan_data$params_upper,
      mean = param_means, sd = param_sds
    ))
    out
  }
}

#' Create a List of Stan Arguments
#'
#' @description `r lifecycle::badge("stable")`
#' Generates a list of arguments as required by the stan sampling functions by
#' combining the required options with data, and type of initialisation.
#' Initialisation defaults to random but it is expected that
#' [create_initial_conditions()] will be used.
#'
#' @param stan A list of stan options as generated by [stan_opts()]. Defaults
#' to [stan_opts()]. Can be used to override `data`, `init`, and `verbose`
#' settings if desired.
#'
#' @param data A list of stan data as created by [create_stan_data()]
#'
#' @param init Initial conditions passed to `{rstan}`. Defaults to "random"
#' (initial values randomly drawn between -2 and 2) but can also be a
#' function (as supplied by [create_initial_conditions()]).
#'
#' @param model Character, name of the model for which arguments are
#' to be created.
#' @param fixed_param Logical, defaults to `FALSE`. Should arguments be
#' created to sample from fixed parameters (used by simulation functions).
#'
#' @param verbose Logical, defaults to `FALSE`. Should verbose progress
#' messages be returned.
#'
#' @importFrom utils modifyList
#'
#' @return A list of stan arguments
#' @keywords internal
#' @examples
#' \dontrun{
#' # default settings
#' create_stan_args()
#'
#' # increasing warmup
#' create_stan_args(stan = stan_opts(warmup = 1000))
#' }
create_stan_args <- function(stan = stan_opts(),
                             data = NULL,
                             init = "random",
                             model = "estimate_infections",
                             fixed_param = FALSE,
                             verbose = FALSE) {
  if (fixed_param) {
    if (stan$backend == "rstan") {
      stan$algorithm <- "Fixed_param"
    } else if (stan$backend == "cmdstanr") {
      stan$fixed_param <- TRUE
      stan$adapt_delta <- NULL
      stan$max_treedepth <- NULL
    }
  }
  ## generate stan model
  if (is.null(stan$object)) {
    stan$object <- epinow2_stan_model(stan$backend, model)
    stan$backend <- NULL
  }
  # cmdstanr doesn't have an init = "random" argument
  if (is.character(init) && init == "random" &&
        inherits(stan$object, "CmdStanModel")) {
    init <- 2
  }
  # set up shared default arguments
  stan_args <- list(
    data = data,
    init = init,
    refresh = ifelse(verbose, 50, 0)
  )
  stan_args <- modifyList(stan_args, stan)
  stan_args$return_fit <- NULL
  return(stan_args)
}

##' Create delay variables for stan
##'
##' @param ... Named delay distributions. The names are assigned to IDs
##' @param time_points Integer, the number of time points in the data;
##'   determines weight associated with weighted delay priors; default: 1
##' @return A list of variables as expected by the stan model
##' @importFrom purrr transpose map flatten
##' @keywords internal
create_stan_delays <- function(..., time_points = 1L) {
  delays <- list(...)
  ## discretise
  delays <- map(delays, discretise, strict = FALSE)
  delays <- map(delays, collapse)
  ## get maximum delays
  bounded_delays <- map(delays, function(x) discretise(fix_parameters(x)))
  max_delay <- unname(as.numeric(flatten(map(bounded_delays, max))))
  ## number of different non-empty types
  type_n <- vapply(delays, ndist, integer(1))
  ## assign ID values to each type
  ids <- rep(0L, length(type_n))
  ids[type_n > 0] <- seq_len(sum(type_n > 0))
  names(ids) <- paste(names(type_n), "id", sep = "_")

  ## create "flat version" of delays, i.e. a list of all the delays (including
  ## elements of composite delays)
  if (length(delays) > 1) {
    flat_delays <- do.call(c, delays)
  } else {
    flat_delays <- delays
  }
  parametric <- unname(
    vapply(flat_delays, get_distribution, character(1)) != "nonparametric"
  )
  param_length <- unname(vapply(flat_delays[parametric], function(x) {
    length(get_parameters(x))
  }, numeric(1)))
  nonparam_length <- unname(vapply(flat_delays[!parametric], function(x) {
    length(x$pmf)
  }, numeric(1)))
  distributions <- unname(as.character(
    map(flat_delays[parametric], get_distribution)
  ))

  ## create stan object
  ret <- list(
    n = length(flat_delays),
    n_p = sum(parametric),
    n_np = sum(!parametric),
    types = sum(type_n > 0),
    types_p = array(as.integer(parametric))
  )

  ## delay identifiers
  ret$types_id <- integer(0)
  ret$types_id[ret$types_p == 1] <- seq_len(ret$n_p)
  ret$types_id[ret$types_p == 0] <- seq_len(ret$n_np)
  ret$types_id <- array(ret$types_id)
  ## map delays to identifiers
  ret$types_groups <- array(c(0, cumsum(unname(type_n[type_n > 0]))) + 1)

  ret$params_mean <- array(unname(as.numeric(
    map(flatten(map(flat_delays[parametric], get_parameters)), mean)
  )))
  ret$params_sd <- array(unname(as.numeric(
    map(flatten(map(flat_delays[parametric], get_parameters)), sd)
  )))
  ret$params_sd[is.na(ret$params_sd)] <- 0
  ret$max <- array(max_delay[parametric])

  ret$np_pmf <- array(unname(as.numeric(
    flatten(map(flat_delays[!parametric], get_pmf))
  )))
  ## get non zero length delay pmf lengths
  ret$np_pmf_groups <- array(c(0, cumsum(nonparam_length)) + 1)
  ## calculate total np pmf length
  ret$np_pmf_length <- sum(nonparam_length)
  ## get non zero length param lengths
  ret$params_groups <- array(c(0, cumsum(param_length)) + 1)
  ## calculate total param length
  ret$params_length <- sum(param_length)
  ## set lower bounds
  ret$params_lower <- array(unname(as.numeric(flatten(
    map(flat_delays[parametric], function(x) {
      lower_bounds(get_distribution(x))[names(get_parameters(x))]
    })
  ))))
  ## assign prior weights
  weight_priors <- vapply(
    delays[parametric], attr, "weight_prior",
    FUN.VALUE = logical(1)
  )
  ret$weight <- array(rep(1, ret$n_p))
  ret$weight[weight_priors] <- time_points
  ## assign distribution
  ret$dist <- array(match(distributions, c("lognormal", "gamma")) - 1L)

  names(ret) <- paste("delay", names(ret), sep = "_")
  ret <- c(ret, ids)

  return(ret)
}

##' Create parameters for stan
##'
##' @param params A list of `<EpiNow2.params>` as created by [make_param()]
##'
##' @return A list of variables as expected by the stan model
##' @importFrom data.table fcase
##' @importFrom purrr transpose
##' @keywords internal
create_stan_params <- function(params) {
  tparams <- transpose(params)
  ## set IDs of any parameters that is NULL to 0 and remove
  null_params <- vapply(tparams$dist, is.null, logical(1))
  null_ids <- rep(0, sum(null_params))
  if (length(null_ids) > 0) {
    names(null_ids) <- paste(
      "param_id", tparams$name[null_params], sep = "_"
    )
    params <- params[!null_params]
    tparams <- transpose(params)
  }

  ## initialise variables
  params_fixed_lookup <- rep(0L, length(params))
  params_variable_lookup <- rep(0L, length(params))

  ## identify fixed/variable parameters
  fixed <- vapply(tparams$dist, get_distribution, character(1)) == "fixed"
  params_fixed_lookup[fixed] <- seq_along(which(fixed))
  params_variable_lookup[!fixed] <- seq_along(which(!fixed))

  ## lower bounds
  lower_bounds <- unlist(tparams$lower_bound[!fixed])
  if (is.null(lower_bounds)) {
    params_lower <- array(numeric(0))
  } else {
    params_lower <- lower_bounds
  }

  ## upper bounds
  params_upper <- vapply(tparams$dist[!fixed], max, numeric(1))

  ## prior distributions
  prior_dist_name <- vapply(
    tparams$dist[!fixed], get_distribution, character(1)
  )
  prior_dist <- fcase(
    prior_dist_name == "lognormal", 0L,
    prior_dist_name == "gamma", 1L,
    prior_dist_name == "normal", 2L
  )
  ## parameters
  prior_dist_params <- lapply(tparams$dist[!fixed], get_parameters)
  prior_dist_params_lengths <- lengths(prior_dist_params)

  ## check none of the parameters are uncertain
  prior_uncertain <- vapply(prior_dist_params, function(x) {
    !all(vapply(x, is.numeric, logical(1)))
  }, logical(1))
  if (any(prior_uncertain)) {
    uncertain_priors <- tparams$name[!fixed][prior_uncertain] # nolint: object_usage_linter
    cli_abort(
      c(
        "!" = "Parameter prior distribution{?s} for {.var {uncertain_priors}}
        cannot have uncertain parameters."
      )
    )
  }

  prior_dist_params <- unlist(prior_dist_params)
  if (is.null(prior_dist_params)) {
    prior_dist_params <- numeric(0)
  }

  ## extract distributions and parameters
  ret <- list(
    n_params_variable = length(params) - sum(fixed),
    n_params_fixed = sum(fixed),
    params_lower = array(params_lower),
    params_upper = array(params_upper),
    params_fixed_lookup = array(params_fixed_lookup),
    params_variable_lookup = array(params_variable_lookup),
    params_value = array(vapply(
      tparams$dist[fixed], function(x) get_parameters(x)$value, numeric(1)
    )),
    prior_dist = array(prior_dist),
    prior_dist_params_length = sum(prior_dist_params_lengths),
    prior_dist_params = array(prior_dist_params)
  )
  ids <- seq_along(params)
  if (length(ids) > 0) {
    names(ids) <- paste("param_id", tparams$name, sep = "_")
  }
  c(ret, as.list(ids), as.list(null_ids))
}

#' Create summary output from infection estimation objects
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function creates summary output from infection estimation objects
#' (either `estimate_infections` or `forecast_infections`). It is used
#' internally by [summary.estimate_infections()] and
#' [summary.forecast_infections()] to provide a consistent summary interface.
#'
#' @param object An infection estimation object (either from
#'   [estimate_infections()] or [forecast_infections()]).
#'
#' @param type A character vector of data types to return. Defaults to
#'   "snapshot" but also supports "parameters". "snapshot" returns
#'   a summary at a given date (by default the latest date informed by data).
#'   "parameters" returns summarised parameter estimates that can be further
#'   filtered using `params` to show just the parameters of interest and date.
#'
#' @inheritParams summary.estimate_infections
#'
#' @param CrIs Numeric vector of credible intervals to calculate. Defaults
#'   to c(0.2, 0.5, 0.9).
#'
#' @param ... Additional arguments passed to [report_summary()].
#'
#' @return A `<data.frame>` of summary output, either a snapshot summary
#'   (via [report_summary()]) or parameter summaries (via
#'   [calc_summary_measures()]).
#'
#' @importFrom rlang arg_match
#' @seealso [summary.estimate_infections()] [summary.forecast_infections()]
#'   [report_summary()] [calc_summary_measures()]
#' @keywords internal
create_infection_summary <- function(object,
                                     type = c("snapshot", "parameters"),
                                     target_date = NULL, params = NULL,
                                     CrIs = c(0.2, 0.5, 0.9), ...) {
  type <- arg_match(type)

  if (is.null(target_date)) {
    target_date <- max(object$observations$date)
  } else {
    target_date <- as.Date(target_date)
  }

  samples <- get_samples(object)

  summarised <- calc_summary_measures(
    samples,
    summarise_by = c("date", "variable", "strat", "type"),
    order_by = c("variable", "date"),
    CrIs = CrIs
  )

  if (type == "snapshot") {
    out <- report_summary(
      summarised_estimates = summarised[date == target_date],
      rt_samples = samples[variable == "R"][
        date == target_date, .(sample, value)
      ],
      ...
    )
  } else if (type == "parameters") {
    out <- summarised
    if (!is.null(target_date)) {
      out <- out[date == target_date]
    }
    if (!is.null(params)) {
      out <- out[variable %in% params]
    }
  }
  out[]
}
