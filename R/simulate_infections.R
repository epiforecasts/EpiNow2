#' Simulate infections using a given trajectory of the time-varying reproduction number
#'
#' @description `r lifecycle::badge("stable")`
#' This function simulates infections using an existing fit to observed cases but with a modified
#' time-varying reproduction number. This can be used to explore forecast models or past counterfactuals.
#' Simulations can be run in parallel using `future::plan`.
#' @param estimates The \code{estimates} element of an \code{epinow} run that
#' has been done with output = "fit", or the result of
#' \code{estimate_infections} with \code{return_fit} set to TRUE.
#'
#' @param model A compiled stan model as returned by `rstan::stan_model`.
#'
#' @param R A numeric vector of reproduction numbers; these will overwrite the
#' reproduction numbers contained in \code{estimates}, except elements set to
#' NA. Alternatively accepts a data.frame containing at least `date` and `value`
#' (integer) variables and optionally `sample`. More (or fewer) days than in
#' the original fit can be simulated.
#'
#' @param samples Numeric, number of posterior samples to simulate from. The
#' default is to use all samples in the `estimates` input.
#'
#' @param batch_size Numeric, defaults to 10. Size of batches in which to
#' simulate. May decrease run times due to reduced IO costs but this is still
#' being evaluated. If set to NULL then al simulations are done at once.
#'
#' @param verbose Logical defaults to `interactive()`. Should a progress bar
#' (from `progressr`) be shown.
#' @importFrom rstan extract sampling
#' @importFrom purrr transpose map safely compact
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor
#' @importFrom data.table rbindlist as.data.table
#' @importFrom lubridate days
#' @return A list of output as returned by [estimate_infections()] but based on
#' results from the specified scenario rather than fitting.
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
#' # set up example generation time
#' generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#' # set delays between infection and case report
#' incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#' reporting_delay <- list(
#'   mean = convert_to_logmean(2, 1), mean_sd = 0.1,
#'   sd = convert_to_logsd(2, 1), sd_sd = 0.1, max = 15
#' )
#'
#' # fit model to data to recover Rt estimates
#' est <- estimate_infections(reported_cases,
#'   generation_time = generation_time,
#'   delays = delay_opts(incubation_period, reporting_delay),
#'   rt = rt_opts(prior = list(mean = 2, sd = 0.1), rw = 7),
#'   stan = stan_opts(control = list(adapt_delta = 0.9)),
#'   obs = obs_opts(scale = list(mean = 0.1, sd = 0.01)),
#'   gp = NULL, horizon = 0
#' )
#'
#' # update Rt trajectory and simulate new infections using it
#' R <- c(rep(NA_real_, 26), rep(0.5, 10), rep(0.8, 7))
#' sims <- simulate_infections(est, R)
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
#' sims <- simulate_infections(est, R_dt)
#' plot(sims)
#'
#' #' # with a data.frame input of samples
#' R_samples <- summary(est, type = "samples", param = "R")
#' R_samples <- R_samples[, .(date, sample, value)][sample <= 1000][date <= "2020-04-10"]
#' R_samples <- R_samples[date >= "2020-04-01", value := 1.1]
#' sims <- simulate_infections(est, R_samples)
#' plot(sims)
#'
#' options(old_opts)
#' }
simulate_infections <- function(estimates,
                                R = NULL,
                                model = NULL,
                                samples = NULL,
                                batch_size = 10,
                                verbose = interactive()) {
  ## check batch size
  if (!is.null(batch_size)) {
    if (batch_size <= 1) {
      stop("batch_size must be greater than 1")
    }
  }
  ## extract samples from given stanfit object
  draws <- extract(estimates$fit,
    pars = c(
      "noise", "eta", "lp__", "infections",
      "reports", "imputed_reports", "r"
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
    if (any(class(R) %in% "data.frame")) {
      if (is.null(R$sample)) {
        R <- R$value
      }
    }
    if (any(class(R) %in% "data.frame")) {
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
    posterior_samples <- sample(1:posterior_sample, samples, replace = TRUE)
    R_draws <- draws$R
    draws <- map(draws, ~ as.matrix(.[posterior_samples, ]))
    draws$R <- R_draws
  }

  # redefine time if Rt != data$t
  time <- estimates$args$t
  horizon <- estimates$args$h
  obs_time <- time - shift

  if (obs_time != dim(draws$R)[2]) {
    horizon <- dim(draws$R)[2] - time + horizon + shift
    horizon <- ifelse(horizon < 0, 0, horizon)
    time <- dim(draws$R)[2] + shift
    obs_time <- time - shift
    starting_day <- estimates$args$day_of_week[1]
    days <- max(estimates$args$day_of_week)
    day_of_week <- (
      (starting_day + rep(0:(days - 1), ceiling((obs_time) / days))) %% days)
    day_of_week <- day_of_week[1:(obs_time)]
    day_of_week <- ifelse(day_of_week == 0, days, day_of_week)

    estimates$args$horizon <- horizon
    estimates$args$t <- time
    estimates$args$day_of_week <- day_of_week
  }

  # define dates of interest
  dates <-
    seq(
      min(na.omit(unique(estimates$summarised[variable == "R"]$date)))
      - days(shift),
      by = "day", length.out = dim(draws$R)[2] + shift
    )

  # Load model
  if (is.null(model)) {
    model <- stanmodels$simulate_infections
  }

  ## set up batch simulation
  batch_simulate <- function(estimates, draws, model,
                             shift, dates, nstart, nend) {
    # extract batch samples from draws
    draws <- map(draws, ~ as.matrix(.[nstart:nend, ]))

    ## prepare data for stan command
    data <- c(list(n = dim(draws$R)[1]), draws, estimates$args)

    ## allocate empty parameters
    data <- allocate_empty(
      data, c("frac_obs", "delay_mean", "delay_sd", "rep_phi"),
      n = data$n
    )

    ## simulate
    sims <- sampling(
      object = model,
      data = data, chains = 1, iter = 1,
      algorithm = "Fixed_param",
      refresh = 0
    )

    out <- extract_parameter_samples(sims, data,
      reported_inf_dates = dates,
      reported_dates = dates[-(1:shift)],
      drop_length_1 = TRUE, merge = TRUE
    )
    return(out)
  }

  ## set up batching
  if (!is.null(batch_size)) {
    batch_no <- ceiling(samples / batch_size)
    nstarts <- seq(1, by = batch_size, length.out = batch_no)
    nends <- c(seq(batch_size, by = batch_size, length.out = batch_no - 1), samples)
    batches <- transpose(list(nstarts, nends))
  } else {
    batches <- list(list(1, samples))
  }

  safe_batch <- safely(batch_simulate)

  ## simulate in batches
  with_progress({
    if (verbose) {
      p <- progressor(along = batches)
    }
    out <- future_lapply(batches,
      function(batch) {
        if (verbose) {
          p()
        }
        safe_batch(
          estimates, draws, model,
          shift, dates, batch[[1]],
          batch[[2]]
        )[[1]]
      },
      future.seed = TRUE
    )
  })

  ## join batches
  out <- compact(out)
  out <- transpose(out)
  out <- map(out, ~ data.table::rbindlist(.))

  ## format output
  format_out <- format_fit(
    posterior_samples = out,
    horizon = estimates$args$horizon,
    shift = shift,
    burn_in = 0,
    start_date = min(estimates$observations$date),
    CrIs = extract_CrIs(estimates$summarised) / 100
  )
  format_out$samples <- format_out$samples[, sample := 1:.N,
    by = c("variable", "time", "date", "strat")
  ]

  format_out$observations <- estimates$observations
  class(format_out) <- c("estimate_infections", class(format_out))
  return(format_out)
}
