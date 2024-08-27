#' Fit an Integer Adjusted Exponential, Gamma or Lognormal distributions
#'
#' @description `r lifecycle::badge("stable")`
#' Fits an integer adjusted exponential, gamma or lognormal distribution using
#' stan.
#' @param values Numeric vector of values
#'
#' @param samples Numeric, number of samples to take. Must be >= 1000.
#' Defaults to 1000.
#'
#' @param dist Character string, which distribution to fit. Defaults to
#' exponential (`"exp"`) but gamma (`"gamma"`) and lognormal (`"lognormal"`) are
#' also supported.
#'
#' @param cores Numeric, defaults to 1. Number of CPU cores to use (no effect
#' if greater than the number of chains).
#'
#' @param chains Numeric, defaults to 2. Number of MCMC chains to use. More is
#' better with the minimum being two.
#'
#' @param verbose Logical, defaults to FALSE. Should verbose progress messages
#' be printed.
#'
#' @return A stan fit of an interval censored distribution
#' @export
#' @inheritParams stan_opts
#' @importFrom cli cli_warn col_blue
#' @examples
#' \donttest{
#' # integer adjusted exponential model
#' dist_fit(rexp(1:100, 2),
#'   samples = 1000, dist = "exp",
#'   cores = ifelse(interactive(), 4, 1), verbose = TRUE
#' )
#'
#'
#' # integer adjusted gamma model
#' dist_fit(rgamma(1:100, 5, 5),
#'   samples = 1000, dist = "gamma",
#'   cores = ifelse(interactive(), 4, 1), verbose = TRUE
#' )
#'
#' # integer adjusted lognormal model
#' dist_fit(rlnorm(1:100, log(5), 0.2),
#'   samples = 1000, dist = "lognormal",
#'   cores = ifelse(interactive(), 4, 1), verbose = TRUE
#' )
#' }
dist_fit <- function(values = NULL, samples = 1000, cores = 1,
                     chains = 2, dist = "exp", verbose = FALSE,
                     backend = "rstan") {
  if (samples < 1000) {
    samples <- 1000
    cli_warn(
      c(
        "!" = "{.var samples} must be at least {col_blue(\"1000\")}.",
        "i" = "Now setting it to {col_blue(\"1000\")} internally."
      )
    )
  }
  # model parameters
  lows <- values - 1
  lows <- ifelse(lows <= 0, 1e-6, lows)
  ups <- values + 1

  data <- list(
    N = length(values),
    low = lows,
    up = ups,
    lam_mean = numeric(0),
    prior_mean = numeric(0),
    prior_sd = numeric(0),
    par_sigma = numeric(0)
  )

  model <- epinow2_stan_model(backend, "dist_fit")

  if (dist == "exp") {
    data$dist <- 0
    data$lam_mean <- array(mean(values))
  } else if (dist == "lognormal") {
    data$dist <- 1
    data$prior_mean <- array(log(mean(values)))
    data$prior_sd <- array(log(sd(values)))
  } else if (dist == "gamma") {
    data$dist <- 2
    data$prior_mean <- array(mean(values))
    data$prior_sd <- array(sd(values))
    data$par_sigma <- array(1.0)
  }

  # set adapt delta based on the sample size
  if (length(values) <= 30) {
    adapt_delta <- 0.999
  } else {
    adapt_delta <- 0.9
  }

  # fit model
  args <- create_stan_args(
    stan = stan_opts(
      model,
      samples = samples,
      warmup = 1000,
      control = list(adapt_delta = adapt_delta),
      chains = chains,
      cores = cores
    ),
    data = data, verbose = verbose, model = "dist_fit"
  )

  fit <- fit_model(args, id = "dist_fit")

  return(fit)
}


#' Fit a Subsampled Bootstrap to Integer Values and Summarise Distribution
#' Parameters
#'
#' @description `r lifecycle::badge("stable")`
#' Fits an integer adjusted distribution to a subsampled bootstrap of data and
#' then integrates the posterior samples into a single set of summary
#' statistics. Can be used to generate a robust reporting delay that accounts
#' for the fact the underlying delay likely varies over time or that the size
#' of the available reporting delay sample may not be representative of the
#' current case load.
#'
#' @param values Integer vector of values.
#'
#' @param dist Character string, which distribution to fit. Defaults to
#' lognormal (`"lognormal"`) but gamma (`"gamma"`) is also supported.
#'
#' @param verbose Logical, defaults to `FALSE`. Should progress messages be
#' printed.
#'
#' @param samples Numeric, number of samples to take overall from the
#' bootstrapped posteriors.
#'
#' @param bootstraps Numeric, defaults to 1. The number of bootstrap samples
#' (with replacement) of the delay distribution to take. If `samples` is less
#' than `bootstraps`, `samples` takes the value of `bootstraps`.
#'
#' @param bootstrap_samples Numeric, defaults to 250. The number of samples to
#' take in each bootstrap if the sample size of the supplied delay
#' distribution is less than its value.
#'
#' @param max_value Numeric, defaults to the maximum value in the observed
#' data. Maximum delay to  allow (added to output but does impact fitting).
#'
#' @return A `<dist_spec>` object summarising the bootstrapped distribution
#' @importFrom purrr list_transpose
#' @importFrom future.apply future_lapply
#' @importFrom rstan extract
#' @importFrom data.table data.table rbindlist
#' @importFrom cli cli_abort col_blue
#' @export
#' @examples
#' \donttest{
#' # lognormal
#' delays <- rlnorm(500, log(5), 1)
#' out <- bootstrapped_dist_fit(delays,
#'   samples = 1000, bootstraps = 10,
#'   dist = "lognormal"
#' )
#' out
#' }
bootstrapped_dist_fit <- function(values, dist = "lognormal",
                                  samples = 2000, bootstraps = 10,
                                  bootstrap_samples = 250, max_value,
                                  verbose = FALSE) {
  if (!dist %in% c("gamma", "lognormal")) {
    cli_abort(
      c(
        "x" = "Unsupported distribution.",
        "i" = "Only {col_blue(\"lognormal\")} and {col_blue(\"gamma\")}
      distributions are supported"
      )
    )
  }
  if (samples < bootstraps) {
    samples <- bootstraps
  }
  ## Make values integer if not
  values <- as.integer(values)
  ## Remove NA values
  values <- values[!is.na(values)]
  ## Filter out negative values
  values <- values[values >= 0]

  get_single_dist <- function(values, samples = samples,
                              single_sample_size = 1) {
    set_dt_single_thread()

    fit <- EpiNow2::dist_fit(values, samples = samples, dist = dist)

    out <- list()
    if (dist == "lognormal") {
      out$meanlog <- sample(extract(fit)$mu, single_sample_size)
      out$sdlog <- sample(extract(fit)$sigma, single_sample_size)
    } else if (dist == "gamma") {
      out$shape <- sample(extract(fit)$alpha, single_sample_size)
      out$rate <- sample(extract(fit)$beta, single_sample_size)
    }
    return(out)
  }

  if (bootstraps == 1) {
    dist_samples <- get_single_dist(values,
                                    samples = samples,
                                    single_sample_size = samples
    )
  } else {
    ## Fit each sub sample
    dist_samples <- future.apply::future_lapply(1:bootstraps,
      function(boot) {
        get_single_dist(
          sample(values,
            min(length(values), bootstrap_samples),
            replace = TRUE
          ),
          samples = samples,
          single_sample_size = ceiling(samples / bootstraps)
        )
      },
      future.scheduling = Inf,
      future.globals = c(
        "values", "bootstraps", "samples",
        "bootstrap_samples", "get_single_dist"
      ),
      future.packages = "data.table", future.seed = TRUE
    )


    dist_samples <- purrr::list_transpose(dist_samples, simplify = FALSE)
    dist_samples <- purrr::map(dist_samples, unlist)
  }

  params <- lapply(dist_samples, function(x) {
    Normal(mean = mean(x), sd = sd(x))
  })

  if (!missing(max_value)) {
    max <- max_value
  } else {
    max <- max(values)
  }
  return(new_dist_spec(params = params, max = max, distribution = dist))
}

#' Estimate a Delay Distribution
#'
#' @description `r lifecycle::badge("maturing")`
#' Estimate a log normal delay distribution from a vector of integer delays.
#' Currently this function is a simple wrapper for [bootstrapped_dist_fit()].
#'
#' @param delays Integer vector of delays
#'
#' @param ... Arguments to pass to internal methods.
#'
#' @return A `<dist_spec>` summarising the bootstrapped distribution
#' @export
#' @seealso [bootstrapped_dist_fit()]
#' @examples
#' \donttest{
#' delays <- rlnorm(500, log(5), 1)
#' estimate_delay(delays, samples = 1000, bootstraps = 10)
#' }
estimate_delay <- function(delays, ...) {
  fit <- bootstrapped_dist_fit(
    values = delays,
    dist = "lognormal", ...
  )
  return(fit)
}
