#' Generation Time Distribution Options
#'
#' @description `r lifecycle::badge("stable")`
#' Returns generation time parameters in a format for lower level model use. The
#' generation time can either be given as a \code{disease} and \code{source} to
#' be passed to [get_generation_time], or as parameters of a distribution to be
#' passed to [delay_dist].
#' @param ... Any parameters to be passed to [delay_dist], if the generation time
#' is given as parameters of a distribution rather than a \code{disease} and
#' \code{source}. In this case if the \code{mean} parameter is not set a mean of
#' 1 will be assumed, if the \code{max} parameter not set then the \code{max} will
#' be set to 15 to ensure backwards compatibility, and if no \code{dist} parameter
#' is given then a gamma distribution will be used for backwards compatibility.
#' @param max Integer, defaults to 15. Maximum generation time.
#' @param fixed Logical, defaults to `FALSE`. Should the generation time be
#' treated as coming from fixed (vs uncertain) distributions.
#' @param prior_weight numeric, weight given to the generation time prior.
#' By default the generation time prior will be weighted by the number of
#' observation data points, usually preventing the posteriors from shifting
#' much from the given distribution. Another sensible option would be 1,
#' i.e. treating the generation time distribution as a single parameter.
#' @inheritParams get_generation_time
#' @seealso convert_to_logmean convert_to_logsd bootstrapped_dist_fit delay_dist
#' @return A list summarising the input delay distributions.
#' @export
#' @examples
#' # default settings with a fixed generation time
#' generation_time_opts()
#'
#' # A fixed gamma distributed generation time
#' generation_time_opts(mean = 3, sd = 2)
#'
#' # An uncertain gamma distributed generation time
#' generation_time_opts(mean = 3, sd = 2, mean_sd = 1, sd_sd = 0.5)

generation_time_opts <- function(..., disease, source, max = 15L,
                                 fixed = FALSE, prior_weight = NULL) {
  dot_options <- list(...) ## options for delay_dist
  ## check consistent options are given
  type_options <- (length(dot_options) > 0) + ## distributional parameters
    (!missing(disease) && !missing(source)) ## from included distributions
  if (type_options > 1)  {
    stop("Generation time should be given either as distributional options  ",
         "or as disease/source, but not both.")
  }

  if (!missing(disease) && !missing(source)) { ## generation time provided as disease/source
    dist <- get_generation_time(
      disease = disease, source = source, max_value = max
    )
    dist$fixed <- fixed
    gt <- do.call(delay_dist, dist)
  } else { ## generation time provided as distributional parameters or not at all
    ## make gamma default for backwards compatibility
    if (!("dist" %in% names(dot_options))) {
      dot_options$dist <- "gamma"
    }
    ## set max
    dot_options$max <- max
    ## set default of mean=1 for backwards compatibility
    if (!("mean" %in% names(dot_options))) {
      dot_options$mean <- 1
    }
    dot_options$fixed <- fixed
    gt <- do.call(delay_dist, dot_options)
  }
  gt$weight <- prior_weight

  names(gt) <- paste0("gt_", names(gt))

  return(gt)
}

#' Delay Distribution Options
#'
#' @description `r lifecycle::badge("stable")`
#' Returns delay distributions formatted for usage by downstream
#' functions.
#' @param ... Parameters of discretised (upper-)truncated lognormal delay
#' distributions as a list with the following parameters:
#' "mean", the mu parameter or mean of the natural logarithm of the delay;
#' "mean_sd", the standard deviation in the estimate of "mean" parameter
#' (assumed normally distributed); "sd", the sigma parameter or standard
#' deviation of the natural logarithm of the delay; "sd_sd", the standard
#' deviation of the estimate of the "sd" parameter (assumed normally
#' distributed) sd_sd"; and "max", the maximum delay.
#' The "mean" parameter is mandatory; if it is the only one given it represents
#' a fixed delay and must be integer-valued; if "sd" is also given and
#' greater than 0 this represents a delay distribution and "mean" can be
#' real-valued. In that case, "max" also needs to be given.
#' The "mean_sd" and "sd_sd" parameters should be provided to represent
#' uncertainty in the parameter values of the delay but are optional.
#' @param fixed Logical, defaults to `FALSE`. Should all reporting delays be
#' treated as coming from fixed (vs uncertain) distributions. Making this
#' simplification drastically reduces compute requirements. Setting this here
#' overrides any of the constituent delay distributions being set to be fixed
#' or not.
#' @seealso convert_to_logmean convert_to_logsd bootstrapped_dist_fit
#' @return A list summarising the input delay distributions.
#' @export
#' @examples
#' # no delays
#' delay_opts()
#'
#' # A single delay that has uncertainty
#' delay <- list(mean = 1, mean_sd = 0.2, sd = 0.5, sd_sd = 0.1, max = 15)
#' delay_opts(delay)
#'
#' # A single delay where we override the uncertainty assumption
#' delay_opts(delay, fixed = TRUE)
#'
#' # A delay where uncertainty is implict
#' delay_opts(list(mean = 1, mean_sd = 0, sd = 0.5, sd_sd = 0, max = 15))
delay_opts <- function(..., fixed = FALSE) {
  delays <- list(...)
  data <- lapply(delays, do.call, what = delay_dist)

  if (fixed) { ## set all to fixed
    data <- lapply(data, function(x) {
      x$fixed <- 1L
      x$mean_sd <- 0
      x$sd_sd <- 0
      return(x)
    })
  }

  if (length(data) > 0) {
    data <- purrr::transpose(data)
    ## convert back to arrays
    data <- lapply(data, function(x) array(unlist(x)))
  } else {
    data <- delay_dist()
  }

  names(data) <- paste0("delay_", names(data))
  # Estimate the mean delay -----------------------------------------------
  data$seeding_time <- sum(purrr::map2_dbl(
    data$delay_mean_mean, data$delay_sd_mean, ~ exp(.x + .y^2 / 2))
  )
  if (data$seeding_time < 1) {
    data$seeding_time <- 1
  } else {
    data$seeding_time <- as.integer(data$seeding_time)
  }

  data$delays <- length(delays)

  data$uncertain_mean_delays <- array(which(data$delay_mean_sd > 0))
  data$uncertain_sd_delays <- array(which(data$delay_sd_sd > 0))
  data$fixed_delays <- array(
    which(data$delay_mean_sd == 0 & data$delay_sd_sd == 0)
  )

  data$n_uncertain_mean_delays <- length(data$uncertain_mean_delays)
  data$n_uncertain_sd_delays <- length(data$uncertain_sd_delays)
  data$n_fixed_delays <- length(data$fixed_delays)

  return(data)
}

#' Truncation Distribution Options
#'
#' @description `r lifecycle::badge("stable")`
#' Returns a truncation distribution formatted for usage by
#'  downstream functions. See `estimate_truncation()` for an approach to
#'  estimate this distribution.
#' @param dist Parameters of a discretised (upper-)truncated lognormal
#' truncation distribution as a list with parameters that are all passed to
#' [delay_dist].
#' @seealso convert_to_logmean convert_to_logsd bootstrapped_dist_fit delay_dist
#' @return A list summarising the input truncation distribution.
#' @export
#' @examples
#' # no truncation
#' trunc_opts()
#'
#' # truncation dist
#' trunc_opts(dist = list(mean = 3, sd = 2))
trunc_opts <- function(dist = list()) {
  data <- do.call(delay_dist, dist)
  names(data) <- paste0("trunc_", names(data))
  data$truncation <- as.integer(length(data$trunc_max) > 0)
  return(data)
}

#' Time-Varying Reproduction Number Options
#'
#' @description `r lifecycle::badge("stable")`
#' Defines a list specifying the optional arguments for the time-varying reproduction number.
#' Custom settings can be supplied which override the defaults.
#' @param prior List containing named numeric elements "mean" and "sd". The mean and
#'  standard deviation of the log normal Rt prior. Defaults to mean of 1 and standard
#'  deviation of 1.
#' @param use_rt Logical, defaults to `TRUE`. Should Rt be used to generate infections
#'  and hence reported cases.
#' @param rw Numeric step size of the random walk, defaults to 0. To specify a weekly random
#'   walk set `rw = 7`. For more custom break point settings consider passing in a `breakpoints`
#'   variable as outlined in the next section.
#' @param use_breakpoints Logical, defaults to `TRUE`. Should break points be used if present
#'  as a `breakpoint` variable in the input data. Break points should be defined as 1 if present
#'  and otherwise 0. By default breakpoints are fit jointly with a global non-parametric effect
#'  and so represent a conservative estimate of break point changes (alter this by setting `gp = NULL`).
#' @param pop Integer, defaults to 0. Susceptible population initially present. Used to adjust
#' Rt estimates when otherwise fixed based on the proportion of the population that is
#' susceptible. When set to 0 no population adjustment is done.
#' @param gp_on Character string, defaulting to  "R_t-1". Indicates how the Gaussian process,
#' if in use, should be applied to Rt.  Currently supported options are applying the Gaussian
#' process to the last estimated Rt (i.e Rt = Rt-1 * GP), and applying the Gaussian process to
#'  a global mean (i.e Rt = R0 * GP). Both should produced comparable results when data is not
#'  sparse but the method relying on a global mean will revert to this for real time estimates,
#'  which may not be desirable.
#' @return A list of settings defining the time-varying reproduction number
#' @inheritParams create_future_rt
#' @export
#' @examples
#' # default settings
#' rt_opts()
#'
#' # add a custom length scale
#' rt_opts(prior = list(mean = 2, sd = 1))
#'
#' # add a weekly random walk
#' rt_opts(rw = 7)
rt_opts <- function(prior = list(mean = 1, sd = 1),
                    use_rt = TRUE,
                    rw = 0,
                    use_breakpoints = TRUE,
                    future = "latest",
                    gp_on = "R_t-1",
                    pop = 0) {
  rt <- list(
    prior = prior,
    use_rt = use_rt,
    rw = rw,
    use_breakpoints = use_breakpoints,
    future = future,
    pop = pop,
    gp_on = match.arg(gp_on, choices = c("R_t-1", "R0"))
  )

  # replace default settings with those specified by user
  if (rt$rw > 0) {
    rt$use_breakpoints <- TRUE
  }

  if (!("mean" %in% names(rt$prior) & "sd" %in% names(rt$prior))) {
    stop("prior must have both a mean and sd specified")
  }
  return(rt)
}

#' Back Calculation Options
#'
#' @description `r lifecycle::badge("stable")`
#' Defines a list specifying the optional arguments for the back calculation
#' of cases. Only used if `rt = NULL`.
#' @param prior A character string defaulting to "reports". Defines the prior to use
#' when deconvolving. Currently implemented options are to use smoothed mean delay
#' shifted reported cases ("reports"), to use the estimated infections from the
#' previous time step seeded for the first time step using mean shifted reported cases
#' ("infections"), or no prior ("none"). Using no prior will result in poor real time performance.
#' No prior and using infections are only supported when a Gaussian process is present. If observed
#' data is not reliable then it a sensible first step is to explore increasing the `prior_window` with
#' a sensible second step being to no longer use reported cases as a prior (i.e set `prior = "none"`).
#' @param prior_window Integer, defaults to 14 days. The mean centred smoothing window
#' to apply to mean shifted reports (used as a prior during back calculation). 7 days is minimum recommended
#' settings as this smooths day of the week effects but depending on the quality of the data and the
#' amount of information users wish to use as a prior (higher values equalling a less informative prior).
#' @param rt_window Integer, defaults to 1. The size of the centred rolling average to use when estimating
#' Rt. This must be odd so that the central estimate is included.
#' @return A list of back calculation settings
#' @export
#' @examples
#' # default settings
#' backcalc_opts()
backcalc_opts <- function(prior = "reports", prior_window = 14, rt_window = 1) {
  backcalc <- list(
    prior = match.arg(prior, choices = c("reports", "none", "infections")),
    prior_window = prior_window,
    rt_window = as.integer(rt_window)
  )
  if (backcalc$rt_window %% 2 == 0) {
    stop("Rt rolling average window must be odd in order to include the current estimate")
  }
  return(backcalc)
}

#' Approximate Gaussian Process Settings
#'
#' @description `r lifecycle::badge("stable")`
#' Defines a list specifying the structure of the approximate Gaussian
#'  process. Custom settings can be supplied which override the defaults.
#' @param ls_mean Numeric, defaults to 21 days. The mean of the lognormal length scale.
#' @param ls_sd Numeric, defaults to 7 days. The standard deviation of the log normal length
#' scale. If \code{ls_sd = 0}, inverse-gamma prior on Gaussian process length scale will
#' be used with recommended parameters \code{inv_gamma(1.499007, 0.057277 * ls_max)}.
#' @param ls_max Numeric, defaults to 60. The maximum value of the length scale. Updated in
#' `create_gp_data` to be the length of the input data if this is smaller.
#' @param ls_min Numeric, defaults to 0. The minimum value of the length scale.
#' @param alpha_sd Numeric, defaults to 0.05. The standard deviation of the magnitude parameter of
#' the Gaussian process kernel. Should be approximately the expected standard deviation of the logged Rt.
#' @param kernel Character string, the type of kernel required. Currently supporting the squared exponential
#' kernel ("se") and the 3 over 2 Matern kernel ("matern", with `matern_type = 3/2`). Defaulting to the Matern 3 over 2 kernel as discontinuities are expected
#' in Rt and infections.
#' @param matern_type Numeric, defaults to 3/2. Type of Matern Kernel to use. Currently only the Matern
#' 3/2 kernel is supported.
#' @param basis_prop Numeric, proportion of time points to use as basis functions. Defaults to 0.1. Decreasing
#' this value results in a decrease in accuracy but a faster compute time (with increasing it having the first
#' effect). In general smaller posterior length scales require a higher proportion of basis functions.
#' See (Riutort-Mayol et al. 2020 <https://arxiv.org/abs/2004.11408>) for advice on updating this default.
#' This setting is an area of active research.
#' @param boundary_scale Numeric, defaults to 1.5. Boundary scale of the approximate Gaussian process. See
#' (Riutort-Mayol et al. 2020 <https://arxiv.org/abs/2004.11408>) for advice on updating this
#' default.
#' @return A list of settings defining the Gaussian process
#' @export
#' @examples
#' # default settings
#' gp_opts()
#'
#' # add a custom length scale
#' gp_opts(ls_mean = 4)
gp_opts <- function(basis_prop = 0.2,
                    boundary_scale = 1.5,
                    ls_mean = 21,
                    ls_sd = 7,
                    ls_min = 0,
                    ls_max = 60,
                    alpha_sd = 0.05,
                    kernel = "matern",
                    matern_type = 3 / 2) {
  gp <- list(
    basis_prop = basis_prop,
    boundary_scale = boundary_scale,
    ls_mean = ls_mean,
    ls_sd = ls_sd,
    ls_min = ls_min,
    ls_max = ls_max,
    alpha_sd = alpha_sd,
    kernel = match.arg(kernel, choices = c("se", "matern_3/2")),
    matern_type = matern_type
  )

  if (gp$matern_type != 3 / 2) {
    stop("only the Matern 3/2 kernel is currently supported")
  }
  return(gp)
}

#' Observation Model Options
#'
#' @description `r lifecycle::badge("stable")`
#' Defines a list specifying the structure of the observation
#' model. Custom settings can be supplied which override the defaults.
#' @param family Character string defining the observation model. Options are
#' Negative binomial ("negbin"), the default, and Poisson.
#' @param phi A numeric vector of length 2, defaults to 0, 1. Indicates the
#' mean and standard deviation of the normal prior used for the observation
#' process.
#' @param weight Numeric, defaults to 1. Weight to give the observed data in
#'  the log density.
#' @param week_effect Logical defaulting to `TRUE`. Should a day of the week effect
#'  be used in the observation model.
#' @param week_length Numeric assumed length of the week in days, defaulting to
#' 7 days. This can be modified if data aggregated over a period other than a week
#' or if data has a non-weekly periodicity.
#' @param scale List, defaulting to an empty list. Should an scaling factor be applied
#'  to map latent infections (convolved to date of report). If none empty a mean
#'  (`mean`) and standard deviation (`sd`) needs to be supplied defining the normally
#'  distributed scaling factor.
#' @param likelihood Logical, defaults to `TRUE`. Should the likelihood be
#'  included in the model
#' @param return_likelihood Logical, defaults to `FALSE`. Should the likelihood
#'  be returned by the model.
#' @return A list of observation model settings.
#' @export
#' @examples
#' # default settings
#' obs_opts()
#'
#' # Turn off day of the week effect
#' obs_opts(week_effect = TRUE)
#'
#' # Scale reported data
#' obs_opts(scale = list(mean = 0.2, sd = 0.02))
obs_opts <- function(family = "negbin",
                     phi = c(0, 1),
                     weight = 1,
                     week_effect = TRUE,
                     week_length = 7,
                     scale = list(),
                     likelihood = TRUE,
                     return_likelihood = FALSE) {
  if (length(phi) != 2 | !is.numeric(phi)) {
    stop("phi be numeric and of length two")
  }
  obs <- list(
    family = match.arg(family, choices = c("poisson", "negbin")),
    phi = phi,
    weight = weight,
    week_effect = week_effect,
    week_length = week_length,
    scale = scale,
    likelihood = likelihood,
    return_likelihood = return_likelihood
  )

  if (length(obs$scale) != 0) {
    scale_names <- names(obs$scale)
    scale_correct <- "mean" %in% scale_names & "sd" %in% scale_names
    if (!scale_correct) {
      stop("If specifying a scale both a mean and sd are needed")
    }
  }
  return(obs)
}

#' Rstan Sampling Options
#'
#' @description `r lifecycle::badge("stable")`
#'  Defines a list specifying the arguments passed to
#' `rstan::sampling`. Custom settings can be supplied which override the defaults.
#' @param cores Number of cores to use when executing the chains in parallel,
#'  which defaults to 1 but it is recommended to set the mc.cores option to be as
#'  many processors as the hardware and RAM allow (up to the number of chains).
#' @param warmup Numeric, defaults to 250. Number of warmup samples per chain.
#' @param samples Numeric, default 2000. Overall number of posterior samples.
#' When using multiple chains iterations per chain is samples / chains.
#' @param chains Numeric, defaults to 4. Number of MCMC chains to use.
#' @param control List, defaults to empty. control parameters to pass to underlying
#' `rstan` function. By default `adapt_delta = 0.95` and `max_treedepth = 15`
#' though these settings can be overwritten.
#' @param save_warmup Logical, defaults to FALSE. Should warmup progress be saved.
#' @param seed Numeric, defaults uniform random number between 1 and 1e8. Seed of
#' sampling process.
#' @param future Logical, defaults to `FALSE`. Should stan chains be run in parallel
#' using `future`. This allows users to have chains fail gracefully (i.e when combined with
#' `max_execution_time`). Should be combined with a call to `future::plan`
#' @param max_execution_time Numeric, defaults to Inf (seconds). If set will kill off
#' processing of each chain if not finished within the specified timeout. When more than 2 chains
#' finish successfully estimates will still be returned. If less than 2 chains return within the
#' allowed time then estimation will fail with an informative error.
#' @param ... Additional parameters to pass to `rstan::sampling`.
#' @return A list of arguments to pass to `rstan::sampling`
#' @export
#' @examples
#' rstan_sampling_opts(samples = 2000)
rstan_sampling_opts <- function(cores = getOption("mc.cores", 1L),
                                warmup = 250,
                                samples = 2000,
                                chains = 4,
                                control = list(),
                                save_warmup = FALSE,
                                seed = as.integer(runif(1, 1, 1e8)),
                                future = FALSE,
                                max_execution_time = Inf,
                                ...) {
  opts <- list(
    cores = cores,
    warmup = warmup,
    chains = chains,
    save_warmup = save_warmup,
    seed = seed,
    future = future,
    max_execution_time = max_execution_time
  )
  control_def <- list(adapt_delta = 0.95, max_treedepth = 15)
  opts$control <- update_list(control_def, control)
  opts$iter <- ceiling(samples / opts$chains) + opts$warmup
  opts <- c(opts, ...)
  return(opts)
}

#' Rstan Variational Bayes Options
#'
#' @description `r lifecycle::badge("stable")`
#'  Defines a list specifying the arguments passed to
#' `rstan::vb`. Custom settings can be supplied which override the defaults.
#' @param samples Numeric, default 2000. Overall number of approximate posterior
#' samples.
#' @param trials Numeric, defaults to 10. Number of attempts to use `rstan::vb`
#' before failing.
#' @param iter Numeric, defaulting to 10000. Number of iterations to use in
#' `rtan::vb`.
#' @param ... Additional parameters to pass to `rstan::vb`.
#' @return A list of arguments to pass to `rstan::vb`
#' @export
#' @examples
#' rstan_vb_opts(samples = 1000)
rstan_vb_opts <- function(samples = 2000,
                          trials = 10,
                          iter = 10000, ...) {
  opts <- list(
    trials = trials,
    iter = iter,
    output_samples = samples
  )
  opts <- c(opts, ...)
  return(opts)
}

#' Rstan Options
#'
#' @description `r lifecycle::badge("stable")`
#' Defines a list specifying the arguments passed to underlying `rstan`
#' functions via `rstan_sampling_opts()` and `rstan_vb_opts()`.Custom settings can be supplied
#'  which override the defaults.
#' @param object Stan model object. By default uses the compiled package default.
#' @param method A character string, defaulting to sampling. Currently supports
#' `rstan::sampling` ("sampling") or `rstan:vb` ("vb").
#' @param ... Additional parameters to pass  underlying option functions.
#' @return A list of arguments to pass to the appropriate rstan functions.
#' @export
#' @inheritParams rstan_sampling_opts
#' @seealso rstan_sampling_opts rstan_vb_opts
#' @examples
#' rstan_opts(samples = 1000)
#'
#' # using vb
#' rstan_opts(method = "vb")
rstan_opts <- function(object = NULL,
                       samples = 2000,
                       method = "sampling", ...) {
  method <- match.arg(method, choices = c("sampling", "vb"))
  # shared everywhere opts
  if (is.null(object)) {
    object <- stanmodels$estimate_infections
  }
  opts <- list(
    object = object,
    method = method
  )
  if (method %in% "sampling") {
    opts <- c(opts, rstan_sampling_opts(samples = samples, ...))
  } else if (method %in% "vb") {
    opts <- c(opts, rstan_vb_opts(samples = samples, ...))
  }
  return(opts)
}

#' Stan Options
#'
#' @description `r lifecycle::badge("stable")`
#' Defines a list specifying the arguments passed to underlying stan
#' backend functions via `rstan_sampling_opts()` and `rstan_vb_opts()`. Custom settings
#' can be supplied which override the defaults.
#' @param backend Character string indicating the backend to use for fitting stan models.
#' Currently only "rstan" is supported.
#' @param init_fit `r lifecycle::badge("experimental")`
#' Character string or `stanfit` object, defaults to NULL. Should an initial fit be used to
#' initialise the full fit. An example scenario would be using a national level fit to parametrise
#' regional level fits. Optionally a character string can be passed with the currently supported
#' option being "cumulative". This fits the model to cumulative cases and may be useful for certain
#' data sets where the sampler gets stuck or struggles to initialise. See `init_cumulative_fit()` for details.
#' This implementation is based on the approach taken in [epidemia](https://github.com/ImperialCollegeLondon/epidemia/)
#' authored by James Scott.
#' @param return_fit Logical, defaults to TRUE. Should the fit stan model be returned.
#' @param ... Additional parameters to pass  underlying option functions.
#' @return A list of arguments to pass to the appropriate rstan functions.
#' @export
#' @inheritParams rstan_opts
#' @seealso rstan_opts
#' @examples
#' # using default of rstan::sampling
#' stan_opts(samples = 1000)
#'
#' # using vb
#' stan_opts(method = "vb")
stan_opts <- function(samples = 2000,
                      backend = "rstan",
                      init_fit = NULL,
                      return_fit = TRUE,
                      ...) {
  backend <- match.arg(backend, choices = c("rstan"))
  if (backend %in% "rstan") {
    opts <- rstan_opts(
      samples = samples,
      ...
    )
  }
  if (!is.null(init_fit)) {
    if (is.character(init_fit)) {
      init_fit <- match.arg(init_fit, choices = "cumulative")
    }
    opts$init_fit <- init_fit
  }
  opts <- c(opts, list(return_fit = return_fit))
  return(opts)
}

#' Return an _opts List per Region
#'
#' @description `r lifecycle::badge("maturing")`
#' Define a list of `_opts()` to pass to `regional_epinow` `_opts()` accepting arguments.
#' This is useful when different settings are needed between regions within a single
#' `regional_epinow` call. Using `opts_list` the defaults can be applied to all regions
#' present with an override passed to regions as necessary (either within `opts_list` or
#' externally).
#' @param opts An `_opts()` function call such as `rt_opts()`
#' @param reported_cases A data frame containing a `region` variable
#' indicating the target regions
#' @param ... Optional override for region defaults. See the examples
#' for use case.
#' @return A named list of options per region which can be passed to the `_opt`
#' accepting arguments of `regional_epinow`
#' @seealso regional_epinow rt_opts
#' @export
#' @examples
#' # uses example case vector
#' cases <- example_confirmed[1:40]
#' cases <- data.table::rbindlist(list(
#'   data.table::copy(cases)[, region := "testland"],
#'   cases[, region := "realland"]
#' ))
#'
#' # default settings
#' opts_list(rt_opts(), cases)
#'
#' # add a weekly random walk in realland
#' opts_list(rt_opts(), cases, realland = rt_opts(rw = 7))
#'
#' # add a weekly random walk externally
#' rt <- opts_list(rt_opts(), cases)
#' rt$realland$rw <- 7
#' rt
opts_list <- function(opts, reported_cases, ...) {
  regions <- unique(reported_cases$region)
  default <- rep(list(opts), length(regions))
  names(default) <- regions
  out <- update_list(default, list(...))
  return(out)
}

#' Filter Options for a Target Region
#'
#' @description `r lifecycle::badge("maturing")`
#' A helper function that allows the selection of region specific settings if
#' present and otherwise applies the overarching settings
#' @param opts Either a list of calls to an `_opts()` function or a single
#' call to an `_opts()` function.
#' @param region A character string indicating a region of interest.
#' @return A list of options
filter_opts <- function(opts, region) {
  if (region %in% names(opts)) {
    out <- opts[[region]]
  } else {
    out <- opts
  }
  return(out)
}
