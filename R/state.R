#' Time-varying parameters
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These constructors mark a model parameter as varying over time, driven by a
#' stochastic state. `GP()` uses an approximate Gaussian process and `RW()` a
#' random walk. They wrap a `<dist_spec>` giving the prior on the value the
#' parameter reverts to (`mean`, a stationary / mean-reverting state) or on its
#' initial value (`init`, a state on first differences). Exactly one of `mean`
#' or `init` must be supplied.
#'
#' The wrapped prior may itself be a known trajectory (e.g. a `NonParametric()`
#' built from a data column), in which case the state fits deviations around
#' that mean. The link function applied to the resulting trajectory (e.g. log
#' for positive parameters, logit for probabilities) is a property of the
#' parameter
#' and is set where the parameter is registered, not here.
#'
#' @details
#' The state specification deliberately carries only the value/state part of a
#' time-varying parameter. The level (whether the overall value is estimated or
#' `Fixed()`) and the link function are handled where the parameter is used.
#'
#' These functions define the user interface for time-varying parameters. Wiring
#' them into the models is ongoing; passing a state specification where a model
#' parameter is not yet supported raises an informative error.
#'
#' @param mean A `<dist_spec>` giving the prior on the (stationary) mean the
#'   state reverts to, or a numeric vector giving a known mean trajectory the
#'   state fits deviations around. Supply either `mean` or `init`, not both.
#' @param init A `<dist_spec>` giving the prior on the initial value of a state
#'   on first differences, or a numeric vector giving known initial value(s).
#'   Supply either `mean` or `init`, not both.
#' @return A `<state_spec>` object describing the time-varying state.
#' @name state
#' @rdname state
NULL

#' Construct a state specification
#'
#' @param type Character, the state type (`"gp"` or `"rw"`).
#' @param mean,init A `<dist_spec>`; exactly one must be supplied.
#' @param settings A list of additional state settings (e.g. a `<gp_opts>`
#'   object for `"gp"`, or the step standard deviation prior for `"rw"`).
#' @return A `<state_spec>` object.
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_class
#' @keywords internal
new_state_spec <- function(type, mean, init, settings = list()) {
  has_mean <- !missing(mean) && !is.null(mean)
  has_init <- !missing(init) && !is.null(init)
  if (has_mean + has_init != 1) {
    cli_abort(
      c(
        "!" = "Exactly one of {.arg mean} or {.arg init} must be supplied.",
        "i" = "Use {.arg mean} for a mean-reverting (stationary) state or
        {.arg init} for a state on first differences."
      )
    )
  }
  anchor <- if (has_mean) "mean" else "init"
  prior <- if (has_mean) mean else init
  ## the anchor may be a prior (a <dist_spec>) or a known trajectory supplied as
  ## a numeric vector (the state then fits deviations around it)
  if (!is(prior, "dist_spec") && !is.numeric(prior)) {
    cli_abort(
      c(
        "!" = "{.arg {anchor}} must be a {.cls dist_spec} or a numeric vector.",
        "i" = "Supply a prior (e.g. {.fn Normal}) or a known trajectory as a
        numeric vector."
      )
    )
  }

  state <- list(
    type = type,
    anchor = anchor,
    prior = prior,
    settings = settings
  )
  class(state) <- c(
    paste0(type, "_state"), "state_spec", "param_spec", "list"
  )
  state
}

#' @rdname state
#' @param basis_prop Numeric, the proportion of time points to use as basis
#'   functions for the Gaussian process. Defaults to 0.2.
#' @param boundary_scale Numeric, defaults to 1.5. Boundary scale of the
#'   approximate Gaussian process.
#' @param ls A `<dist_spec>` giving the prior on the Gaussian process
#'   lengthscale (on the scale of days). Defaults to
#'   `LogNormal(mean = 21, sd = 7, max = 60)`.
#' @param alpha A `<dist_spec>` giving the prior on the Gaussian process
#'   magnitude. Defaults to `Normal(mean = 0, sd = 0.01)` (a lower limit of 0 is
#'   enforced where the parameter is used).
#' @param kernel Character string, the type of kernel. One of the Matern kernel
#'   ("matern", the default), squared exponential kernel ("se"),
#'   Ornstein-Uhlenbeck kernel ("ou"), or periodic kernel ("periodic").
#' @param matern_order Numeric, defaults to 3/2. Order of the Matern kernel.
#'   Common choices are 1/2, 3/2, and 5/2. Set automatically for the "se" and
#'   "ou" kernels. Only used if `kernel` is "matern".
#' @param w0 Numeric, defaults to 1.0. Fundamental frequency for the periodic
#'   kernel. Only used if `kernel` is "periodic".
#' @export
#' @examples
#' # mean-reverting Gaussian process
#' GP(mean = Normal(mean = 5, sd = 1))
#' # Gaussian process on first differences
#' GP(init = Normal(mean = 5, sd = 1))
#' # Gaussian process with a squared exponential kernel
#' GP(init = Normal(mean = 5, sd = 1), kernel = "se")
GP <- function(mean, init,
               basis_prop = 0.2,
               boundary_scale = 1.5,
               ls = LogNormal(mean = 21, sd = 7, max = 60),
               alpha = Normal(mean = 0, sd = 0.01),
               kernel = c("matern", "se", "ou", "periodic"),
               matern_order = 3 / 2,
               w0 = 1.0) {
  new_state_spec(
    "gp", mean, init,
    settings = new_gp_settings(
      basis_prop = basis_prop, boundary_scale = boundary_scale, ls = ls,
      alpha = alpha, kernel = kernel, matern_order = matern_order, w0 = w0
    )
  )
}

#' @rdname state
#' @param sd A `<dist_spec>` giving the prior on the random walk step standard
#'   deviation. Defaults to a half-normal `Normal(mean = 0, sd = 0.1)` (the
#'   lower limit of 0 is enforced where the parameter is used).
#' @param period Integer; the number of time steps between random walk steps,
#'   i.e. the value is held constant for `period` steps before changing.
#'   Defaults to 1 (a step every time point). Set `period = 7` for a weekly
#'   random walk.
#' @importFrom checkmate assert_class assert_integerish
#' @export
#' @examples
#' # random walk with an initial-value prior
#' RW(init = Normal(mean = 5, sd = 1))
#' # mean-reverting random walk with a custom step size prior
#' RW(mean = Normal(mean = 5, sd = 1), sd = Normal(mean = 0, sd = 0.05))
#' # weekly random walk
#' RW(init = Normal(mean = 5, sd = 1), period = 7)
RW <- function(mean, init, sd = Normal(mean = 0, sd = 0.1), period = 1) {
  assert_class(sd, "dist_spec")
  assert_integerish(period, lower = 1, len = 1)
  new_state_spec(
    "rw", mean, init, settings = list(sd = sd, period = as.integer(period))
  )
}

#' Test whether an object is a time-varying state specification
#'
#' @param x An object to test.
#' @return Logical, `TRUE` if `x` is a `<state_spec>`.
#' @keywords internal
is_state_spec <- function(x) {
  inherits(x, "state_spec")
}

#' Test whether an object is a parameter specification
#'
#' A `<param_spec>` is the common superclass of `<dist_spec>` (a constant or
#' uncertain value) and `<state_spec>` (a time-varying value created by [GP()]
#' or [RW()]). It is the type accepted wherever a parameter's value may be
#' either constant or time-varying.
#'
#' @param x An object to test.
#' @return Logical, `TRUE` if `x` is a `<param_spec>`.
#' @keywords internal
is_param_spec <- function(x) {
  inherits(x, "param_spec")
}

#' @export
print.state_spec <- function(x, ...) {
  type <- if (x$type == "gp") "Gaussian process" else "random walk"
  variant <- if (x$anchor == "mean") {
    "mean-reverting"
  } else {
    "on first differences"
  }
  cat(
    "Time-varying parameter: ", type, " (", variant, ")\n", sep = ""
  )
  if (is.numeric(x$prior)) {
    label <- if (x$anchor == "mean") {
      "known mean trajectory"
    } else {
      "known initial value(s)"
    }
    cat("- ", label, ": ", paste(x$prior, collapse = " "), "\n", sep = "")
  } else {
    label <- if (x$anchor == "mean") "mean prior" else "initial-value prior"
    cat("- ", label, ":\n", sep = "")
    print(x$prior)
  }
  if (x$type == "rw") {
    cat("- step sd prior:\n", sep = "")
    print(x$settings$sd)
  }
  invisible(x)
}

#' Draw samples from a `<dist_spec>` prior
#'
#' @description Internal helper that draws `n` values from the distribution
#'   represented by a `<dist_spec>`, resolving any uncertainty in its parameters
#'   first. Values are constrained to be at least `lower`.
#' @param d A `<dist_spec>`.
#' @param n Number of values to draw.
#' @param lower Lower bound to enforce on the drawn values.
#' @return A numeric vector of length `n`.
#' @importFrom stats rlnorm rnorm rgamma
#' @keywords internal
sample_dist_values <- function(d, n, lower = -Inf) {
  d <- fix_parameters(d, strategy = "sample")
  dist_family <- get_distribution(d)
  p <- get_parameters(d)
  vals <- switch(dist_family,
    lognormal = rlnorm(n, p$meanlog, p$sdlog),
    normal = rnorm(n, p$mean, p$sd),
    gamma = rgamma(n, shape = p$shape, rate = p$rate),
    fixed = rep(p$value, n),
    cli_abort(
      "Cannot sample from a {.val {dist_family}} prior for a state plot."
    )
  )
  pmax(vals, lower)
}

#' Gaussian process kernel covariance for prior-predictive state plots
#'
#' @param n Number of time points.
#' @param alpha Gaussian process magnitude.
#' @param rho Gaussian process lengthscale.
#' @param kernel Kernel type (one of "se", "matern", "ou", "periodic").
#' @param matern_order Matern order (used when `kernel` is "matern").
#' @return An `n` by `n` covariance matrix.
#' @keywords internal
state_kernel_cov <- function(n, alpha, rho, kernel, matern_order) {
  d <- abs(outer(seq_len(n), seq_len(n), "-"))
  nu <- if (kernel == "ou") 0.5 else matern_order
  corr <- if (kernel == "se" || is.infinite(nu)) {
    exp(-0.5 * (d / rho)^2)
  } else if (nu == 0.5) {
    exp(-d / rho)
  } else if (nu == 1.5) {
    (1 + sqrt(3) * d / rho) * exp(-sqrt(3) * d / rho)
  } else if (nu == 2.5) {
    (1 + sqrt(5) * d / rho + 5 * d^2 / (3 * rho^2)) * exp(-sqrt(5) * d / rho)
  } else {
    exp(-0.5 * (d / rho)^2)
  }
  alpha^2 * corr + diag(1e-6, n)
}

#' Plot prior-predictive trajectories of a time-varying state
#'
#' @description `r lifecycle::badge("experimental")`
#' Draws sample trajectories from the prior of a `GP()` or `RW()` state
#' specification to visualise the time-varying behaviour the prior implies
#' before fitting. Gaussian process draws use the chosen kernel directly (the
#' model uses an approximation to the same process).
#'
#' @param x A `<state_spec>` as created by [GP()] or [RW()].
#' @param n Integer; number of time points to simulate. Defaults to 50.
#' @param samples Integer; number of prior trajectories to draw. Defaults to 50.
#' @param ... Unused.
#' @return A `<ggplot>` object.
#' @importFrom ggplot2 ggplot aes geom_line labs theme_bw
#' @importFrom data.table data.table rbindlist
#' @importFrom stats rnorm
#' @method plot state_spec
#' @export
#' @examples
#' plot(GP(init = LogNormal(mean = 1, sd = 0.5)))
#' plot(RW(mean = Normal(mean = 1, sd = 0.2)))
plot.state_spec <- function(x, n = 50L, samples = 50L, ...) {
  if (is.numeric(x$prior)) {
    cli_abort(
      "Cannot plot a state with a known (numeric) trajectory; supply a prior."
    )
  }
  init <- x$anchor == "init"
  level <- sample_dist_values(x$prior, samples, lower = 0)

  traj <- lapply(seq_len(samples), function(s) {
    if (x$type == "rw") {
      step_sd <- sample_dist_values(x$settings$sd, 1, lower = 0)
      steps <- rnorm(n - 1, 0, step_sd)
      dev <- c(0, cumsum(steps))
    } else {
      alpha <- sample_dist_values(x$settings$alpha, 1, lower = 0)
      rho <- sample_dist_values(x$settings$ls, 1, lower = 1e-3)
      kernel_cov <- state_kernel_cov(
        n, alpha, rho, x$settings$kernel, x$settings$matern_order
      )
      noise <- as.numeric(crossprod(chol(kernel_cov), rnorm(n)))
      dev <- if (init) cumsum(noise) else noise
    }
    if (init) {
      log_traj <- log(level[s]) + (dev - dev[1])
    } else {
      log_traj <- log(level[s]) + (dev - mean(dev))
    }
    data.table::data.table(sample = s, time = seq_len(n), value = exp(log_traj))
  })
  traj <- data.table::rbindlist(traj)

  type <- if (x$type == "gp") "Gaussian process" else "random walk"
  variant <- if (init) "first differences" else "mean-reverting"
  ggplot2::ggplot(
    traj, ggplot2::aes(x = time, y = value, group = sample)
  ) +
    ggplot2::geom_line(alpha = 0.3) +
    ggplot2::labs(
      x = "Time", y = "Value",
      title = paste0("Prior draws: ", type, " (", variant, ")")
    ) +
    ggplot2::theme_bw()
}
