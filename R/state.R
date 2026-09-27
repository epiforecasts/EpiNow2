#' Time-varying states
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A model quantity is either a *parameter* (a value held constant over time) or
#' a *state* (a value that evolves over time). These constructors turn a
#' parameter into a state: `GP()` drives it with an approximate Gaussian process
#' and `RW()` with a random walk. They wrap a `<dist_spec>` giving the prior on
#' the value the state reverts to (`mean`, a stationary / mean-reverting state)
#' or on its initial value (`init`, a state on first differences). Exactly one
#' of `mean` or `init` must be supplied.
#'
#' The wrapped prior may itself be a known trajectory (e.g. a `NonParametric()`
#' built from a data column), in which case the state fits deviations around
#' that mean. The link function applied to the resulting trajectory (e.g. log
#' for positive values, logit for probabilities) is set where the state is
#' registered, not here.
#'
#' @details
#' The state specification deliberately carries only the value part. The level
#' (whether the overall value is estimated or `Fixed()`) and the link function
#' are handled where the state is used.
#'
#' These functions define the user interface for time-varying states. Wiring
#' them into the models is ongoing; passing a state specification where it is
#' not yet supported raises an informative error.
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
#' @param future What the state does over the forecast horizon. One of
#'   `"latest"` (hold the last estimated value flat, the default), `"project"`
#'   (let the state keep varying), `"estimate"` (hold from a seeding time before
#'   the end of the data), or an integer giving the point, relative to the
#'   forecast horizon, from which it is held constant. See [GP()].
#' @return A `<state_spec>` object.
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_class
#' @keywords internal
new_state_spec <- function(type, mean, init, settings = list(),
                           future = "latest") {
  future <- validate_future(future)
  has_mean <- !missing(mean) && !is.null(mean)
  has_init <- !missing(init) && !is.null(init)
  if (has_mean && has_init) {
    cli_abort(
      c(
        "!" = "At most one of {.arg mean} or {.arg init} may be supplied.",
        "i" = "Use {.arg mean} for a mean-reverting (stationary) state or
        {.arg init} for a state on first differences."
      )
    )
  }
  ## neither supplied: a "bare" component with a shape but no baseline of its
  ## own, valid only combined with a baseline (a `constant()`/`initial()`
  ## trajectory, or another component's own mean/init) via `+`
  anchor <- if (has_mean) "mean" else if (has_init) "init" else NULL
  prior <- if (has_mean) mean else if (has_init) init else NULL
  ## the anchor may be a prior (a <dist_spec>) or a known trajectory supplied as
  ## a numeric vector (the state then fits deviations around it)
  if (!is.null(prior) && !is(prior, "dist_spec") && !is.numeric(prior)) {
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
    future = future,
    settings = settings,
    components = list(list(type = type, settings = settings))
  )
  class(state) <- c(
    paste0(type, "_state"), "state_spec", "param_spec", "list"
  )
  state
}

#' Validate a forecast-horizon `future` setting for a state
#'
#' @param future A character string (`"latest"`, `"project"`, `"estimate"`) or
#'   an integer giving the point from which the state is held fixed.
#' @return The validated `future`: an integer, or one of the allowed strings.
#' @keywords internal
validate_future <- function(future) {
  if (is.numeric(future)) {
    assert_integerish(future, len = 1, any.missing = FALSE)
    return(as.integer(future))
  }
  match.arg(future, c("latest", "project", "estimate"))
}

#' Resolve `Date` knots on a state spec to time-step positions
#'
#' A random-walk component's `knots` may be given as dates ([RW()]), which
#' are data-independent at construction time. This resolves them to 1-indexed
#' positions into `dates` (the target parameter's own time frame) once the
#' data is known, so a spec built before the data is seen (and reused across
#' `regional_epinow()`'s regions) still works.
#'
#' @param spec A `<state_spec>`, or `NULL`.
#' @param dates The `Date` vector giving the target parameter's trajectory
#'   frame (its index 1 is `dates[1]`).
#' @return `spec`, with any `Date` `knots` replaced by integer positions.
#' @keywords internal
resolve_state_dates <- function(spec, dates) {
  if (is.null(spec) || !is_state_spec(spec) || length(spec$components) == 0) {
    return(spec)
  }
  spec$components <- lapply(spec$components, function(comp) {
    if (identical(comp$type, "rw") && inherits(comp$settings$knots, "Date")) {
      idx <- match(comp$settings$knots, dates)
      if (anyNA(idx)) {
        cli_abort(
          c(
            "!" = "Some {.arg knots} dates were not found in the data's date
            column."
          )
        )
      }
      comp$settings$knots <- sort(as.integer(idx))
    }
    comp
  })
  if (!is.null(spec$type)) {
    # a single-component spec (GP()/RW(), not a composed trajectory_spec)
    # mirrors its one component's settings at the top level for
    # print()/plot(); keep that mirror in sync with the resolved copy
    spec$settings <- spec$components[[1]]$settings
  }
  spec
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
#' @param future What the state does over the forecast horizon. One of
#'   `"latest"` (the default; the last estimated value is held flat through the
#'   horizon), `"project"` (the state keeps varying into the future),
#'   `"estimate"` (held from a seeding time before the end of the data, where
#'   the most recent values are least informed), or an integer giving the point,
#'   relative to the forecast horizon, from which the state is held constant (a
#'   negative value fixes it that many steps before the horizon starts).
#' @export
#' @examples
#' # mean-reverting Gaussian process
#' GP(mean = Normal(mean = 5, sd = 1))
#' # Gaussian process on first differences
#' GP(init = Normal(mean = 5, sd = 1))
#' # Gaussian process with a squared exponential kernel
#' GP(init = Normal(mean = 5, sd = 1), kernel = "se")
#' # project the Gaussian process into the forecast horizon
#' GP(init = Normal(mean = 5, sd = 1), future = "project")
GP <- function(mean, init,
               basis_prop = 0.2,
               boundary_scale = 1.5,
               ls = LogNormal(mean = 21, sd = 7, max = 60),
               alpha = Normal(mean = 0, sd = 0.01),
               kernel = c("matern", "se", "ou", "periodic"),
               matern_order = 3 / 2,
               w0 = 1.0,
               future = "latest") {
  new_state_spec(
    "gp", mean, init,
    settings = new_gp_settings(
      basis_prop = basis_prop, boundary_scale = boundary_scale, ls = ls,
      alpha = alpha, kernel = kernel, matern_order = matern_order, w0 = w0
    ),
    future = future
  )
}

#' @rdname state
#' @param sd A `<dist_spec>` giving the prior on the random walk step standard
#'   deviation. Defaults to a half-normal `Normal(mean = 0, sd = 0.1)` (the
#'   lower limit of 0 is enforced where the parameter is used).
#' @param period Integer; the number of time steps between random walk steps,
#'   i.e. the value is held constant for `period` steps before changing.
#'   Defaults to 1 (a step every time point). Set `period = 7` for a weekly
#'   random walk. Supply at most one of `period` or `knots`.
#' @param knots A `<Date>` vector or an integer vector of 1-indexed time-step
#'   positions giving the points at which the random walk takes a new step
#'   (irregular breakpoints), instead of a regular `period`. Dates are
#'   resolved against the data's own date column when the model is fit, so a
#'   single spec is safe to reuse across `regional_epinow()`'s regions.
#'   Supply at most one of `period` or `knots`.
#' @importFrom checkmate assert_class assert_integerish
#' @export
#' @examples
#' # random walk with an initial-value prior
#' RW(init = Normal(mean = 5, sd = 1))
#' # mean-reverting random walk with a custom step size prior
#' RW(mean = Normal(mean = 5, sd = 1), sd = Normal(mean = 0, sd = 0.05))
#' # weekly random walk
#' RW(init = Normal(mean = 5, sd = 1), period = 7)
#' # irregular breakpoints at known dates
#' RW(init = Normal(mean = 5, sd = 1),
#'   knots = as.Date(c("2020-03-23", "2020-06-15"))
#' )
#' # project the random walk into the forecast horizon
#' RW(init = Normal(mean = 5, sd = 1), future = "project")
RW <- function(mean, init, sd = Normal(mean = 0, sd = 0.1), period = 1,
               knots = NULL, future = "latest") {
  assert_class(sd, "dist_spec")
  if (!is.null(knots)) {
    if (!missing(period)) {
      cli_abort(
        c("!" = "Supply at most one of {.arg period} or {.arg knots}.")
      )
    }
    if (!inherits(knots, "Date") && !is.numeric(knots)) {
      cli_abort(
        c(
          "!" = "{.arg knots} must be a {.cls Date} vector or an integer
          vector of time-step positions."
        )
      )
    }
    if (is.numeric(knots)) {
      assert_integerish(knots, lower = 1)
      knots <- sort(as.integer(knots))
    } else {
      knots <- sort(knots)
    }
    period <- NULL
  } else {
    assert_integerish(period, lower = 1, len = 1)
    period <- as.integer(period)
  }
  new_state_spec(
    "rw", mean, init, settings = list(sd = sd, period = period, knots = knots),
    future = future
  )
}

#' Construct a trajectory baseline
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' `constant()` and `initial()` give a time-varying trajectory its baseline:
#' the prior on the (stationary) mean the trajectory reverts to, or on its
#' initial value, exactly as the `mean`/`init` argument of [GP()] and [RW()]
#' does for a single component. Compose one baseline with one or more
#' components using `+`:
#'
#' ```r
#' constant(LogNormal(mean = 2, sd = 0.2)) + GP()
#' initial(LogNormal(mean = 1, sd = 1)) + GP() + RW(period = 7)
#' ```
#'
#' A trajectory has exactly one baseline. It may come from `constant()`/
#' `initial()`, or from a single component's own `mean =`/`init =` (as in
#' `GP(mean = ...) + RW()`); combining two baselines is an error. [GP()] and
#' [RW()] used without `mean`/`init` are "bare" components: a shape (and its
#' own hyperparameter priors) with no baseline of their own, valid only when
#' combined with a baseline elsewhere in the sum.
#'
#' @param prior A `<dist_spec>` giving the prior on the baseline, or a numeric
#'   vector giving a known trajectory the components fit deviations around.
#' @param future What the trajectory does over the forecast horizon; see
#'   [GP()]. Shared by all components in the composition.
#' @return A `<state_spec>` object with no components, ready to compose with
#'   `+`.
#' @seealso [GP()], [RW()]
#' @name trajectory_baseline
#' @rdname trajectory_baseline
#' @export
#' @examples
#' constant(LogNormal(mean = 2, sd = 0.2)) + GP()
#' initial(LogNormal(mean = 1, sd = 1)) + GP() + RW(period = 7)
constant <- function(prior, future = "latest") {
  new_trajectory_spec(prior, anchor = "mean", future = future)
}

#' @rdname trajectory_baseline
#' @export
initial <- function(prior, future = "latest") {
  new_trajectory_spec(prior, anchor = "init", future = future)
}

#' Construct a bare trajectory spec carrying only a baseline
#'
#' @param prior A `<dist_spec>` or numeric vector; see [constant()].
#' @param anchor `"mean"` or `"init"`.
#' @param future See [GP()].
#' @return A `<state_spec>` object with no components.
#' @keywords internal
new_trajectory_spec <- function(prior, anchor, future = "latest") {
  if (!is(prior, "dist_spec") && !is.numeric(prior)) {
    cli_abort(
      c(
        "!" = "{.arg prior} must be a {.cls dist_spec} or a numeric vector.",
        "i" = "Supply a prior (e.g. {.fn Normal}) or a known trajectory as a
        numeric vector."
      )
    )
  }
  state <- list(
    type = NULL,
    anchor = anchor,
    prior = prior,
    future = validate_future(future),
    settings = NULL,
    components = list()
  )
  class(state) <- c("trajectory_spec", "state_spec", "param_spec", "list")
  state
}

#' Compose time-varying trajectory components
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Combines two `<state_spec>` objects (from [GP()], [RW()], [constant()] or
#' [initial()]) into one trajectory whose deviation is the sum of its
#' components' deviations. Exactly one operand may carry a baseline (a prior
#' and an anchor, from `constant()`/`initial()` or a component's own
#' `mean =`/`init =`); the other(s) must be bare (`GP()`/`RW()` with neither
#' `mean` nor `init`).
#'
#' @param e1,e2 `<state_spec>` objects.
#' @return A `<state_spec>` object combining both.
#' @export
#' @examples
#' constant(LogNormal(mean = 2, sd = 0.2)) + GP() + RW(period = 7)
"+.state_spec" <- function(e1, e2) {
  if (!is_state_spec(e2)) {
    cli_abort(
      c(
        "!" = "Can only combine a time-varying state with another
        time-varying state (from {.fn GP}, {.fn RW}, {.fn constant} or
        {.fn initial}).",
        "i" = "Got a {.cls {class(e2)[1]}} on the right-hand side."
      )
    )
  }
  has_baseline <- function(s) !is.null(s$anchor)
  if (has_baseline(e1) && has_baseline(e2)) {
    cli_abort(
      c(
        "!" = "A trajectory can have only one baseline.",
        "i" = "Both sides of {.code +} already carry a baseline prior (a
        {.fn constant}/{.fn initial} wrapper, or {.arg mean}/{.arg init} on a
        component). Drop {.arg mean}/{.arg init} from one side when
        combining."
      )
    )
  }
  base <- if (has_baseline(e1)) e1 else e2
  other <- if (has_baseline(e1)) e2 else e1
  ## a state's components share one free-noise/forecast window (see GP()'s
  ## `future`), so a component's own non-default `future` must agree with the
  ## baseline's rather than being silently dropped
  if (!identical(other$future, "latest") &&
      !identical(other$future, base$future)) {
    cli_abort(
      c(
        "!" = "Components of one trajectory share a single forecast-horizon
        setting.",
        "i" = "Set {.arg future} once, on the baseline ({.fn constant}/
        {.fn initial}, or the component that carries {.arg mean}/
        {.arg init}); a bare component's own {.arg future} must then be left
        at the default or match it."
      )
    )
  }
  state <- list(
    type = NULL,
    anchor = base$anchor,
    prior = base$prior,
    future = base$future,
    settings = NULL,
    components = c(e1$components, e2$components)
  )
  class(state) <- c("trajectory_spec", "state_spec", "param_spec", "list")
  state
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
#' A parameter specification is either a `<dist_spec>` (a constant or uncertain
#' value, defined in the \pkg{distspec} package) or a `<state_spec>` (a
#' time-varying value created by [GP()] or [RW()]). It is the type accepted
#' wherever a parameter's value may be either constant or time-varying. A
#' `<state_spec>` carries the `param_spec` class; a `<dist_spec>` is recognised
#' by its own class.
#'
#' @param x An object to test.
#' @return Logical, `TRUE` if `x` is a `<dist_spec>` or a `<state_spec>`.
#' @keywords internal
is_param_spec <- function(x) {
  inherits(x, "param_spec") || inherits(x, "dist_spec")
}

#' Assert that an object is a parameter specification
#'
#' @param x An object to check.
#' @param name Name used to refer to `x` in the error message.
#' @return Invisibly returns `x` if it is a parameter specification; otherwise
#' aborts.
#' @keywords internal
assert_param_spec <- function(x, name = deparse(substitute(x))) {
  if (!is_param_spec(x)) {
    cli_abort(
      c(
        "{.arg {name}} must be a fixed distribution or a time-varying state.",
        "i" = "Supply a {.cls dist_spec} (e.g. from {.fn Fixed} or
               {.fn LogNormal}) or a {.cls state_spec} (from {.fn GP} or
               {.fn RW})."
      ),
      class = "epinow2_invalid_param_spec"
    )
  }
  invisible(x)
}

#' Describe one trajectory component for `print()`
#'
#' @param comp A component list (`type`, `settings`), as stored in a
#'   `<state_spec>`'s `components` field.
#' @keywords internal
describe_component <- function(comp) {
  label <- if (comp$type == "gp") "Gaussian process" else "random walk"
  cat("+ ", label, "\n", sep = "")
  if (comp$type == "rw") {
    cat("  step sd prior:\n", sep = "")
    print(comp$settings$sd)
  }
}

#' @export
print.state_spec <- function(x, ...) {
  type <- if (x$type == "gp") "Gaussian process" else "random walk"
  if (is.null(x$anchor)) {
    cat(
      "Time-varying state: ", type, " (bare component, no baseline)\n",
      sep = ""
    )
    cat("Combine with a baseline (constant()/initial(), or mean=/init= on
        another component) using +.\n")
    return(invisible(x))
  }
  variant <- if (x$anchor == "mean") {
    "mean-reverting"
  } else {
    "on first differences"
  }
  cat(
    "Time-varying state: ", type, " (", variant, ")\n", sep = ""
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
  if (!identical(x$future, "latest")) {
    future_label <- if (is.numeric(x$future)) {
      paste0("fixed from ", x$future)
    } else {
      x$future
    }
    cat("- forecast horizon: ", future_label, "\n", sep = "")
  }
  invisible(x)
}

#' @export
print.trajectory_spec <- function(x, ...) {
  if (is.null(x$anchor)) {
    cat("Time-varying state: deferred (no baseline yet)\n")
    cat("Combine with a baseline (constant()/initial(), or mean=/init= on one
        of the components) using +.\n")
  } else {
    variant <- if (x$anchor == "mean") {
      "mean-reverting"
    } else {
      "on first differences"
    }
    cat("Time-varying state (", variant, "):\n", sep = "")
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
  }
  if (length(x$components) == 0) {
    cat("- constant (no time-varying components)\n")
  } else {
    for (comp in x$components) describe_component(comp)
  }
  if (!identical(x$future, "latest")) {
    future_label <- if (is.numeric(x$future)) {
      paste0("fixed from ", x$future)
    } else {
      x$future
    }
    cat("- forecast horizon: ", future_label, "\n", sep = "")
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
  if (is.null(x$anchor)) {
    cli_abort(
      "Cannot plot a bare component with no baseline; combine with
      {.fn constant}/{.fn initial} (or {.arg mean}/{.arg init} on another
      component) first."
    )
  }
  if (is.numeric(x$prior)) {
    cli_abort(
      "Cannot plot a state with a known (numeric) trajectory; supply a prior."
    )
  }
  if (length(x$components) == 0) {
    cli_abort("Nothing to plot: this trajectory has no components.")
  }
  init <- x$anchor == "init"
  level <- sample_dist_values(x$prior, samples, lower = 0)

  ## sample each component's link-scale deviation and sum them, exactly as
  ## the Stan trajectory does; centring/anchoring is then applied once to the
  ## combined deviation (this reduces to the prior single-component draw when
  ## there is exactly one component)
  sample_component_dev <- function(comp) {
    if (comp$type == "rw") {
      step_sd <- sample_dist_values(comp$settings$sd, 1, lower = 0)
      steps <- rnorm(n - 1, 0, step_sd)
      c(0, cumsum(steps))
    } else {
      alpha <- sample_dist_values(comp$settings$alpha, 1, lower = 0)
      rho <- sample_dist_values(comp$settings$ls, 1, lower = 1e-3)
      kernel_cov <- state_kernel_cov(
        n, alpha, rho, comp$settings$kernel, comp$settings$matern_order
      )
      noise <- as.numeric(crossprod(chol(kernel_cov), rnorm(n)))
      if (init) cumsum(noise) else noise
    }
  }

  traj <- lapply(seq_len(samples), function(s) {
    dev <- Reduce(`+`, lapply(x$components, sample_component_dev))
    if (init) {
      log_traj <- log(level[s]) + (dev - dev[1])
    } else {
      log_traj <- log(level[s]) + (dev - mean(dev))
    }
    data.table(sample = s, time = seq_len(n), value = exp(log_traj))
  })
  traj <- rbindlist(traj)

  label <- vapply(x$components, function(comp) {
    if (comp$type == "gp") "Gaussian process" else "random walk"
  }, character(1))
  label <- paste(label, collapse = " + ")
  variant <- if (init) "first differences" else "mean-reverting"
  ggplot(
    traj, aes(x = time, y = value, group = sample)
  ) +
    geom_line(alpha = 0.3) +
    labs(
      x = "Time", y = "Value",
      title = paste0("Prior draws: ", label, " (", variant, ")")
    ) +
    theme_bw()
}

#' @rdname state
#' @param x A `<state_spec>` as created by [GP()], [RW()], [constant()] or
#'   [initial()].
#' @export
plot.trajectory_spec <- function(x, n = 50L, samples = 50L, ...) {
  plot.state_spec(x, n = n, samples = samples, ...)
}
