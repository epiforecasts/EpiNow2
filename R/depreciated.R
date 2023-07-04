#' Back Calculation Options
#'
#' @description `r lifecycle::badge("deprecated")`
#' Defines a list specifying the optional arguments for the back calculation
#' of cases. Only used if `rt = NULL`.
#'
#' @param prior A character string defaulting to "reports". Defines the prior
#' to use when deconvolving. Currently implemented options are to use smoothed
#' mean delay shifted reported cases ("reports"), to use the estimated
#' infections from the previous time step seeded for the first time step using
#' mean shifted reported cases ("infections"), or no prior ("none"). Using no
#' prior will result in poor real time performance. No prior and using
#' infections are only supported when a Gaussian process is present . If
#' observed data is not reliable then it a sensible first step is to explore
#' increasing the `prior_window` wit a sensible second step being to no longer
#' use reported cases as a prior (i.e set `prior = "none"`).
#'
#' @param prior_window Integer, defaults to 14 days. The mean centred smoothing
#' window to apply to mean shifted reports (used as a prior during back
#' calculation). 7 days is minimum recommended settings as this smooths day of
#' the week effects but depending on the quality of the data and the amount of
#' information users wish to use as a prior (higher values equalling a less
#' informative prior).
#'
#' @param rt_window Integer, defaults to 1. The size of the centred rolling
#' average to use when estimating Rt. This must be odd so that the central
#' estimate is included.
#'
#' @return A list of back calculation settings.
#' @author Sam Abbott
#' @export
#' @examples
#' # default settings
#' backcalc_opts()
backcalc_opts <- function(prior = "reports", prior_window = 14, rt_window = 1) {
  stop("backcalc_opts is deprecated - use process_opts instead")
   backcalc <- list(
    prior = match.arg(prior, choices = c("reports", "none", "infections")),
    prior_window = prior_window,
    rt_window = as.integer(rt_window)
  )
  if (backcalc$rt_window %% 2 == 0) {
    stop(
      "Rt rolling average window must be odd in order to include the current
       estimate"
    )
  }
  return(backcalc)
}

#' Time-Varying Reproduction Number Options
#'
#' @description `r lifecycle::badge("deprecated")`
#' Defines a list specifying the optional arguments for the time-varying
#' reproduction number. Custom settings can be supplied which override the
#' defaults.
#'
#' @param prior List containing named numeric elements "mean" and "sd". The
#' mean and standard deviation of the log normal Rt prior. Defaults to mean of
#' 1 and standard deviation of 1.
#'
#' @param use_rt Logical, defaults to `TRUE`. Should Rt be used to generate
#' infections and hence reported cases.
#'
#' @param rw Numeric step size of the random walk, defaults to 0. To specify a
#' weekly random walk set `rw = 7`. For more custom break point settings
#' consider passing in a `breakpoints` variable as outlined in the next section.
#'
#' @param use_breakpoints Logical, defaults to `TRUE`. Should break points be
#' used if present as a `breakpoint` variable in the input data. Break points
#' should be defined as 1 if present and otherwise 0. By default breakpoints
#' are fit jointly with a global non-parametric effect and so represent a
#' conservative estimate of break point changes (alter this by setting
#' `gp = NULL`).
#'
#' @param pop Integer, defaults to 0. Susceptible population initially present.
#' Used to adjust Rt estimates when otherwise fixed based on the proportion of
#' the population that is susceptible. When set to 0 no population adjustment
#' is done.
#'
#' @param gp_on Character string, defaulting to  "R_t-1". Indicates how the
#' Gaussian process, if in use, should be applied to Rt. Currently supported
#' options are applying the Gaussian process to the last estimated Rt (i.e
#' Rt = Rt-1 * GP), and applying the Gaussian process to a global mean (i.e Rt
#' = R0 * GP). Both should produced comparable results when data is not sparse
#' but the method relying on a global mean will revert to this for real time
#' estimates, which may not be desirable.
#'
#' @return A list of settings defining the time-varying reproduction number.
#' @author Sam Abbott

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
  stop("rt_opts is deprecated - use process_opts instead")
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