#' Estimate a Delay Distribution (DEPRECATED)
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Please use [estimate_dist()] instead. The `estimate_delay()` function is
#' deprecated as of EpiNow2 1.8.0 in favour of the more general and robust
#' `estimate_dist()` function, which properly accounts for right truncation
#' and primary event censoring.
#'
#' @param delays Integer vector of delays
#' @param ... Arguments passed to [estimate_dist()]
#'
#' @return A `<dist_spec>` summarising the fitted distribution
#'
#' @seealso [estimate_dist()] for the replacement function
#'
#' @export
#' @examples
#' \donttest{
#' # Old way (deprecated)
#' delays <- rlnorm(500, log(5), 1)
#' # estimate_delay(delays, samples = 1000, bootstraps = 10)
#'
#' # New way (recommended)
#' # estimate_dist(delays, samples = 1000)
#' }
estimate_delay_deprecated <- function(delays, ...) {
  lifecycle::deprecate_warn(
    when = "1.8.0",
    what = "estimate_delay()",
    with = "estimate_dist()"
  )

  # For now, fall back to the old implementation
  # Once estimate_dist() is stable, this would call estimate_dist() instead
  bootstrapped_dist_fit(
    values = delays,
    dist = "lognormal",
    ...
  )
}
