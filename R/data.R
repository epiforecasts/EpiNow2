#' Example generation time
#'
#' @description `r lifecycle::badge("stable")`
#' An example of a generation time estimate. See here for details:
#' https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/generation-time.R
#' @format A `dist_spec` object summarising the distribution
"example_generation_time"

#' Example incubation period
#'
#' @description `r lifecycle::badge("stable")`
#' An example of an incubation period estimate. See here for details:
#'  https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/incubation-period.R # nolint
#' @format A `dist_spec` object summarising the distribution
"example_incubation_period"

#' Example reporting delay
#'
#' @description `r lifecycle::badge("stable")`
#' An example of an reporting delay estimate. See here for details:
#'  https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/reporting-delay # nolint
#' @format A `dist_spec` object summarising the distribution
"example_reporting_delay"

#' Literature Estimates of Generation Times
#'
#' @description `r lifecycle::badge("deprecated")`
#' Generation time estimates. See here for details:
#' https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/generation-time.R
#' @format A `data.table` of summarising the distribution
"generation_times"

#' Literature Estimates of Incubation Periods
#'
#' @description `r lifecycle::badge("deprecated")`
#' Incubation period estimates. See here for details:
#'  https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/incubation-period.R # nolint
#' @format A `data.table` of summarising the distribution
"incubation_periods"


#' Example Confirmed Case Data Set
#'
#' @description `r lifecycle::badge("stable")`
#' An example data frame of observed cases
#' @format A data frame containing cases reported on each date.
"example_confirmed"

#' Example Case Data Set with Truncation
#'
#' @description `r lifecycle::badge("stable")`
#' An example dataset of observed cases with truncation applied
#' This data is generated internally for use in the example of
#' `estimate_truncation()`. For details on how the data is generated, see
#' <https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/estimate-truncation.R> #nolint
#' @format A list of `data.table`s containing cases reported on each date until
#' a point of truncation.
#' #' Each element of the list is a `data.table` with the following columns:
#' \describe{
#'   \item{date}{Date of case report.}
#'   \item{confirm}{Number of confirmed cases.}
#' }
"example_truncated"
