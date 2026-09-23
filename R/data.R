#' Example generation time
#'
#' @description
#' An example of a generation time estimate. See here for details:
#' https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/generation-time.R
#' @format A `dist_spec` object summarising the distribution
"example_generation_time"

# nolint start: line_length_linter.
#' Example incubation period
#'
#' @description
#' An example of an incubation period estimate. See here for details:
#'  https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/incubation-period.R
#' @format A `dist_spec` object summarising the distribution
"example_incubation_period"
# nolint end: line_length_linter.

# nolint start: line_length_linter.
#' Example reporting delay
#'
#' @description
#' An example of an reporting delay estimate. See here for details:
#'  https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/reporting-delay
#' @format A `dist_spec` object summarising the distribution
"example_reporting_delay"
# nolint end: line_length_linter.

#' Example Confirmed Case Data Set
#'
#' @description
#' An example data frame of observed cases
#' @format A data frame containing cases reported on each date.
"example_confirmed"

# nolint start: line_length_linter.
#' Example Case Data Set with Truncation
#'
#' @description
#' An example dataset of observed cases with truncation applied.
#' This data is generated internally for use in the example of
#' `estimate_truncation()`. For details on how the data is generated, see
#' <https://github.com/epiforecasts/EpiNow2/blob/main/data-raw/truncated.R>
#' @format A list of `data.table`s containing cases reported on each date until
#' a point of truncation.
#' Each element of the list is a `data.table` with the following columns:
#' \describe{
#'   \item{date}{Date of case report.}
#'   \item{confirm}{Number of confirmed cases.}
#' }
"example_truncated"
# nolint end: line_length_linter.
