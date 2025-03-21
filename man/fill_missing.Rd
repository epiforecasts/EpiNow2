% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{fill_missing}
\alias{fill_missing}
\title{Fill missing data in a data set to prepare it for use within the package}
\usage{
fill_missing(
  data,
  missing_dates = c("ignore", "accumulate", "zero"),
  missing_obs = c("ignore", "accumulate", "zero"),
  initial_accumulate,
  obs_column = "confirm",
  by = NULL
)
}
\arguments{
\item{data}{Data frame with a \code{date} column. The other columns depend on the
model that the data are to be used, e.g. \code{\link[=estimate_infections]{estimate_infections()}} or
\code{\link[=estimate_secondary]{estimate_secondary()}}. See the documentation there for the expected
format. The data must not already have an \code{accumulate} function, otherwise
the function will fail with an error.}

\item{missing_dates}{Character. Options are "ignore" (the default),
"accumulate" and "zero". This determines how missing dates in the data are
interpreted.  If set to "ignore", any missing dates in the observation
data will be interpreted as missing and skipped in the likelihood. If set
to "accumulate", modelled observations on dates that are missing in the
data will be accumulated and added to the next non-missing data point.
This can be used to model incidence data that is reported less frequently
than daily. In that case, the first data point is not included in the
likelihood (unless \code{initial_accumulate} is set to a value greater than
one) but used only to reset modelled observations to zero. If "zero" then
all observations on missing dates will be assumed to be of value 0.}

\item{missing_obs}{Character. How to process dates that exist in the data
but have observations with NA values. The options available are the same
ones as for the \code{missing_dates} argument.}

\item{initial_accumulate}{Integer. The number of initial dates to accumulate
if \code{missing_dates} or \code{missing_obs} is set to \code{"accumulate"}. This
argument needs ot have a minimum of 1. If it is set to 1 then no
accumulation is happening on the first data point. If it is greater than 1
then dates are added to the beginning of the data set to get be able to
have a sufficient number of modelled observations accumulated onto the
first data point. For modelling weekly incidence data this should be set
to 7. If accumulating and the first data point is not NA and this is
argument is not set, then if all dates in the data have the same gap this
will be taken as initial accumulation and a warning given to inform the
user. If not all gaps are the same the first data point will be removed
with a warning.}

\item{obs_column}{Character (default: "confirm"). If given, only the column
specified here will be used for checking missingness. This is useful if
using a data set that has multiple columns of hwich one of them
corresponds to observations that are to be processed here.}

\item{by}{Character vector. Name(s) of any additional column(s) where
data processing should be done separately for each value in the column.
This is useful when using data representing e.g. multiple geographies. If
NULL (default) no such grouping is done.}
}
\value{
a data.table with an \code{accumulate} column that indicates whether
values are accumulated (see the documentation of the \code{data} argument in
\code{\link[=estimate_infections]{estimate_infections()}})
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
This function ensures that all days between the first and last date in the
data are present. It adds an \code{accumulate} column that indicates whether
modelled observations should be accumulated onto a later data point.
point. This is useful for modelling data that is reported less frequently
than daily, e.g. weekly incidence data, as well as other reporting
artifacts such as delayed weekedn reporting. The function can also be used
to fill in missing observations with zeros.
}
\examples{
cases <- data.table::copy(example_confirmed)
## calculate weekly sum
cases[, confirm := data.table::frollsum(confirm, 7)]
## limit to dates once a week
cases <- cases[seq(7, nrow(cases), 7)]
## set the second observation to missing
cases[2, confirm := NA]
## fill missing data
fill_missing(cases, missing_dates = "accumulate", initial_accumulate = 7)
}
