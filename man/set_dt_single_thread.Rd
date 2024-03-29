% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{set_dt_single_thread}
\alias{set_dt_single_thread}
\title{Set to Single Threading}
\usage{
set_dt_single_thread()
}
\value{
an environment in the parent frame named "dt_settings"
}
\description{
This function sets the threads used by \code{{data.table}} to 1 in the parent
function and then restores the initial \code{{data.table}} threads when the
function exits. This is primarily used as an internal function inside of
other functions and will generally not be used on its own.
}
\examples{
\donttest{
data.table::setDTthreads(2)
test_function <- function() {
  set_dt_single_thread()

  print(data.table::getDTthreads())
}
test_function()
data.table::getDTthreads()
}
}
\keyword{internal}
