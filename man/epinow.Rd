% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epinow.R
\name{epinow}
\alias{epinow}
\title{Real-time Rt Estimation, Forecasting and Reporting}
\usage{
epinow(
  data,
  generation_time = gt_opts(),
  delays = delay_opts(),
  truncation = trunc_opts(),
  rt = rt_opts(),
  backcalc = backcalc_opts(),
  gp = gp_opts(),
  obs = obs_opts(),
  forecast = forecast_opts(),
  stan = stan_opts(),
  CrIs = c(0.2, 0.5, 0.9),
  return_output = is.null(target_folder),
  output = c("samples", "plots", "latest", "fit", "timing"),
  plot_args = list(),
  target_folder = NULL,
  target_date,
  logs = tempdir(),
  id = "epinow",
  verbose = interactive(),
  filter_leading_zeros = TRUE,
  zero_threshold = Inf,
  horizon
)
}
\arguments{
\item{data}{A \verb{<data.frame>} of disease reports (confirm) by date (date).
\code{confirm} must be numeric and \code{date} must be in date format. Optionally,
\code{data} can also have a logical \code{accumulate} column which indicates whether
data should be added to the next data point. This is useful when modelling
e.g. weekly incidence data. See also the \code{\link[=fill_missing]{fill_missing()}} function which
helps add the \code{accumulate} column with the desired properties when dealing
with non-daily data. If any accumulation is done this happens after
truncation as specified by the \code{truncation} argument. If all entries
of \code{confirm} are missing (\code{NA}) the returned estimates will represent the
prior distributions.}

\item{generation_time}{A call to \code{\link[=gt_opts]{gt_opts()}} (or its alias
\code{\link[=generation_time_opts]{generation_time_opts()}}) defining the generation time distribution used.
For backwards compatibility a list of summary parameters can also be passed.}

\item{delays}{A call to \code{\link[=delay_opts]{delay_opts()}} defining delay distributions and
options. See the documentation of \code{\link[=delay_opts]{delay_opts()}} and the examples below for
details.}

\item{truncation}{A call to \code{\link[=trunc_opts]{trunc_opts()}} defining the truncation of
the observed data. Defaults to \code{\link[=trunc_opts]{trunc_opts()}}, i.e. no truncation.  See the
\code{\link[=estimate_truncation]{estimate_truncation()}} help file for an approach to estimating this from
data where the \code{dist} list element returned by \code{\link[=estimate_truncation]{estimate_truncation()}} is
used as the \code{truncation} argument here, thereby propagating the uncertainty
in the estimate.}

\item{rt}{A list of options as generated by \code{\link[=rt_opts]{rt_opts()}} defining Rt
estimation. Defaults to \code{\link[=rt_opts]{rt_opts()}}. To generate new infections using
the non-mechanistic model instead of the renewal equation model, use
\code{rt = NULL}. The non-mechanistic model internally uses the setting
\code{rt = rt_opts(use_rt = FALSE, future = "project", gp_on = "R0")}.}

\item{backcalc}{A list of options as generated by \code{\link[=backcalc_opts]{backcalc_opts()}} to
define the back calculation. Defaults to \code{\link[=backcalc_opts]{backcalc_opts()}}.}

\item{gp}{A list of options as generated by \code{\link[=gp_opts]{gp_opts()}} to define the
Gaussian process. Defaults to \code{\link[=gp_opts]{gp_opts()}}. Set to \code{NULL} to disable the
Gaussian process.}

\item{obs}{A list of options as generated by \code{\link[=obs_opts]{obs_opts()}} defining the
observation model. Defaults to \code{\link[=obs_opts]{obs_opts()}}.}

\item{forecast}{A list of options as generated by \code{\link[=forecast_opts]{forecast_opts()}} defining
the forecast opitions. Defaults to \code{\link[=forecast_opts]{forecast_opts()}}. If NULL then no
forecasting will be done.}

\item{stan}{A list of stan options as generated by \code{\link[=stan_opts]{stan_opts()}}. Defaults
to \code{\link[=stan_opts]{stan_opts()}}. Can be used to override \code{data}, \code{init}, and \code{verbose}
settings if desired.}

\item{CrIs}{Numeric vector of credible intervals to calculate.}

\item{return_output}{Logical, defaults to FALSE. Should output be returned,
this automatically updates to TRUE if no directory for saving is specified.}

\item{output}{A character vector of optional output to return. Supported
options are samples ("samples"), plots ("plots"), the run time ("timing"),
copying the dated folder into a latest folder (if \code{target_folder} is not
null, set using "latest"), and the stan fit ("fit"). The default is to
return all options.}

\item{plot_args}{A list of optional arguments passed to \code{\link[=plot.epinow]{plot.epinow()}}.}

\item{target_folder}{Character string specifying where to save results (will
create if not present).}

\item{target_date}{Date, defaults to maximum found in the data if not
specified.}

\item{logs}{Character path indicating the target folder in which to store log
information. Defaults to the temporary directory if not specified. Default
logging can be disabled if \code{logs} is set to NULL. If specifying a custom
logging setup then the code for \code{\link[=setup_default_logging]{setup_default_logging()}} and the
\code{\link[=setup_logging]{setup_logging()}} function are a sensible place to start.}

\item{id}{A character string used to assign logging information on error.
Used by \code{\link[=regional_epinow]{regional_epinow()}} to assign errors to regions. Alter the default to
run with error catching.}

\item{verbose}{Logical, defaults to \code{TRUE} when used interactively and
otherwise \code{FALSE}. Should verbose debug progress messages be printed.
Corresponds to the "DEBUG" level from \code{futile.logger}. See \code{setup_logging}
for more detailed logging options.}

\item{filter_leading_zeros}{Logical, defaults to TRUE. Should zeros at the
start of the time series be filtered out.}

\item{zero_threshold}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} Numeric defaults
to Inf. Indicates if detected zero cases are meaningful by using a threshold
number of cases based on the 7-day average. If the average is above this
threshold then the zero is replaced using \code{fill}.}

\item{horizon}{Deprecated; use \code{forecast} instead to specify the predictive
horizon}
}
\value{
A list of output from estimate_infections with additional elements
summarising results and reporting errors if they have occurred.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
This function wraps the functionality of \code{\link[=estimate_infections]{estimate_infections()}} in order
to estimate Rt and cases by date of infection and forecast these infections
into the future. In addition to the functionality of
\code{\link[=estimate_infections]{estimate_infections()}} it produces additional summary output useful for
reporting results and interpreting them as well as error catching and
reporting, making it particularly useful for production use e.g. running at
set intervals on a dedicated server.
}
\examples{
\donttest{
# set number of cores to use
old_opts <- options()
options(mc.cores = ifelse(interactive(), 4, 1))

# set an example generation time. In practice this should use an estimate
# from the literature or be estimated from data
generation_time <- Gamma(
  shape = Normal(1.3, 0.3),
  rate = Normal(0.37, 0.09),
  max = 14
)
# set an example incubation period. In practice this should use an estimate
# from the literature or be estimated from data
incubation_period <- LogNormal(
  meanlog = Normal(1.6, 0.06),
  sdlog = Normal(0.4, 0.07),
  max = 14
)
# set an example reporting delay. In practice this should use an estimate
# from the literature or be estimated from data
reporting_delay <- LogNormal(mean = 2, sd = 1, max = 10)

# example case data
reported_cases <- example_confirmed[1:40]

# estimate Rt and nowcast/forecast cases by date of infection
out <- epinow(
  data = reported_cases,
  generation_time = gt_opts(generation_time),
  rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.1)),
  delays = delay_opts(incubation_period + reporting_delay)
)
# summary of the latest estimates
summary(out)
# plot estimates
plot(out)

# summary of R estimates
summary(out, type = "parameters", params = "R")

options(old_opts)
}
}
\seealso{
\code{\link[=estimate_infections]{estimate_infections()}} \code{\link[=forecast_infections]{forecast_infections()}} \code{\link[=regional_epinow]{regional_epinow()}}
}
