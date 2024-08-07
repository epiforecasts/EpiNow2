% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opts.R
\name{gp_opts}
\alias{gp_opts}
\title{Approximate Gaussian Process Settings}
\usage{
gp_opts(
  basis_prop = 0.2,
  boundary_scale = 1.5,
  ls_mean = 21,
  ls_sd = 7,
  ls_min = 0,
  ls_max = 60,
  alpha_sd = 0.05,
  kernel = c("matern", "se", "ou"),
  matern_order = 3/2,
  matern_type
)
}
\arguments{
\item{basis_prop}{Numeric, proportion of time points to use as basis
functions. Defaults to 0.2. Decreasing this value results in a decrease in
accuracy but a faster compute time (with increasing it having the first
effect). In general smaller posterior length scales require a higher
proportion of basis functions. See (Riutort-Mayol et al. 2020
\url{https://arxiv.org/abs/2004.11408}) for advice on updating this default.}

\item{boundary_scale}{Numeric, defaults to 1.5. Boundary scale of the
approximate Gaussian process. See (Riutort-Mayol et al. 2020
\url{https://arxiv.org/abs/2004.11408}) for advice on updating this default.}

\item{ls_mean}{Numeric, defaults to 21 days. The mean of the lognormal
length scale.}

\item{ls_sd}{Numeric, defaults to 7 days. The standard deviation of the log
normal length scale. If \code{ls_sd = 0}, inverse-gamma prior on Gaussian
process length scale will be used with recommended parameters
\code{inv_gamma(1.499007, 0.057277 * ls_max)}.}

\item{ls_min}{Numeric, defaults to 0. The minimum value of the length scale.}

\item{ls_max}{Numeric, defaults to 60. The maximum value of the length
scale. Updated in \code{\link[=create_gp_data]{create_gp_data()}} to be the length of the input data if
this is smaller.}

\item{alpha_sd}{Numeric, defaults to 0.05. The standard deviation of the
magnitude parameter of the Gaussian process kernel. Should be approximately
the expected standard deviation of the logged Rt.}

\item{kernel}{Character string, the type of kernel required. Currently
supporting the squared exponential kernel ("se", or "matern" with
'matern_order = Inf'), 3 over 2 oder 5 over 2 Matern kernel ("matern", with
\code{matern_order = 3/2} (default) or \code{matern_order = 5/2}, respectively), or
Orstein-Uhlenbeck ("ou", or "matern" with 'matern_order = 1/2'). Defaulting
to the Matérn 3 over 2 kernel for a balance of smoothness and
discontinuities.}

\item{matern_order}{Numeric, defaults to 3/2. Order of Matérn Kernel to use.
Currently the orders 1/2, 3/2, 5/2 and Inf are supported.}

\item{matern_type}{Deprated; Numeric, defaults to 3/2. Order of Matérn Kernel
to use.  Currently the orders 1/2, 3/2, 5/2 and Inf are supported.}
}
\value{
A \verb{<gp_opts>} object of settings defining the Gaussian process
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
Defines a list specifying the structure of the approximate Gaussian
process. Custom settings can be supplied which override the defaults.
}
\examples{
# default settings
gp_opts()

# add a custom length scale
gp_opts(ls_mean = 4)
}
