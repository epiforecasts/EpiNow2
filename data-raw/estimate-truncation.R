library("EpiNow2")
library("here")

#' Apply truncation to a data set
#'
#' @param index Index from which to truncate
#' @param data Data set
#' @param dist Truncation distribution
#' @importFrom stats dgamma dlnorm
#'
#' @return A truncated data set
#' @keywords internal
apply_truncation <- function(index, data, dist) {
  set.seed(index)
  if (dist$dist == 0) {
    dfunc <- dlnorm
  } else {
    dfunc <- dgamma
  }
  cmf <- cumsum(
    dfunc(
      1:(dist$max + 1),
      rnorm(1, dist$mean_mean, dist$mean_sd),
      rnorm(1, dist$sd_mean, dist$sd_sd)
    )
  )
  cmf <- cmf / cmf[dist$max + 1]
  cmf <- rev(cmf)[-1]
  trunc_data <- data.table::copy(data)[1:(.N - index)]
  trunc_data[
    (.N - length(cmf) + 1):.N, confirm := as.integer(confirm * cmf)
  ]
  return(trunc_data)
}

# get example case counts
reported_cases <- example_confirmed[1:60]

# define example truncation distribution (note not integer adjusted)
trunc_dist <- dist_spec(
  mean = convert_to_logmean(3, 2),
  mean_sd = 0.1,
  sd = convert_to_logsd(3, 2),
  sd_sd = 0.1,
  max = 10
)

# Use the make_truncated_data() function to generate example data for
# an example using estimate_truncation()
example_truncated <- purrr::map(
  seq(20, 0, -5),
  apply_truncation,
  data = reported_cases,
  dist = trunc_dist
)

saveRDS(
  example_truncation_data,
  here("inst", "extdata", "example_truncation_data.rds"),
  compress = "xz"
)
