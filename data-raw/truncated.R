library("EpiNow2")

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
  if (get_distribution(dist) == "lognormal") {
    dfunc <- dlnorm
  } else {
    dfunc <- dgamma
  }
  cmf <- cumsum(
    dfunc(
      seq_len(max(dist) + 1),
      rnorm(1,
        get_parameters(get_parameters(dist)$meanlog)$mean,
        get_parameters(get_parameters(dist)$meanlog)$sd
      ),
      rnorm(1,
        get_parameters(get_parameters(dist)$sdlog)$mean,
        get_parameters(get_parameters(dist)$sdlog)$sd
      )
    )
  )
  cmf <- cmf / cmf[max(dist) + 1]
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
trunc <- LogNormal(
  meanlog = Normal(0.9, 0.1),
  sdlog = Normal(0.6, 0.1),
  max = 10
)

# Use the make_truncated_data() function to generate example data for
# an example using estimate_truncation()
example_truncated <- purrr::map(
  seq(20, 0, -5),
  apply_truncation,
  data = reported_cases,
  dist = trunc
)

usethis::use_data(
  example_truncated,
  overwrite = TRUE,
  compress = "xz"
)
