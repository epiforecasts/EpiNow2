library("EpiNow2")
library("here")

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

# Use the make_truncated_data function to generate example data for
# an example using estimate_truncation()
example_truncation_data <- purrr::map(
  c(20, 15, 10, 0),
  make_truncated_data,
  data = reported_cases,
  dist = trunc_dist
)

usethis::use_data(
  example_truncation_data,
  overwrite = TRUE,
  compress = "xz"
)
