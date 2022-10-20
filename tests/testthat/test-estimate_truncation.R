# Setup for testing -------------------------------------------------------
skip_on_cran()
futile.logger::flog.threshold("FATAL")

 # set number of cores to use
 options(old_opts)
 options(mc.cores = ifelse(interactive(), 4, 1))

 # get example case counts
 reported_cases <- example_confirmed[1:60]

 # define example truncation distribution (note not integer adjusted)
 trunc_dist <- list(
   mean = convert_to_logmean(3, 2),
   mean_sd = 0.1,
   sd = convert_to_logsd(3, 2),
   sd_sd = 0.1,
   max = 10
 )

 # apply truncation to example data
 construct_truncation <- function(index, cases, dist) {
   set.seed(index)
   cmf <- cumsum(
     dlnorm(
       1:(dist$max + 1),
       rnorm(1, dist$mean, dist$mean_sd),
       rnorm(1, dist$sd, dist$sd_sd)
     )
   )
   cmf <- cmf / cmf[dist$max + 1]
   cmf <- rev(cmf)[-1]
   trunc_cases <- data.table::copy(cases)[1:(.N - index)]
   trunc_cases[(.N - length(cmf) + 1):.N, confirm := as.integer(confirm * cmf)]
   return(trunc_cases)
 }
 example_data <- purrr::map(c(20, 15, 10, 0),
   construct_truncation,
   cases = reported_cases,
   dist = trunc_dist
 )

test_that("estimate_truncation can return values from simulated data and plot
           them", {
  # fit model to example data
  est <- estimate_truncation(example_data,
    verbose = interactive(), refresh = 0,
    chains = 2, iter = 1000, warmup = 250
  )
  expect_equal(
    names(est),
    c("dist", "obs", "last_obs", "cmf", "data", "fit")
  )
  expect_equal(
    names(est$dist),
    c("mean", "mean_sd", "sd", "sd_sd", "max")
  )
  expect_error(plot(est), NA)
})

options(old_opts)