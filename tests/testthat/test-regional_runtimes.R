futile.logger::flog.threshold("FATAL")

df_non_zero <- function(df) {
  expect_true(nrow(df) > 0)
}

# Load pre-computed regional_epinow output instead of running MCMC
out <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_regional_epinow.rds"
))

test_that("regional_runtimes produces expected output when with input", {
  skip_on_cran()
  runtimes <- regional_runtimes(out$regional)
  expect_equal(names(runtimes), c("region", "time"))
  df_non_zero(runtimes)
  expect_s3_class(runtimes$time, "difftime")
})
