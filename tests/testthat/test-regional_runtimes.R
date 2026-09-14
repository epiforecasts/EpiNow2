skip_on_cran()

# Uses shared fixtures from setup.R (regional_epinow run once)

futile.logger::flog.threshold("FATAL")

df_non_zero <- function(df) {
  expect_true(nrow(df) > 0)
}

test_that("regional_runtimes produces expected output when with input", {
  fixtures <- get_test_fixtures()
  out <- fixtures$regional

  runtimes <- regional_runtimes(out$regional)
  expect_equal(names(runtimes), c("region", "time"))
  df_non_zero(runtimes)
  expect_s3_class(runtimes$time, "difftime")
})
