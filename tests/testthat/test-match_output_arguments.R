test_that("match_output_arguments works as expected", {
  out <- rep(FALSE, 3)
  names(out) <- c("fit", "plots", "samples")
  expect_equal(
    EpiNow2:::match_output_arguments(supported_args = names(out)),
    out
  )
  out["plots"] <- TRUE
  expect_equal(
    EpiNow2:::match_output_arguments("plots", supported_args = names(out)), out
  )
  out["samples"] <- TRUE
  expect_equal(EpiNow2:::match_output_arguments(c("plots", "samples"),
    supported_args = names(out)
  ), out)
  expect_equal(
    EpiNow2:::match_output_arguments("p", supported_args = names(out)), out
  )
})
