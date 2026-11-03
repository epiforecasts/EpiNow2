test_that("rendered documentation does not contain visible nolint comments", {
  rd_files <- list.files(
    testthat::test_path("..", "..", "man"),
    pattern = "\\.Rd$", full.names = TRUE
  )
  has_nolint <- vapply(rd_files, function(rd) {
    any(grepl("nolint", readLines(rd, warn = FALSE), ignore.case = TRUE))
  }, logical(1))

  expect_equal(rd_files[has_nolint], character(0))
})
