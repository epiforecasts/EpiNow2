test_that("epinow2_cmdstan_model passes the package header as user_header", {
  skip_if_not_installed("cmdstanr")
  local_mocked_bindings(
    cmdstan_model = function(...) list(...), .package = "cmdstanr"
  )
  expect_identical(epinow2_cmdstan_model()$user_header, epinow2_stan_header())
  expect_identical(
    epinow2_cmdstan_model(user_header = "custom.hpp")$user_header,
    "custom.hpp"
  )
  expect_null(epinow2_cmdstan_model(user_header = NULL)[["user_header"]])
})
