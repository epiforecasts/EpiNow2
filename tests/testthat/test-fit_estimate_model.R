test_that("fit_estimate_model passes arguments through to fit_model", {
  seen <- new.env()
  local_mocked_bindings(
    check_truncation_length = function(stan_args, time_points) {
      seen$time_points <- time_points
      invisible()
    },
    fit_model = function(args, id) {
      seen$id <- id
      args
    }
  )
  data <- list(t = 10)
  init <- function() list()
  out <- fit_estimate_model(
    stan = stan_opts(), data = data, init = init,
    model = "estimate_secondary", time_points = 8
  )
  expect_identical(out$data, data)
  expect_identical(out$init, init)
  expect_identical(out$refresh, 0)
  expect_identical(seen$id, "estimate_secondary")
  expect_identical(seen$time_points, 8)
})

test_that("fit_estimate_model respects id and verbose", {
  seen <- new.env()
  local_mocked_bindings(
    check_truncation_length = function(stan_args, time_points) {
      seen$checked <- TRUE
      invisible()
    },
    fit_model = function(args, id) {
      seen$id <- id
      args
    }
  )
  out <- fit_estimate_model(
    stan = stan_opts(), data = list(t = 10), init = "random",
    model = "estimate_infections", id = "region", verbose = TRUE
  )
  expect_identical(out$refresh, 50)
  expect_identical(seen$id, "region")
  expect_null(seen$checked)
})

test_that("new_epinowfit returns a classed list in the given order", {
  out <- new_epinowfit(fit = 1, args = 2, observations = 3, class = "foo")
  expect_identical(names(out), c("fit", "args", "observations"))
  expect_identical(class(out), c("foo", "epinowfit", "list"))
  expect_identical(unclass(out), list(fit = 1, args = 2, observations = 3))
})
