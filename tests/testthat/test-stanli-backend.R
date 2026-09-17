skip_on_cran()

skip_if_no_stanli <- function() {
  skip_if_not_installed("stanli")
  testthat::skip_if_not(
    stanli::stanli_available(),
    "stanli runtime is not installed"
  )
}

stan_dir <- system.file("stan", package = "EpiNow2")

test_that("expand_stan_includes resolves includes as expected", {
  lines <- expand_stan_includes(
    file.path(stan_dir, "estimate_infections.stan"), stan_dir
  )
  expect_false(any(grepl("^\\s*#include", lines)))
  # content from an included file is present
  expect_true(any(grepl("convolve_with_rev_pmf", lines)))
})

test_that("expand_stan_includes errors for bad 'include' specifications", {
  tmp <- withr::local_tempfile(fileext = ".stan")
  writeLines("#include does_not_exist.stan", tmp)
  expect_error(
    expand_stan_includes(tmp, stan_dir),
    "Could not find included file"
  )
})

test_that("expand_stan_includes errors for circular includes", {
  dir <- withr::local_tempdir()
  writeLines("#include b.stan", file.path(dir, "a.stan"))
  writeLines("#include a.stan", file.path(dir, "b.stan"))
  expect_error(
    expand_stan_includes(file.path(dir, "a.stan"), dir),
    "Circular"
  )
})

test_that("expand_stan_includes handles quoted and bracketed paths", {
  dir <- withr::local_tempdir()
  writeLines("target contents", file.path(dir, "target.stan"))
  writeLines('#include "target.stan"', file.path(dir, "quoted.stan"))
  writeLines("#include <target.stan>", file.path(dir, "bracketed.stan"))
  expect_equal(
    expand_stan_includes(file.path(dir, "quoted.stan"), dir),
    "target contents"
  )
  expect_equal(
    expand_stan_includes(file.path(dir, "bracketed.stan"), dir),
    "target contents"
  )
})

test_that("stan_opts works as expected with the stanli backend", {
  skip_if_no_stanli()
  opts <- stan_opts(backend = "stanli")
  expect_equal(opts$backend, "stanli")
  # uses the cmdstanr-style argument names
  expect_true("iter_warmup" %in% names(opts))
  expect_true("iter_sampling" %in% names(opts))
  expect_true("parallel_chains" %in% names(opts))
  expect_false("iter" %in% names(opts))
})

test_that("stan_opts errors for unsupported stanli methods", {
  skip_if_no_stanli()
  for (method in c("vb", "laplace", "pathfinder")) {
    expect_error(
      stan_opts(backend = "stanli", method = method),
      "sampling"
    )
  }
})

test_that("stan_sampling_opts errors when stanli is combined with future", {
  skip_if_no_stanli()
  expect_error(
    stan_sampling_opts(backend = "stanli", future = TRUE),
    "future"
  )
})

test_that("stan_opts infers the stanli backend from a model object", {
  skip_if_no_stanli()
  model <- epinow2_stanli_model("simulate_secondary")
  opts <- stan_opts(object = model)
  expect_null(opts$backend)
  expect_s3_class(opts$object, "stanli_cstanmodel")
})

test_that("epinow2_stan_model returns a stanli model for each model", {
  skip_if_no_stanli()
  models <- c(
    "estimate_infections", "simulate_infections", "estimate_secondary",
    "simulate_secondary", "estimate_truncation", "estimate_dist", "dist_fit"
  )
  for (model in models) {
    expect_s3_class(
      epinow2_stan_model("stanli", model), "stanli_cstanmodel"
    )
  }
})

test_that("create_stan_args works as expected with the stanli backend", {
  skip_if_no_stanli()
  args <- create_stan_args(stan = stan_opts(backend = "stanli"))
  expect_s3_class(args$object, "stanli_cstanmodel")
  expect_null(args$init)
  expect_null(args$backend)
})

test_that("create_stan_args drops adaptation settings for fixed parameters", {
  skip_if_no_stanli()
  args <- create_stan_args(
    stan = stan_opts(backend = "stanli"),
    model = "simulate_infections",
    fixed_param = TRUE
  )
  expect_null(args$adapt_delta)
  expect_null(args$max_treedepth)
})
