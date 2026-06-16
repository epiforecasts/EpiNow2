test_that("GP() constructs a mean-reverting process spec", {
  gp <- GP(mean = Normal(mean = 5, sd = 1))
  expect_s3_class(gp, "process_spec")
  expect_s3_class(gp, "gp_process")
  expect_identical(gp$type, "gp")
  expect_identical(gp$anchor, "mean")
  expect_s3_class(gp$prior, "dist_spec")
  expect_s3_class(gp$settings, "gp_opts")
})

test_that("GP() constructs a first-difference process spec", {
  gp <- GP(init = Normal(mean = 5, sd = 1))
  expect_identical(gp$anchor, "init")
})

test_that("GP() forwards settings to gp_opts()", {
  gp <- GP(mean = Normal(mean = 5, sd = 1), kernel = "se")
  expect_identical(gp$settings$kernel, "se")
})

test_that("RW() constructs a process spec with a step sd prior", {
  rw <- RW(init = Normal(mean = 5, sd = 1))
  expect_s3_class(rw, "process_spec")
  expect_s3_class(rw, "rw_process")
  expect_identical(rw$type, "rw")
  expect_identical(rw$anchor, "init")
  expect_s3_class(rw$settings$sd, "dist_spec")
})

test_that("RW() accepts a custom step sd prior", {
  rw <- RW(mean = Normal(mean = 5, sd = 1), sd = Normal(mean = 0, sd = 0.05))
  expect_identical(rw$anchor, "mean")
  expect_equal(mean(rw$settings$sd), 0)
})

test_that("process constructors accept a known trajectory vector", {
  gp <- GP(mean = c(1, 2, 3, 2, 1))
  expect_true(is.numeric(gp$prior))
  expect_identical(gp$anchor, "mean")

  rw <- RW(init = c(5, 5, 5))
  expect_true(is.numeric(rw$prior))
})

test_that("process constructors require exactly one of mean/init", {
  expect_error(GP(), "Exactly one")
  expect_error(
    GP(mean = Normal(5, 1), init = Normal(5, 1)), "Exactly one"
  )
  expect_error(RW(), "Exactly one")
})

test_that("process constructors reject invalid anchors", {
  expect_error(GP(mean = "a"), "dist_spec.*numeric")
  expect_error(RW(init = list(1)), "dist_spec.*numeric")
})

test_that("RW() validates the step sd prior", {
  expect_error(RW(init = Normal(5, 1), sd = 0.1), "dist_spec")
})

test_that("is_process_spec() identifies process specs", {
  expect_true(is_process_spec(GP(mean = Normal(5, 1))))
  expect_true(is_process_spec(RW(init = Normal(5, 1))))
  expect_false(is_process_spec(Normal(5, 1)))
  expect_false(is_process_spec(5))
})

test_that("process specs print without error", {
  expect_output(print(GP(mean = Normal(5, 1))), "Gaussian process")
  expect_output(print(RW(init = Normal(5, 1))), "random walk")
  expect_output(print(GP(mean = c(1, 2, 3))), "known mean trajectory")
})

test_that("create_stan_params errors on time-varying parameters", {
  params <- list(
    make_param("fraction_observed", GP(mean = Normal(0.5, 0.1)), lower_bound = 0)
  )
  expect_error(create_stan_params(params), "not yet supported")
})
