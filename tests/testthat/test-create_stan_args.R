context("create_stan_args")


test_that("create_stan_args returns the expected defaults when the exact method is used", {

  expect_equal(names(create_stan_args()), c("object", "data", "init", "refresh", "cores", "warmup",
                                            "chains", "control", "save_warmup", "seed", "iter"))
})

test_that("create_stan_args returns the expected defaults when the approximate method is used", {
  
  expect_equal(names(create_stan_args(method = "approximate")), c("object", "data", "init", 
                                                                  "refresh", "trials", "iter", 
                                                                  "output_samples"))
})


test_that("create_stan_args can modify arugments", {
  expect_equal(create_stan_args(stan_args = list(warmup = 1000))$warmup, 1000)
})