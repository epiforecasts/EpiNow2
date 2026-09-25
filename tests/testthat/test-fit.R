# Setup for testing logging features
futile.logger::flog.threshold("DEBUG")

# Set up arguments for cmdstanr and rstan
# Backend is auto-detected from the object class

# Create mock objects for testing
cmdstanr_object <- structure(list(), class = "CmdStanModel")
rstan_object <- structure(list(), class = "stanmodel")

args_cmdstanr <- list(
  object = cmdstanr_object,
  iter_sampling = 1000,
  iter_warmup = 500,
  chains = 4,
  data = list()
)

args_rstan <- list(
  object = rstan_object,
  iter = 2000,
  warmup = 1000,
  chains = 2,
  data = list()
)

test_that("create_sampling_log_message works for CmdStanModel objects", {
  result <- create_sampling_log_message(args_cmdstanr, "sampling")
  expect_type(result, "character")
  expect_match(result, "exact mode")
  expect_match(result, "4000 samples")
  expect_match(result, "4 chains")
  expect_match(result, "500 iterations")
})

test_that("create_sampling_log_message works for stanmodel objects", {
  result <- create_sampling_log_message(args_rstan, "sampling")
  expect_type(result, "character")
  expect_match(result, "exact mode")
  expect_match(result, "2000 samples")
  expect_match(result, "2 chains")
  expect_match(result, "1000 iterations")
})

test_that("create_sampling_log_message includes time steps when t is present", {
  args <- args_cmdstanr
  args$data$t <- 30
  result <- create_sampling_log_message(args, "sampling")
  expect_match(result, "30 time steps")
})

test_that("create_sampling_log_message excludes time steps when t is NULL", {
  args <- args_cmdstanr
  args$data$t <- NULL
  result <- create_sampling_log_message(args, "sampling")
  expect_no_match(result, "time steps")
})

test_that("create_sampling_log_message includes forecast when both t and horizon are present", {
  args <- args_cmdstanr
  args$data$t <- 30
  args$data$horizon <- 7
  result <- create_sampling_log_message(args, "sampling")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})

test_that("create_sampling_log_message excludes forecast when horizon is NULL", {
  args <- args_cmdstanr
  args$data$t <- 30
  args$data$horizon <- NULL
  result <- create_sampling_log_message(args, "sampling")
  expect_match(result, "30 time steps")
  expect_no_match(result, "forecast")
})

test_that("create_sampling_log_message excludes forecast when t is NULL", {
  args <- args_cmdstanr
  args$data$t <- NULL
  args$data$horizon <- 7
  result <- create_sampling_log_message(args, "sampling")
  expect_no_match(result, "time steps")
  expect_no_match(result, "forecast")
})

test_that("create_sampling_log_message works for rstan with t and horizon", {
  args <- args_rstan
  args$data$t <- 30
  args$data$horizon <- 7
  result <- create_sampling_log_message(args, "sampling")
  expect_match(result, "exact mode")
  expect_match(result, "2000 samples")
  expect_match(result, "2 chains")
  expect_match(result, "1000 iterations")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})

# Tests for VB approximate mode
test_that("create_sampling_log_message works for VB method without time steps and forecast", {
  args <- list(
    iter = 10000,
    trials = 10,
    output_samples = 2000,
    data = list()
  )
  result <- create_sampling_log_message(args, "vb")
  expect_type(result, "character")
  expect_match(result, "approximate mode")
  expect_match(result, "10000 iterations")
  expect_match(result, "10 attempts")
  expect_match(result, "2000 approximate posterior samples")
})

test_that("create_sampling_log_message works for VB with time steps and forecast", {
  args <- list(
    iter = 10000,
    trials = 10,
    output_samples = 2000,
    data = list(t = 30, horizon = 7)
  )
  result <- create_sampling_log_message(args, "vb")
  expect_match(result, "approximate mode")
  expect_match(result, "10000 iterations")
  expect_match(result, "2000 approximate posterior samples")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})

# Tests for Laplace approximate mode
test_that("create_sampling_log_message works for Laplace method without time steps and forecast", {
  args <- list(
    trials = 10,
    data = list()
  )
  result <- create_sampling_log_message(args, "laplace")
  expect_type(result, "character")
  expect_match(result, "approximate mode")
  expect_match(result, "Laplace approximation")
  expect_match(result, "10 attempts")
})

test_that("create_sampling_log_message works for Laplace with time steps and forecast", {
  args <- list(
    trials = 10,
    data = list(t = 30, horizon = 7)
  )
  result <- create_sampling_log_message(args, "laplace")
  expect_match(result, "Laplace approximation")
  expect_match(result, "10 attempts")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})

# Tests for Pathfinder approximate mode
test_that("create_sampling_log_message works for Pathfinder method without time steps and forecast", {
  args <- list(
    trials = 10,
    draws = 2000,
    data = list()
  )
  result <- create_sampling_log_message(args, "pathfinder")
  expect_type(result, "character")
  expect_match(result, "approximate mode")
  expect_match(result, "pathfinder")
  expect_match(result, "10 attempts")
  expect_match(result, "2000 approximate posterior samples")
})

test_that("create_sampling_log_message works for Pathfinder with time steps and forecast", {
  args <- list(
    trials = 10,
    draws = 2000,
    data = list(t = 30, horizon = 7)
  )
  result <- create_sampling_log_message(args, "pathfinder")
  expect_match(result, "pathfinder")
  expect_match(result, "10 attempts")
  expect_match(result, "2000 approximate posterior samples")
  expect_match(result, "30 time steps")
  expect_match(result, "7 are a forecast")
})

# Backend dispatch with mocked samplers ------------------------------------

quiet_fit_logs <- function(env = parent.frame()) {
  loggers <- c("EpiNow2.epinow.estimate_infections.fit", "EpiNow2.fit")
  old <- lapply(
    loggers, function(n) futile.logger::flog.threshold(name = n)
  )
  for (n in loggers) futile.logger::flog.threshold("FATAL", name = n)
  withr::defer(
    for (i in seq_along(loggers)) {
      futile.logger::flog.threshold(old[[i]], name = loggers[[i]])
    },
    envir = env
  )
}

# A stand-in for a cmdstanr model whose methods record their arguments
fake_cmdstan_model <- function(sample = NULL, variational = NULL,
                               laplace = NULL, pathfinder = NULL) {
  structure(
    list(
      sample = sample, variational = variational,
      laplace = laplace, pathfinder = pathfinder
    ),
    class = "CmdStanModel"
  )
}

fake_mcmc <- function(...) {
  structure(list(args = list(...)), class = "CmdStanMCMC")
}

nuts_args <- function(object, chains = 2, ...) {
  c(
    list(
      object = object, method = "sampling", chains = chains,
      iter_sampling = 10, iter_warmup = 5, data = list()
    ),
    list(...)
  )
}

test_that("fit_model_with_nuts passes cleaned arguments to the sampler", {
  quiet_fit_logs()
  model <- fake_cmdstan_model(sample = fake_mcmc)
  fit <- fit_model_with_nuts(
    nuts_args(model, max_execution_time = Inf, future = FALSE),
    id = "estimate_infections"
  )
  expect_s3_class(fit, "CmdStanMCMC")
  expect_equal(fit$args$chain_id, 1:2)
  expect_setequal(
    names(fit$args),
    c("chains", "iter_sampling", "iter_warmup", "data", "chain_id")
  )
})

test_that("fit_model_with_nuts errors when sampling fails", {
  quiet_fit_logs()
  failing <- fake_cmdstan_model(sample = function(...) stop("sampler broke"))

  expect_error(
    fit_model_with_nuts(nuts_args(fake_cmdstan_model(sample = function(...) {
      NULL
    }))),
    "timed out or failed"
  )
  # Errors propagate for direct calls but are caught and logged otherwise
  expect_error(
    fit_model_with_nuts(nuts_args(failing), id = "estimate_infections"),
    "sampler broke"
  )
  expect_error(
    fit_model_with_nuts(nuts_args(failing), id = "region"),
    "timed out or failed"
  )
  expect_error(
    fit_model_with_nuts(
      nuts_args(fake_cmdstan_model(sample = fake_mcmc), stuck_chains = 1)
    ),
    "timed out or failed"
  )
})

test_that("fit_model_with_nuts muffles sampler warnings only when catching", {
  quiet_fit_logs()
  model <- fake_cmdstan_model(sample = function(...) {
    warning("divergent transitions")
    fake_mcmc(...)
  })
  expect_no_warning(fit_model_with_nuts(nuts_args(model), id = "region"))
  expect_warning(
    fit_model_with_nuts(nuts_args(model), id = "epinow"),
    "divergent transitions"
  )
})

test_that("fit_model_with_nuts treats a timed out run as a failure", {
  quiet_fit_logs()
  model <- fake_cmdstan_model(sample = function(...) {
    for (i in 1:500) Sys.sleep(0.01)
    fake_mcmc(...)
  })
  expect_error(
    fit_model_with_nuts(
      nuts_args(model), max_execution_time = 0.2, id = "estimate_infections"
    ),
    "timed out or failed"
  )
})

test_that("fit_model_with_nuts fits chains one by one with future", {
  quiet_fit_logs()
  local_mocked_bindings(
    lapply_func = function(...) lapply(...),
    sflist2stanfit = function(sflist) sflist
  )
  model <- fake_cmdstan_model(sample = fake_mcmc)
  fits <- fit_model_with_nuts(nuts_args(model, chains = 3), future = TRUE)
  expect_length(fits, 3)
  expect_equal(vapply(fits, function(f) f$args$chain_id, integer(1)), 1:3)
  expect_true(all(vapply(fits, function(f) f$args$chains, numeric(1)) == 1))
  expect_true(all(vapply(fits, function(f) f$args$cores, numeric(1)) == 1))

  # Stuck chains are dropped before combining
  fits <- fit_model_with_nuts(
    nuts_args(model, chains = 3, stuck_chains = 1), future = TRUE
  )
  expect_length(fits, 2)
})

test_that("fit_model_with_nuts with future needs at least two chains", {
  quiet_fit_logs()
  local_mocked_bindings(
    lapply_func = function(...) lapply(...),
    sflist2stanfit = function(sflist) sflist
  )
  fail_chains <- function(failed) {
    fake_cmdstan_model(sample = function(...) {
      if (list(...)$chain_id %in% failed) stop("chain failed")
      fake_mcmc(...)
    })
  }

  fits <- fit_model_with_nuts(
    nuts_args(fail_chains(2), chains = 3), future = TRUE
  )
  expect_length(fits, 2)
  expect_error(
    fit_model_with_nuts(nuts_args(fail_chains(2:3), chains = 3), future = TRUE),
    "too few chains"
  )
  expect_error(
    fit_model_with_nuts(nuts_args(fail_chains(1:3), chains = 3), future = TRUE),
    "all chains failed"
  )
})

approx_args <- function(object, method, ...) {
  c(list(object = object, method = method, data = list()), list(...))
}

test_that("fit_model_approximate dispatches to the matching cmdstanr method", {
  quiet_fit_logs()
  record <- function(name) function(...) list(method = name, args = list(...))
  model <- fake_cmdstan_model(
    variational = record("variational"),
    laplace = record("laplace"),
    pathfinder = record("pathfinder")
  )
  expected <- c(
    vb = "variational", laplace = "laplace", pathfinder = "pathfinder"
  )
  for (method in names(expected)) {
    fit <- fit_model_approximate(approx_args(model, method, trials = 2))
    expect_equal(fit$method, expected[[method]])
    expect_named(fit$args, "data")
  }
})

test_that("fit_model_approximate uses rstan::vb for rstan models", {
  quiet_fit_logs()
  local_mocked_bindings(vb = function(object, ...) {
    list(object = object, args = list(...))
  })
  model <- structure(list(), class = "stanmodel")
  fit <- fit_model_approximate(approx_args(model, "vb", iter = 100))
  expect_s3_class(fit$object, "stanmodel")
  expect_equal(fit$args, list(data = list(), iter = 100))

  for (method in c("laplace", "pathfinder")) {
    expect_error(
      fit_model_approximate(approx_args(model, method, trials = 0)),
      "only available in the cmdstanr"
    )
  }
})

test_that("fit_model_approximate retries after a failed attempt", {
  quiet_fit_logs()
  calls <- 0
  model <- fake_cmdstan_model(variational = function(...) {
    calls <<- calls + 1
    if (calls == 1) stop("first attempt failed")
    list(method = "variational")
  })
  fit <- fit_model_approximate(approx_args(model, "vb", trials = 2))
  expect_equal(fit$method, "variational")
  expect_equal(calls, 2)
})

test_that("fit_model_approximate reports why fitting failed", {
  quiet_fit_logs()
  errored_fit <- structure(
    list(
      return_codes = function() 1L,
      output = function() cat("starting\nelbo diverged\n")
    ),
    class = c("CmdStanVB", "CmdStanFit")
  )
  model <- fake_cmdstan_model(variational = function(...) errored_fit)
  expect_error(
    fit_model_approximate(approx_args(model, "vb", trials = 0)),
    "Approximate inference failed due to: elbo diverged"
  )

  # A fit without any output is also treated as a failure
  model <- fake_cmdstan_model(pathfinder = function(...) list())
  expect_error(
    fit_model_approximate(approx_args(model, "pathfinder", trials = 0)),
    "Approximate inference failed"
  )
})

test_that("fit_model dispatches on the requested method", {
  local_mocked_bindings(
    fit_model_with_nuts = function(args, future, max_execution_time, id) {
      list(fn = "nuts", future = future, time = max_execution_time, id = id)
    },
    fit_model_approximate = function(args, id) {
      list(fn = "approximate", method = args$method, id = id)
    }
  )
  fit <- fit_model(
    list(method = "sampling", future = TRUE, max_execution_time = 60),
    id = "region"
  )
  expect_equal(
    fit, list(fn = "nuts", future = TRUE, time = 60, id = "region")
  )
  for (method in c("vb", "laplace", "pathfinder")) {
    expect_equal(
      fit_model(list(method = method)),
      list(fn = "approximate", method = method, id = "stan")
    )
  }
  expect_error(fit_model(list(method = "optimize")), "unknown")
})
