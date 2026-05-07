test_stan_delays <- function(generation_time = gt_opts(Fixed(1)),
                             delays = delay_opts(),
                             truncation = trunc_opts(),
                             params = c()) {
  data <- EpiNow2:::create_stan_delays(
    generation_time = generation_time,
    delays = delays,
    truncation = truncation,
    time_points = 10
  )
  return(unlist(unname(data[params])))
}

delay_params <-
  c("delay_params_mean", "delay_params_sd", "delay_max", "delay_np_pmf")

test_that("generation times can be specified in different ways", {
  expect_equal(
    test_stan_delays(params = delay_params),
    c(0, 1, 1, 1)
  )
  expect_equal(
    test_stan_delays(
      generation_time = gt_opts(Fixed(value = 3)),
      params = delay_params
    ),
    c(0, 0, 0, 1, 1, 1)
  )
  expect_equal(
    round(test_stan_delays(
      generation_time = gt_opts(
        LogNormal(meanlog = 3, sdlog = 1, max = 4)
      ),
      params = delay_params
    ), digits = 2),
    c(0.01, 0.12, 0.34, 0.53, 1.00, 1.00)
  )
})

test_that("delay parameters can be specified in different ways", {
  expect_equal(
    tail(test_stan_delays(
      delays = delay_opts(Fixed(value = 3)),
      params = delay_params
    ), n = -2),
    c(0, 0, 0, 1, 1)
  )
  expect_equal(
    tail(round(test_stan_delays(
      delays = delay_opts(
        LogNormal(meanlog = 3, sdlog = 1, max = 4)
      ),
      params = delay_params
    ), digits = 2), n = -2),
    c(0.01, 0.12, 0.34, 0.53, 1.00)
  )
  expect_equal(
    tail(round(test_stan_delays(
      delays = suppressMessages(delay_opts(
        LogNormal(meanlog = 0.5, sdlog = 0.5)
      )),
      params = delay_params
    ), digits = 2), n = -2),
    c(0.03, 0.38, 0.37, 0.14, 0.05, 0.02, 0.01, 0.00, 0.00, 1.00)
  )
  expect_equal(
    test_stan_delays(
      delays = delay_opts(NonParametric(pmf = c(0.1, 0.6, 0.3))),
      params = delay_params
    ),
    c(0.0, 1.0, 0.1, 0.6, 0.3, 1.0)
  )
})

test_that("truncation parameters can be specified in different ways", {
  expect_equal(
    tail(round(test_stan_delays(
      truncation = trunc_opts(
        dist = LogNormal(meanlog = 3, sdlog = 1, max = 4)
      ),
      params = delay_params
    ), digits = 2), n = -2),
    c(1.00, 0.01, 0.12, 0.34, 0.53)
  )
})

test_that("distributions incompatible with stan models are caught", {
  expect_error(suppressMessages(gt_opts(
    Gamma(2, 2),
    default_cdf_cutoff = 0
  )), "maximum")
  expect_error(delay_opts(
    Normal(2, 2, max = 10)
  ), "lognormal")
})

test_that("create_stan_delays creates delay_id_* variables with correct names", {
  # Test with all delay types (infection context)
  data <- EpiNow2:::create_stan_delays(
    generation_time = gt_opts(Fixed(1)),
    reporting = delay_opts(Fixed(2)),
    truncation = trunc_opts(Fixed(1))
  )

  expect_true("delay_id_generation_time" %in% names(data))
  expect_true("delay_id_reporting" %in% names(data))
  expect_true("delay_id_truncation" %in% names(data))

  # IDs should be sequential for non-empty delays
  expect_equal(data$delay_id_generation_time, 1)
  expect_equal(data$delay_id_reporting, 2)
  expect_equal(data$delay_id_truncation, 3)
})

test_that("create_stan_delays creates delay_id_* for secondary models", {
  # Test with reporting delay for secondary models
  data <- EpiNow2:::create_stan_delays(
    reporting = delay_opts(Fixed(2)),
    truncation = trunc_opts(Fixed(1))
  )

  expect_true("delay_id_reporting" %in% names(data))
  expect_true("delay_id_truncation" %in% names(data))

  expect_equal(data$delay_id_reporting, 1)
  expect_equal(data$delay_id_truncation, 2)
})

test_that("create_stan_delays sets ID to 0 for missing delays", {
  # Test with only one delay type
  data <- EpiNow2:::create_stan_delays(
    generation_time = gt_opts(Fixed(1))
  )

  expect_equal(data$delay_id_generation_time, 1)
  # No reporting or truncation delays provided
  expect_false("delay_id_reporting" %in% names(data))
  expect_false("delay_id_truncation" %in% names(data))
})

test_that("extract_delays works with delay_id_* naming", {
  # Create mock samples with delay_params (2 samples, 2 params)
  samples <- list(
    delay_params = matrix(c(1.5, 2.0, 1.8, 2.2), nrow = 2, ncol = 2)
  )
  # Args contain the ID lookup information using existing delay variables
  # Scenario: one delay type (generation_time) with one parametric delay
  args <- list(
    delay_id_generation_time = 1,
    delay_id_reporting = 0,
    delay_types_groups = c(1, 2),    # type 1 has flat delay 1
    delay_types_p = c(1),            # flat delay 1 is parametric
    delay_types_id = c(1),           # flat delay 1 is parametric delay 1
    delay_params_groups = c(1, 3)    # parametric delay 1 has params 1-2
  )

  result <- EpiNow2:::extract_delays(samples, args = args)

  expect_true(!is.null(result))
  expect_true("variable" %in% names(result))
  expect_true("sample" %in% names(result))
  expect_true("value" %in% names(result))

  # Check that generation_time parameters are named correctly
  expect_true(any(grepl("generation_time\\[1\\]", result$variable)))
  expect_true(any(grepl("generation_time\\[2\\]", result$variable)))
})

test_that("extract_delays returns NULL when delay_params don't exist", {
  samples <- list(some_other_param = 1:10)
  args <- list()  # Empty args
  result <- EpiNow2:::extract_delays(samples, args = args)
  expect_null(result)
})

test_that("extract_delays handles delays with no ID lookup gracefully", {
  # Samples with delay_params but args without delay_id_* variables
  samples <- list(
    delay_params = matrix(c(1.5, 2.0), nrow = 2, ncol = 1)
  )
  args <- list()  # No ID lookup information

  result <- EpiNow2:::extract_delays(samples, args = args)

  expect_true(!is.null(result))
  # Should fall back to indexed naming
  expect_true(any(grepl("delay_params\\[", result$variable)))
})

test_that("build_delay_name_lookup correctly names parameters", {
  # Scenario 1: Single parametric delay (reporting with 2 params)
  args_single <- list(
    delay_id_reporting = 1,
    delay_types_groups = c(1, 2),
    delay_types_p = c(1),
    delay_types_id = c(1),
    delay_params_groups = c(1, 3)
  )
  result <- EpiNow2:::build_delay_name_lookup(args_single, n_cols = 2)
  expect_equal(result, c("reporting[1]", "reporting[2]"))

  # Scenario 2: Nonparametric followed by parametric
  # This pattern caused bug #1236: truncation is Fixed(0), reporting is LogNormal
  args_nonparam_first <- list(
    delay_id_truncation = 1,
    delay_id_reporting = 2,
    delay_types_groups = c(1, 2, 3),
    delay_types_p = c(0, 1),
    delay_types_id = c(1, 1),
    delay_params_groups = c(1, 3)
  )
  result <- EpiNow2:::build_delay_name_lookup(args_nonparam_first, n_cols = 2)
  # Should be reporting, NOT truncation
  expect_equal(result, c("reporting[1]", "reporting[2]"))

  # Scenario 3: Two parametric delays (reporting then truncation)
  args_both_param <- list(
    delay_id_reporting = 1,
    delay_id_truncation = 2,
    delay_types_groups = c(1, 2, 3),
    delay_types_p = c(1, 1),
    delay_types_id = c(1, 2),
    delay_params_groups = c(1, 3, 5)
  )
  result <- EpiNow2:::build_delay_name_lookup(args_both_param, n_cols = 4)
  expect_equal(
    result,
    c("reporting[1]", "reporting[2]", "truncation[1]", "truncation[2]")
  )

  # Scenario 4: Parametric followed by nonparametric
  args_param_first <- list(
    delay_id_reporting = 1,
    delay_id_truncation = 2,
    delay_types_groups = c(1, 2, 3),
    delay_types_p = c(1, 0),
    delay_types_id = c(1, 1),
    delay_params_groups = c(1, 3)
  )
  result <- EpiNow2:::build_delay_name_lookup(args_param_first, n_cols = 2)
  expect_equal(result, c("reporting[1]", "reporting[2]"))

  # Scenario 5: Three delay types with mixed parametric/nonparametric
  # generation_time (parametric), reporting (nonparametric), truncation (param)
  args_three_mixed <- list(
    delay_id_generation_time = 1,
    delay_id_reporting = 2,
    delay_id_truncation = 3,
    delay_types_groups = c(1, 2, 3, 4),
    delay_types_p = c(1, 0, 1),
    delay_types_id = c(1, 1, 2),
    delay_params_groups = c(1, 3, 5)
  )
  result <- EpiNow2:::build_delay_name_lookup(args_three_mixed, n_cols = 4)
  expect_equal(
    result,
    c("generation_time[1]", "generation_time[2]",
      "truncation[1]", "truncation[2]")
  )
})

test_that("create_stan_delays works with fixed NonParametric", {
  data <- EpiNow2:::create_stan_delays(
    delays = delay_opts(
      dist = NonParametric(c(0.1, 0.6, 0.3))
    )
  )
  expect_equal(data$delay_n_np, 1L)
  expect_equal(as.numeric(data$delay_np_pmf), c(0.1, 0.6, 0.3))
  expect_equal(data$delay_n_np_est, 0L)
  expect_equal(data$delay_np_est_length, 0L)
  expect_equal(as.numeric(data$delay_np_est_alpha), numeric(0))
  expect_equal(as.integer(data$delay_np_est_pos), integer(0))
  expect_equal(as.integer(data$delay_np_est_groups), 1L)
})

test_that("create_stan_delays handles Dirichlet prior", {
  pmf <- c(0.1, 0.5, 0.3, 0.1)
  conc <- 2
  data <- EpiNow2:::create_stan_delays(
    delays = delay_opts(
      dist = NonParametric(Dirichlet(prior = pmf, concentration = conc))
    )
  )
  expect_equal(data$delay_n_np_est, 1L)
  expect_equal(data$delay_np_est_length, 4L)
  expect_equal(
    as.numeric(data$delay_np_est_alpha),
    conc * pmf
  )
  expect_equal(as.integer(data$delay_np_est_pos), 1L:4L)
  expect_equal(as.integer(data$delay_np_est_which), 1L)
  expect_equal(
    as.integer(data$delay_np_est_groups), c(1L, 5L)
  )
  # Prior PMF still present in np_pmf
  expect_equal(as.numeric(data$delay_np_pmf), pmf)
})

test_that("create_stan_delays skips zero alpha entries", {
  pmf <- c(0, 0.3, 0.5, 0.2)
  conc <- 10
  data <- EpiNow2:::create_stan_delays(
    generation_time = gt_opts(
      dist = NonParametric(Dirichlet(prior = pmf, concentration = conc))
    )
  )
  # Only 3 positive entries estimated (zero at t=0 excluded)
  expect_equal(data$delay_np_est_length, 3L)
  expect_equal(
    as.numeric(data$delay_np_est_alpha),
    conc * pmf[pmf > 0]
  )
  # Positions point to indices 2, 3, 4 in delay_np_pmf
  expect_equal(as.integer(data$delay_np_est_pos), 2L:4L)
  expect_equal(
    as.integer(data$delay_np_est_groups), c(1L, 4L)
  )
  # Full PMF still in np_pmf including the zero
  expect_equal(as.numeric(data$delay_np_pmf), pmf)
})

test_that("create_stan_delays with no NP delays gives safe defaults", {
  data <- EpiNow2:::create_stan_delays(
    generation_time = gt_opts(Fixed(1))
  )
  expect_equal(data$delay_n_np_est, 0L)
  expect_equal(data$delay_np_est_length, 0L)
  expect_equal(as.numeric(data$delay_np_est_alpha), numeric(0))
  expect_equal(as.integer(data$delay_np_est_pos), integer(0))
  expect_equal(as.integer(data$delay_np_est_groups), 1L)
})

test_that("create_stan_delays handles mixed fixed and Dirichlet NP", {
  fixed_pmf <- c(0.0, 0.5, 0.5)
  est_pmf <- c(0.1, 0.4, 0.4, 0.1)
  conc <- 5
  data <- EpiNow2:::create_stan_delays(
    generation_time = gt_opts(
      dist = NonParametric(fixed_pmf)
    ),
    delays = delay_opts(
      dist = NonParametric(Dirichlet(prior = est_pmf, concentration = conc))
    )
  )
  # Two NP delays total
  expect_equal(data$delay_n_np, 2L)
  # Only one is estimated
  expect_equal(data$delay_n_np_est, 1L)
  # The estimated one is the second NP delay
  expect_equal(as.integer(data$delay_np_est_which), 2L)
  expect_equal(
    as.numeric(data$delay_np_est_alpha),
    conc * est_pmf
  )
  # Positions offset by the fixed PMF (3 elements)
  expect_equal(as.integer(data$delay_np_est_pos), 4L:7L)
  expect_equal(data$delay_np_est_length, 4L)
  expect_equal(
    as.integer(data$delay_np_est_groups), c(1L, 5L)
  )
  # Both PMFs in np_pmf (generation_time first, then delays)
  expect_equal(
    as.numeric(data$delay_np_pmf),
    c(fixed_pmf, est_pmf)
  )
})

test_that(
  "reconstruct_nonparametric returns NonParametric for fixed NP",
  {
    stan_data <- list(
      delay_np_pmf = c(0.2, 0.5, 0.3),
      delay_np_pmf_groups = c(1L, 4L),
      delay_np_est_which = integer(0),
      delay_np_est_groups = 1L,
      delay_np_est_alpha = numeric(0)
    )
    result <- EpiNow2:::reconstruct_nonparametric(
      stan_data, 1L
    )
    expect_s3_class(result, "dist_spec")
    expect_false(isTRUE(result$estimated))
    expect_equal(
      as.numeric(get_pmf(result)),
      c(0.2, 0.5, 0.3)
    )
  }
)

test_that(
  "reconstruct_nonparametric returns NonParametric(pmf = Dirichlet)",
  {
    pmf <- c(0.1, 0.4, 0.4, 0.1)
    conc <- 3
    alpha <- conc * pmf
    stan_data <- list(
      delay_np_pmf = pmf,
      delay_np_pmf_groups = c(1L, 5L),
      delay_np_est_which = 1L,
      delay_np_est_groups = c(1L, 5L),
      delay_np_est_alpha = alpha,
      delay_np_est_pos = 1L:4L
    )
    result <- EpiNow2:::reconstruct_nonparametric(
      stan_data, 1L
    )
    expect_true(isTRUE(result$estimated))
    expect_equal(as.numeric(result$alpha), alpha)
    expect_equal(as.numeric(get_pmf(result)), pmf)
  }
)

test_that(
  "reconstruct_nonparametric handles mixed fixed and estimated",
  {
    fixed_pmf <- c(0.0, 0.5, 0.5)
    est_pmf <- c(0.1, 0.4, 0.4, 0.1)
    conc <- 5
    alpha <- conc * est_pmf
    stan_data <- list(
      delay_np_pmf = c(fixed_pmf, est_pmf),
      delay_np_pmf_groups = c(1L, 4L, 8L),
      delay_np_est_which = 2L,
      delay_np_est_groups = c(1L, 5L),
      delay_np_est_alpha = alpha,
      delay_np_est_pos = 4L:7L
    )
    # First NP delay is fixed
    fixed_result <- EpiNow2:::reconstruct_nonparametric(
      stan_data, 1L
    )
    expect_false(isTRUE(fixed_result$estimated))
    expect_equal(
      as.numeric(get_pmf(fixed_result)),
      fixed_pmf
    )
    # Second NP delay is estimated
    est_result <- EpiNow2:::reconstruct_nonparametric(
      stan_data, 2L
    )
    expect_true(isTRUE(est_result$estimated))
    expect_equal(as.numeric(est_result$alpha), alpha)
  }
)


test_that(
  "reconstruct_nonparametric uses posterior when available",
  {
    pmf <- c(0, 0.3, 0.5, 0.2)
    conc <- 10
    alpha <- conc * pmf[pmf > 0]
    stan_data <- list(
      delay_np_pmf = pmf,
      delay_np_pmf_groups = c(1L, 5L),
      delay_np_est_which = 1L,
      delay_np_est_groups = c(1L, 4L),
      delay_np_est_alpha = alpha,
      delay_np_est_pos = 2L:4L
    )
    # Simulate posterior draws (3 draws, 3 estimated bins)
    np_posterior <- matrix(
      c(3, 5, 2, 2.5, 5.5, 2, 3.5, 4.5, 2),
      nrow = 3, byrow = TRUE
    )
    result <- EpiNow2:::reconstruct_nonparametric(
      stan_data, 1L, np_posterior
    )
    # Should round-trip as a NonParametric backed by a Dirichlet
    expect_true(isTRUE(result$estimated))
    post_alpha <- result$alpha
    # Structural zero is preserved
    expect_equal(post_alpha[1], 0)
    # Free alphas are positive
    expect_true(all(post_alpha[-1] > 0))
    # Implied mean PMF is a valid simplex
    post_pmf <- as.numeric(get_pmf(result))
    expect_equal(post_pmf[1], 0)
    expect_equal(sum(post_pmf), 1, tolerance = 1e-10)
    expect_true(all(post_pmf >= 0))
  }
)

test_that(
  "reconstruct_nonparametric recovers known Dirichlet via moment matching",
  {
    ## Draw from a known Dirichlet via the gamma trick (matches the
    ## Stan parameterisation), feed the unnormalised draws into
    ## reconstruct_nonparametric, and check the moment-matched alpha
    ## is close to the truth.
    set.seed(42)
    true_alpha <- c(8, 24, 40, 16)
    n_draws <- 10000L
    raw_draws <- matrix(
      rgamma(n_draws * length(true_alpha),
             shape = rep(true_alpha, each = n_draws), rate = 1),
      nrow = n_draws
    )
    pmf <- c(0, true_alpha / sum(true_alpha))
    stan_data <- list(
      delay_np_pmf = pmf,
      delay_np_pmf_groups = c(1L, length(pmf) + 1L),
      delay_np_est_which = 1L,
      delay_np_est_groups = c(1L, length(true_alpha) + 1L),
      delay_np_est_alpha = true_alpha,
      delay_np_est_pos = seq.int(2L, length(pmf))
    )
    result <- EpiNow2:::reconstruct_nonparametric(
      stan_data, 1L, raw_draws
    )
    recovered <- result$alpha[-1] # drop structural zero at t = 0
    ## per-bin alphas within a few percent of truth at this sample
    ## size; concentration recovered tightly
    expect_equal(recovered, true_alpha, tolerance = 0.1)
    expect_equal(sum(recovered), sum(true_alpha), tolerance = 0.1)
  }
)

test_that("build_np_est_data produces expected output", {
  np_delays <- list(
    NonParametric(Dirichlet(prior = c(0.1, 0.5, 0.4), concentration = 10))
  )
  out <- EpiNow2:::build_np_est_data(np_delays, np_pmf_groups = c(1L, 4L))
  expect_equal(out$n_np_est, 1L)
  expect_equal(out$np_est_length, 3L)
  expect_equal(as.integer(out$np_est_which), 1L)
  expect_equal(as.numeric(out$np_est_alpha), 10 * c(0.1, 0.5, 0.4))
  expect_equal(as.integer(out$np_est_pos), 1L:3L)
  expect_equal(as.integer(out$np_est_groups), c(1L, 4L))
})

test_that("build_np_est_data drops structural zeros", {
  np_delays <- list(
    NonParametric(Dirichlet(prior = c(0, 0.3, 0.5, 0.2), concentration = 10))
  )
  out <- EpiNow2:::build_np_est_data(np_delays, np_pmf_groups = c(1L, 5L))
  # Structural zero excluded from the parameter vector
  expect_equal(out$np_est_length, 3L)
  expect_equal(as.numeric(out$np_est_alpha), 10 * c(0.3, 0.5, 0.2))
  # Positions skip the zero entry but stay 1-indexed in delay_np_pmf
  expect_equal(as.integer(out$np_est_pos), 2L:4L)
  expect_equal(as.integer(out$np_est_groups), c(1L, 4L))
})

test_that("build_np_est_data correctly handles mixed fixed and estimated", {
  np_delays <- list(
    NonParametric(c(0.5, 0.5)),
    NonParametric(Dirichlet(prior = c(0.2, 0.8), concentration = 5))
  )
  ## fixed PMF takes positions 1-2, estimated PMF takes 3-4
  out <- EpiNow2:::build_np_est_data(np_delays, np_pmf_groups = c(1L, 3L, 5L))
  expect_equal(out$n_np_est, 1L)
  expect_equal(as.integer(out$np_est_which), 2L)
  expect_equal(as.integer(out$np_est_pos), 3L:4L)
  expect_equal(as.numeric(out$np_est_alpha), 5 * c(0.2, 0.8))
  expect_equal(as.integer(out$np_est_groups), c(1L, 3L))
})

test_that("build_np_est_data returns empty defaults when none estimated", {
  out <- EpiNow2:::build_np_est_data(
    list(NonParametric(c(0.4, 0.6))), np_pmf_groups = c(1L, 3L)
  )
  expect_equal(out$n_np_est, 0L)
  expect_equal(out$np_est_length, 0L)
  expect_equal(as.numeric(out$np_est_alpha), numeric(0))
  expect_equal(as.integer(out$np_est_pos), integer(0))
  expect_equal(as.integer(out$np_est_which), integer(0))
  expect_equal(as.integer(out$np_est_groups), 1L)
})

test_that("build_np_est_data returns empty defaults for empty input", {
  out <- EpiNow2:::build_np_est_data(list(), np_pmf_groups = 1L)
  expect_equal(out$n_np_est, 0L)
  expect_equal(out$np_est_length, 0L)
})
