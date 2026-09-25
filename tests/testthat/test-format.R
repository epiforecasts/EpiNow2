skip_on_cran()

# Hand-built posterior draws in the layout returned by extract_samples():
# one row per sample and one column per time point.
draws <- function(n_time, offset = 0, n_samples = 2) {
  matrix(
    offset + seq_len(n_samples * n_time),
    nrow = n_samples, ncol = n_time, byrow = TRUE
  )
}

obs <- data.table(date = as.Date("2020-01-01") + 0:3)
base_args <- list(
  horizon = 0, seeding_time = 2, imputed_times = 1:4,
  use_pop = 0, bp_n = 0, week_effect = 1
)

test_that("format_fit labels dates by horizon and adds missing strata", {
  dates <- as.Date("2020-01-01") + 0:5
  posterior <- list(
    R = data.table(
      time = rep(1:6, 2), date = rep(dates, 2),
      sample = rep(1:2, each = 6), value = rep(1:6, 2)
    )
  )

  out <- format_fit(posterior, horizon = 2, shift = 1, CrIs = 0.5)

  expect_named(out, c("samples", "summarised"))
  expect_true(all(is.na(out$samples$strat)))
  expect_equal(nrow(out$summarised), 6)
  expect_equal(
    out$summarised$type,
    c(rep("estimate", 3), "estimate based on partial data", rep("forecast", 2))
  )
  expect_equal(out$summarised$median, 1:6)
  expect_true(all(c("lower_50", "upper_50") %in% colnames(out$summarised)))
})

test_that("format_samples_with_dates uses gen_R when R is not sampled", {
  raw_samples <- list(
    infections = draws(6),
    imputed_reports = draws(4),
    gen_R = draws(4, offset = 100),
    r = draws(3)
  )

  out <- format_samples_with_dates(raw_samples, base_args, obs)

  expect_setequal(
    unique(out$variable),
    c("reported_cases", "R", "infections", "growth_rate")
  )
  expect_true(all(is.na(out$strat)))
  expect_equal(out[variable == "R"]$date, rep(obs$date, times = 2))
  expect_equal(out[variable == "R"]$value, 101:108)
  # infections are trimmed to the reported dates, dropping the seeding time
  expect_equal(unique(out[variable == "infections"]$date), obs$date)
  expect_equal(unique(out[variable == "growth_rate"]$date), obs$date[-1])
})

test_that("format_samples_with_dates reports adjusted Rt with population", {
  raw_samples <- list(
    infections = draws(6),
    imputed_reports = draws(4),
    R = draws(4),
    R_adj = draws(4, offset = 100),
    r = draws(3)
  )
  args <- modifyList(base_args, list(use_pop = 1))

  out <- format_samples_with_dates(raw_samples, args, obs)

  expect_true("R_unadjusted" %in% out$variable)
  expect_equal(out[variable == "R"]$value, 101:108)
  expect_equal(out[variable == "R_unadjusted"]$value, 1:8)
})
