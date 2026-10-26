test_that("growth_to_R and R_to_growth give known values", {
  # with gamma_sd = gamma_mean, k = 1 and R = 1 + r * gamma_mean
  expect_equal(growth_to_R(0.1, gamma_mean = 5, gamma_sd = 5), 1.5)
  expect_equal(R_to_growth(1.5, gamma_mean = 5, gamma_sd = 5), 0.1)
  # zero growth corresponds to R = 1
  expect_equal(growth_to_R(0, gamma_mean = 4, gamma_sd = 1), 1)
  expect_equal(R_to_growth(1, gamma_mean = 4, gamma_sd = 1), 0)
})

test_that("growth_to_R and R_to_growth are inverses", {
  r <- c(-0.1, 0, 0.05, 0.2)
  R <- growth_to_R(r, gamma_mean = 4, gamma_sd = 1)
  expect_length(R, 4)
  expect_equal(R_to_growth(R, gamma_mean = 4, gamma_sd = 1), r)
  expect_true(all(diff(R) > 0))
})

test_that("allocate_empty only adds missing parameters", {
  data <- list(a = array(1, dim = c(2, 1)))
  out <- EpiNow2:::allocate_empty(data, c("a", "b"), n = 3)
  expect_named(out, c("a", "b"))
  expect_identical(out$a, data$a)
  expect_equal(dim(out$b), c(3, 0))
  expect_equal(dim(EpiNow2:::allocate_empty(list(), "b")$b), c(0, 0))
})

test_that("lapply_func applies a function over a list", {
  expect_equal(EpiNow2:::lapply_func(1:3, sqrt), lapply(1:3, sqrt))
  expect_equal(
    EpiNow2:::lapply_func(1:3, function(x) x + 1, backend = "cmdstanr"),
    list(2, 3, 4)
  )
})

test_that("pcd_stan_id_to_distribution maps IDs to distribution names", {
  for (dist in c("lognormal", "gamma", "weibull")) {
    id <- primarycensored::pcd_stan_dist_id(dist)
    expect_equal(EpiNow2:::pcd_stan_id_to_distribution(id), dist)
  }
})

test_that("pcd_stan_id_to_distribution errors for unknown IDs", {
  expect_error(
    EpiNow2:::pcd_stan_id_to_distribution(-1L), "Unknown distribution ID"
  )
})

test_that("clean_nowcasts removes files for the given date only", {
  nowcast_dir <- withr::local_tempdir()
  for (region in c("testland", "realland")) {
    for (date in c("2020-01-01", "2020-01-02")) {
      dir <- file.path(nowcast_dir, region, date)
      dir.create(dir, recursive = TRUE)
      file.create(file.path(dir, "summary.rds"))
    }
  }
  old_threshold <- futile.logger::flog.threshold()
  futile.logger::flog.threshold("FATAL")
  withr::defer(futile.logger::flog.threshold(old_threshold))
  clean_nowcasts(date = as.Date("2020-01-01"), nowcast_dir = nowcast_dir)
  for (region in c("testland", "realland")) {
    expect_length(list.files(file.path(nowcast_dir, region, "2020-01-01")), 0)
    expect_equal(
      list.files(file.path(nowcast_dir, region, "2020-01-02")),
      "summary.rds"
    )
  }
})

test_that("get_accumulate returns the accumulate column if present", {
  data <- data.table::data.table(confirm = 1:3)
  expect_equal(EpiNow2:::get_accumulate(data), rep(FALSE, 3))
  data[, accumulate := c(TRUE, FALSE, TRUE)]
  expect_equal(EpiNow2:::get_accumulate(data), c(TRUE, FALSE, TRUE))
})
