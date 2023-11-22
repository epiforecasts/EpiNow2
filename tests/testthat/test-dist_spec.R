
test_that("dist_spec returns correct output for fixed lognormal distribution", {
  result <- lognormal(meanlog = 5, sdlog = 1, max = 19)
  expect_equal(dim(result$params_mean), 0)
  expect_equal(dim(result$dist), 0)
  expect_equal(dim(result$max), 0)
  expect_equal(result$parametric, array(FALSE))
  expect_equal(
    as.vector(round(result$np_pmf, 2)),
    c(0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.02, 0.03,
      0.03, 0.04, 0.05, 0.06, 0.07, 0.07, 0.08, 0.09,
      0.10, 0.10, 0.11, 0.12)
  )
})

test_that("dist_spec returns correct output for uncertain gamma distribution", {
  result <- gamma(shape = normal(3, 0.5), rate = normal(2, 0.5), max = 19)
  expect_equal(result$params_mean, array(c(3, 2)))
  expect_equal(result$params_sd, array(c(0.5, 0.5)))
  expect_equal(result$dist, array("gamma"))
  expect_equal(result$max, array(19))
  expect_equal(result$parametric, array(TRUE))
})

test_that("dist_spec returns correct output for fixed distribution", {
  result <- fix_dist(lognormal(meanlog = normal(5, 3), sdlog = 1, max = 19))
  expect_equal(dim(result$params_mean), 0)
  expect_equal(result$parametric, array(FALSE))
  expect_equal(
    as.vector(round(result$np_pmf, 2)),
    c(0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.02, 0.03,
      0.03, 0.04, 0.05, 0.06, 0.07, 0.07, 0.08, 0.09,
      0.10, 0.10, 0.11, 0.12)
  )
})

test_that("dist_spec returns error when mixed natural and unnatural parameters are specified", {
  expect_error(
    lognormal(meanlog = 5, sd = 1, max = 20),
    "Incompatible combination."
  )
})

test_that("dist_spec returns error when the wrong number of paramters are given", {
  expect_error(lognormal(sd = 1, max = 20), "must be specified")
  expect_error(gamma(shape = 1, rate = 2, mean = 3), "must be specified")
})

test_that("+.dist_spec returns correct output for sum of two distributions", {
  dist1 <- lognormal(meanlog = 5, sdlog = 1, max = 19)
  dist2 <- gamma(shape = normal(3, 0.5), rate = normal(2, 0.5), max = 20)
  result <- dist1 + dist2
  expect_equal(result$params_mean, array(c(3, 2)))
  expect_equal(result$params_sd, array(c(0.5, 0.5)))
  expect_equal(result$n, 2)
  expect_equal(result$n_p, 1)
  expect_equal(result$n_np, 1)
  expect_equal(result$np_pmf_length, 20)
})

test_that("+.dist_spec returns correct output for sum of two fixed distributions", {
  dist1 <- lognormal(meanlog = 5, sdlog = 1, max = 19)
  dist2 <- gamma(mean = 3, sd = 2, max = 19)
  result <- dist1 + dist2
  expect_equal(dim(result$params_mean), 0)
  expect_equal(result$n, 1)
  expect_equal(result$n_p, 0)
  expect_equal(result$n_np, 1)
  expect_equal(result$np_pmf_length, 30)
})

test_that("+.dist_spec returns correct output for sum of two nonparametric distributions", {
  dist1 <- pmf(c(0.1, 0.2, 0.3, 0.4))
  dist2 <- pmf(c(0.1, 0.2, 0.3, 0.4))
  result <- dist1 + dist2
  expect_equal(dim(result$params_mean), 0)
  expect_equal(result$n, 1)
  expect_equal(result$n_p, 0)
  expect_equal(result$n_np, 1)
  expect_equal(result$np_pmf_length, 7)
  expect_equal(
    as.vector(round(result$np_pmf, 2)),
    c(0.01, 0.04, 0.10, 0.20, 0.25, 0.24, 0.16)
  )
})

test_that("Testing `+.dist_spec` function with tolerance parameter", {
  # Create distributions
  dist1 <- lognormal(meanlog = 1.6, sdlog = 1, max = 19)
  dist2 <- gamma(mean = 3, sd = 2, max = 19)

  # Compute combined distribution with default tolerance
  combined_default <- EpiNow2:::`+.dist_spec`(dist1, dist2)
  
  # Compute combined distribution with larger tolerance
  combined_larger_tolerance <- EpiNow2:::dist_spec_plus(
    dist1, dist2, tolerance = 0.01
  )

  # The length of the combined PMF should be greater with default tolerance
  expect_true(
    length(combined_default$np_pmf) > length(combined_larger_tolerance$np_pmf)
  )
  # Both should sum to 1
  expect_equal(sum(combined_default$np_pmf), 1)
  expect_equal(sum(combined_larger_tolerance$np_pmf), 1)
  # The first 5 entries should be within 0.01 of each other
  expect_equal(
    combined_default$np_pmf[1:5], combined_larger_tolerance$np_pmf[1:5],
    tolerance = 0.01
  )
  expect_equal(
    mean(combined_default), mean(combined_larger_tolerance), tolerance = 0.1
  )
})

test_that("summary functions return correct output for fixed lognormal distribution", {
  dist <- lognormal(mean = 3, sd = 1, max = 19)
  ## here we can see the bias from
  # using this kind of discretisation approach
  expect_equal(EpiNow2:::mean.dist_spec(dist), 2.49, tolerance = 0.01)
  expect_equal(EpiNow2:::sd.dist_spec(dist), 1.09, tolerance = 0.01)
  expect_equal(EpiNow2:::max.dist_spec(dist), 19L)
})

test_that("summary functions return correct output for uncertain gamma distribution", {
  dist <- gamma(shape = normal(3, 0.5), rate = normal(2, 0.5), max = 19)
  expect_equal(EpiNow2:::mean.dist_spec(dist), 1.5)
  expect_equal(EpiNow2:::max.dist_spec(dist), 19L)
})

test_that("mean.dist_spec returns correct output for sum of two distributions", {
  dist1 <- lognormal(meanlog = 1, sdlog = 1, max = 19)
  dist2 <- gamma(mean = 3, sd = 2, max = 19)
  dist <- dist1 + dist2
  expect_equal(EpiNow2:::mean.dist_spec(dist), c(5.84), tolerance = 0.001)
  expect_equal(EpiNow2:::sd.dist_spec(dist), c(16.88), tolerance = 0.001)
  ## shortened due to tolerance level
  expect_equal(EpiNow2:::max.dist_spec(dist), 24L)
})

test_that("sd.dist_spec returns an error when applied to uncertain distributions", {
  dist <- gamma(shape = normal(3, 0.5), rate = normal(2, 0.5), max = 19)
  expect_error(EpiNow2:::sd.dist_spec(dist), "uncertain")
})

test_that("print.dist_spec correctly prints the parameters of the fixed lognormal", {
  dist <- lognormal(meanlog = 1.5, sdlog = 0.5, max = 19)
  
  expect_output(print(dist), "\\n  distribution with PMF \\[0\\.0014 0\\.052 0\\.16 0\\.2 0\\.18 0\\.13 0\\.094 0\\.063 0\\.042 0\\.027 0\\.018 0\\.012 0\\.0079 0\\.0052 0\\.0035 0\\.0024 0\\.0016 0\\.0011 0\\.00078 0\\.00055\\]\\.\\n")
})

test_that("print.dist_spec correctly prints the parameters of the uncertain gamma", {
  gamma <- gamma(
    shape = normal(3, 0.5), rate = normal(2, 0.5), max = 19
  )
  
  expect_output(print(gamma), "\\n  gamma distribution \\(max: 19\\) with uncertain shape \\(mean = 3, sd = 0.5\\) and uncertain rate \\(mean = 2, sd = 0\\.5\\)\\.\\n")
})

test_that("print.dist_spec correctly prints the parameters of the uncertain lognormal", {
  dist <- lognormal(
    meanlog = normal(1.5, 0.1), sdlog = normal(0.5, 0.1), max = 19
  )
  
  expect_output(print(dist), "\\n  lognormal distribution \\(max: 19\\) with uncertain meanlog \\(mean = 1\\.5, sd = 0\\.1\\) and uncertain sdlog \\(mean = 0\\.5, sd = 0\\.1\\)\\.\\n")
})

test_that("print.dist_spec correctly prints the parameters of a combination of distributions", {
  dist1 <- lognormal(meanlog = 1.5, sdlog = 0.5, max = 19)
  dist2 <- gamma(shape =  normal(3, 0.5), rate = normal(2, 0.5), max = 19)
  combined <- dist1 + dist2
  
  expect_output(print(combined), "\\nComposite delay distribution:\\n  distribution with PMF \\[0\\.0014 0\\.052 0\\.16 0\\.2 0\\.18 0\\.13 0\\.094 0\\.063 0\\.042 0\\.027 0\\.018 0\\.012 0\\.0079 0\\.0052 0\\.0035 0\\.0024 0\\.0016 0\\.0011 0\\.00078 0\\.00055\\]\\.\\n  gamma distribution \\(max: 19\\) with uncertain shape \\(mean = 3, sd = 0\\.5\\) and uncertain rate \\(mean = 2, sd = 0\\.5\\)\\.\\n")
})

test_that("plot.dist_spec returns a ggplot object", {
  dist <- lognormal(meanlog = 1.6, sdlog = 0.5, max = 19)
  plot <- plot(dist)
  expect_s3_class(plot, "ggplot")
})

test_that("plot.dist_spec correctly plots a single distribution", {
  dist <- lognormal(meanlog = 1.6, sdlog = 0.5, max = 19)
  plot <- plot(dist)
  expect_equal(length(plot$layers), 2)
  expect_equal(length(plot$facet$params$facets), 1)
})

test_that("plot.dist_spec correctly plots multiple distributions", {
  dist1 <- lognormal(meanlog = 1.6, sdlog = 0.5, max = 19)
  dist2 <- gamma(shape = normal(3, 5), rate = normal(1, 2), max = 19)
  combined <- dist1 + dist2
  plot <- plot(combined)
  expect_equal(length(plot$layers), 2)
  expect_equal(length(plot$facet$params$facets), 1)
})

test_that("plot.dist_spec correctly plots a combination of fixed distributions", {
  dist <- lognormal(meanlog = 1.6, sdlog = 0.5, max = 19)
  combined <- dist + dist
  plot <- plot(combined)
  expect_equal(length(plot$layers), 2)
  expect_equal(length(plot$facet$params$facets), 1)
})

test_that("fix_dist works with composite delay distributions", {
  dist1 <- lognormal(meanlog = normal(1, 0.1), sdlog = 1, max = 19)
  dist2 <- gamma(mean = 3, sd = 2, max = 19)
  dist <- dist1 + dist2
  expect_equal(fix_dist(dist)$n, 1L)
})

test_that("composite delay distriubtions can be disassembled", {
  dist1 <- lognormal(meanlog = normal(1, 0.1), sdlog = 1, max = 19)
  dist2 <- gamma(mean = 3, sd = 2, max = 19)
  dist <- dist1 + dist2
  expect_equal(EpiNow2:::extract_single_dist(dist, 1), dist1)
  expect_equal(EpiNow2:::extract_single_dist(dist, 2), dist2)
})

test_that("delay distributions can be specified in different ways", {
  expect_equal(
    lognormal(mean = 4, sd = 1)$params_mean, array(c(1.4, 0.25)),
    tolerance = 0.1
  )
  expect_equal(
    round(lognormal(mean = 4, sd = 1, max = 10)$np_pmf, 2),
    array(c(0.00, 0.00, 0.14, 0.40, 0.30, 0.11, 0.03, 0.01, 0.00, 0.00, 0.00))
  )
  expect_equal(
    gamma(mean = 4, sd = 1)$params_mean, array(c(16, 4)),
    tolerance = 0.1
  )
  expect_equal(
    round(gamma(mean = 4, sd = 1, max = 7)$np_pmf, 2),
    array(c(0.00, 0.01, 0.15, 0.38, 0.31, 0.12, 0.03, 0.00))
  )
  expect_equal(
    gamma(shape = normal(16, 2), rate = normal(4, 1))$params_mean,
    array(c(16, 4))
  )
  expect_equal(
    gamma(shape = normal(16, 2), rate = normal(4, 1))$params_sd,
    array(c(2, 1))
  )
  expect_equal(
    normal(mean = 4, sd = 1)$params_mean, array(c(4, 1))
  )
  expect_equal(
    round(normal(mean = 4, sd = 1, max = 5)$np_pmf, 2),
    array(c(0.00, 0.02, 0.14, 0.35, 0.35, 0.14))
  )
  expect_equal(
    fixed(value = 3)$np_pmf, array(c(0, 0, 0, 1))
  )
  expect_equal(fixed(value = 3.5)$params_mean, array(3.5))
  expect_equal(pmf(c(0.1, 0.3, 0.2, 0.4))$np_pmf, array(c(0.1, 0.3, 0.2, 0.4)))
  expect_equal(
    round(pmf(c(0.1, 0.3, 0.2, 0.1, 0.1))$np_pmf, 2),
    array(c(0.12, 0.37, 0.25, 0.12, 0.12))
  )
})

test_that("a warning is thrown for non-natural parametrisations", {
  expect_warning(lognormal(mean = normal(4, 1), sd = 1, max = 10))
  expect_warning(gamma(mean = normal(4, 1), sd = 1, max = 10))
})

test_that("deprecated functions are deprecated", {
  expect_deprecated(dist_spec(params_mean = 1.6, params_sd = 0.6, max = 19))
})
