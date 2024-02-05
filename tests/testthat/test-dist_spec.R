
test_that("dist_spec returns correct output for fixed lognormal distribution", {
  result <- dist_spec(mean = 5, sd = 1, max = 19, distribution = "lognormal")
  expect_equal(dim(result$mean_mean), 0)
  expect_equal(dim(result$sd_mean), 0)
  expect_equal(dim(result$dist), 0)
  expect_equal(dim(result$max), 0)
  expect_equal(result$fixed, array(1))
  expect_equal(
    as.vector(round(result$np_pmf, 2)),
    c(0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.02, 0.03,
      0.03, 0.04, 0.05, 0.06, 0.07, 0.07, 0.08, 0.09,
      0.10, 0.10, 0.11, 0.12)
  )
})

test_that("dist_spec returns correct output for uncertain gamma distribution", {
  result <- dist_spec(
    mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 19,
    distribution = "gamma"
  )
  expect_equal(result$mean_mean, array(3L))
  expect_equal(result$sd_mean, array(2))
  expect_equal(result$mean_sd, array(0.5))
  expect_equal(result$sd_sd, array(0.5))
  expect_equal(result$dist, array("gamma"))
  expect_equal(result$max, array(19))
  expect_equal(result$fixed, array(0L))
})

test_that("dist_spec returns correct output for fixed distribution", {
  result <- fix_dist(dist_spec(
    mean = 5, mean_sd = 3, sd = 1, max = 19, distribution = "lognormal",
  ))
  expect_equal(dim(result$mean_mean), 0)
  expect_equal(dim(result$sd_mean), 0)
  expect_equal(result$fixed, array(1L))
  expect_equal(
    as.vector(round(result$np_pmf, 2)),
    c(0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.02, 0.03,
      0.03, 0.04, 0.05, 0.06, 0.07, 0.07, 0.08, 0.09,
      0.10, 0.10, 0.11, 0.12)
  )
})

test_that("dist_spec returns error when both pmf and distributional parameters are specified", {
  expect_error(dist_spec(mean = 5, sd = 1, max = 20, distribution = "lognormal", pmf = c(0.1, 0.2, 0.3, 0.4)), 
               "Distributional parameters or a pmf can be specified, but not both.")
})

test_that("dist_spec returns error when mean is missing but other distributional parameters are given", {
  expect_error(dist_spec(sd = 1, max = 20, distribution = "lognormal"), 
               "If any distributional parameters are given then so must the mean.")
})

test_that("dist_spec returns error when maximum of parametric distributions is not specified", {
  expect_error(dist_spec(mean = 5, sd = 1, distribution = "lognormal"), 
               "Maximum of parametric distributions must be specified.")
})

test_that("+.dist_spec returns correct output for sum of two distributions", {
  lognormal <- dist_spec(mean = 5, sd = 1, max = 19, distribution = "lognormal")
  gamma <- dist_spec(mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 20, distribution = "gamma")
  result <- lognormal + gamma
  expect_equal(result$mean_mean, array(3))
  expect_equal(result$sd_mean, array(2))
  expect_equal(result$mean_sd, array(0.5))
  expect_equal(result$sd_sd, array(0.5))
  expect_equal(result$n, 2)
  expect_equal(result$n_p, 1)
  expect_equal(result$n_np, 1)
  expect_equal(result$np_pmf_length, 20)
})

test_that("+.dist_spec returns correct output for sum of two fixed distributions", {
  lognormal <- fix_dist(dist_spec(
    mean = 5, sd = 1, max = 19, distribution = "lognormal"
  ))
  gamma <- fix_dist(dist_spec(
    mean = 3, sd = 2, max = 19, distribution = "gamma"
  ))
  result <- lognormal + gamma
  expect_equal(dim(result$mean_mean), 0)
  expect_equal(dim(result$sd_mean), 0)
  expect_equal(result$n, 1)
  expect_equal(result$n_p, 0)
  expect_equal(result$n_np, 1)
  expect_equal(result$np_pmf_length, 30)
})

test_that("+.dist_spec returns correct output for sum of two nonparametric distributions", {
  lognormal <- dist_spec(pmf = c(0.1, 0.2, 0.3, 0.4))
  gamma <- dist_spec(pmf = c(0.1, 0.2, 0.3, 0.4))
  result <- lognormal + gamma
  expect_equal(dim(result$mean_mean), 0)
  expect_equal(dim(result$sd_mean), 0)
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
  lognormal <- dist_spec(
    mean = 1.6, sd = 1, max = 19, distribution = "lognormal"
  )
  gamma <- dist_spec(
    mean = 3, sd = 2, max = 19, distribution = "gamma"
  )
  
  # Compute combined distribution with default tolerance
  combined_default <- EpiNow2:::`+.dist_spec`(lognormal, gamma)
  
  # Compute combined distribution with larger tolerance
  combined_larger_tolerance <- EpiNow2:::dist_spec_plus(
    lognormal, gamma, tolerance = 0.01
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


test_that("mean.dist_spec returns correct output for fixed lognormal distribution", {
  lognormal <- dist_spec(
    mean = convert_to_logmean(3, 1), sd = convert_to_logsd(3, 1),
    max = 19, distribution = "lognormal"
  )
  result <- EpiNow2:::mean.dist_spec(lognormal)
  expect_equal(result, 2.49, tolerance = 0.01) # here we can see the bias from 
  # using this kind of discretisation approach
})

test_that("mean.dist_spec returns correct output for uncertain gamma distribution", {
  gamma <- dist_spec(mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 19, distribution = "gamma")
  result <- EpiNow2:::mean.dist_spec(gamma)
  expect_equal(result, 3)
})

test_that("mean.dist_spec returns correct output for sum of two distributions", {
  lognormal <- dist_spec(mean = 1, sd = 1, max = 19, distribution = "lognormal")
  gamma <- dist_spec(mean = 3, sd = 2, max = 19, distribution = "gamma")
  result <- EpiNow2:::mean.dist_spec(lognormal + gamma)
  expect_equal(result, c(5.84), tolerance = 0.001)
})

test_that("print.dist_spec correctly prints the parameters of the fixed lognormal", {
  lognormal <- dist_spec(mean = 1.5, sd = 0.5, max = 19, distribution = "lognormal")
  
  expect_output(print(lognormal), "\\n  Fixed distribution with PMF \\[0\\.0014 0\\.052 0\\.16 0\\.2 0\\.18 0\\.13 0\\.094 0\\.063 0\\.042 0\\.027 0\\.018 0\\.012 0\\.0079 0\\.0052 0\\.0035 0\\.0024 0\\.0016 0\\.0011 0\\.00078 0\\.00055\\]\\n")
})

test_that("print.dist_spec correctly prints the parameters of the uncertain gamma", {
  gamma <- dist_spec(
    mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 19,
    distribution = "gamma"
  )
  
  expect_output(print(gamma), "\\n  Uncertain gamma distribution with \\(untruncated\\) mean 3 \\(SD 0\\.5\\) and SD 2 \\(SD 0\\.5\\)\\n")
})

test_that("print.dist_spec correctly prints the parameters of the uncertain lognormal", {
  lognormal_uncertain <- dist_spec(mean = 1.5, sd = 0.5, mean_sd = 0.1, sd_sd = 0.1, max = 19, distribution = "lognormal")
  
  expect_output(print(lognormal_uncertain), "\\n  Uncertain lognormal distribution with \\(untruncated\\) logmean 1\\.5 \\(SD 0\\.1\\) and logSD 0\\.5 \\(SD 0\\.1\\)\\n")
})

test_that("print.dist_spec correctly prints the parameters of an empty distribution", {
  empty <- dist_spec()
  
  expect_output(print(empty), "Empty `dist_spec` distribution.")
})

test_that("print.dist_spec correctly prints the parameters of a combination of distributions", {
  lognormal <- dist_spec(mean = 1.5, sd = 0.5, max = 19, distribution = "lognormal")
  gamma <- dist_spec(mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 19, distribution = "gamma")
  combined <- lognormal + gamma
  
  expect_output(print(combined), "Combination of delay distributions:\\n  Fixed distribution with PMF \\[0\\.0014 0\\.052 0\\.16 0\\.2 0\\.18 0\\.13 0\\.094 0\\.063 0\\.042 0\\.027 0\\.018 0\\.012 0\\.0079 0\\.0052 0\\.0035 0\\.0024 0\\.0016 0\\.0011 0\\.00078 0\\.00055\\]\\n  Uncertain gamma distribution with \\(untruncated\\) mean 3 \\(SD 0\\.5\\) and SD 2 \\(SD 0\\.5\\)\\n")
})

test_that("plot.dist_spec returns a ggplot object", {
  lognormal <- dist_spec(mean = 1.6, sd = 0.5, max = 19, distribution = "lognormal")
  plot <- plot(lognormal)
  expect_s3_class(plot, "ggplot")
})

test_that("plot.dist_spec correctly plots a single distribution", {
  lognormal <- dist_spec(mean = 1.6, sd = 0.5, max = 19, distribution = "lognormal")
  plot <- plot(lognormal)
  expect_equal(length(plot$layers), 2)
  expect_equal(length(plot$facet$params$facets), 1)
})

test_that("plot.dist_spec correctly plots multiple distributions", {
  lognormal <- dist_spec(mean = 1.6, sd = 0.5, max = 19, distribution = "lognormal")
  gamma <- dist_spec(mean = 3, sd = 2, mean_sd = 0.5, sd_sd = 0.5, max = 19, distribution = "gamma")
  combined <- lognormal + gamma
  plot <- plot(combined)
  expect_equal(length(plot$layers), 2)
  expect_equal(length(plot$facet$params$facets), 1)
})

test_that("plot.dist_spec correctly plots a combination of fixed distributions", {
  lognormal <- dist_spec(mean = 1.6, sd = 0.5, max = 19, distribution = "lognormal")
  combined <- lognormal + lognormal
  plot <- plot(combined)
  expect_equal(length(plot$layers), 2)
  expect_equal(length(plot$facet$params$facets), 1)
})

test_that("deprecated arguments are caught", {
  expect_deprecated(dist_spec(mean = 1.6, sd = 0.6, max = 19, fixed = TRUE))
})
