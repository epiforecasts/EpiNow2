test_that("dist_spec returns correct output for fixed lognormal distribution", {
  result <- discretise(LogNormal(meanlog = 5, sdlog = 1, max = 19))
  expect_equal(get_distribution(result), "nonparametric")
  expect_equal(max(result), 19)
  expect_equal(
    as.vector(round(get_pmf(result), 2)),
    c(0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.02, 0.03,
      0.03, 0.04, 0.05, 0.06, 0.07, 0.07, 0.08, 0.09,
      0.10, 0.11, 0.11, 0.12)
  )
})

test_that("dist_spec returns correct output for uncertain gamma distribution", {
  result <- discretise(
    Gamma(shape = Normal(3, 0.5), rate = Normal(2, 0.5), max = 19),
    strict = FALSE
  )
  expect_equal(get_parameters(result)$shape$parameters$mean, 3)
  expect_equal(get_parameters(result)$shape$parameters$sd, 0.5)
  expect_equal(get_parameters(result)$rate$parameters$mean, 2)
  expect_equal(get_parameters(result)$rate$parameters$sd, 0.5)
  expect_equal(get_distribution(result), "gamma")
  expect_equal(max(result), 19)
})

test_that("dist_spec returns correct output for gamma distribution parameterised with scale", {
  result <- Gamma(shape = 3, scale = 2)
  expect_equal(get_parameters(result)$shape, 3)
  expect_equal(get_parameters(result)$rate, 0.5)
  expect_equal(get_distribution(result), "gamma")
  expect_true(is.infinite(max(result)))
})

test_that("dist_spec returns correct output for fixed distribution", {
  result <- discretise(
    fix_dist(LogNormal(meanlog = Normal(5, 3), sdlog = 1, max = 19))
  )
  expect_equal(get_distribution(result), "nonparametric")
  expect_equal(max(result), 19)
  expect_equal(
    as.vector(round(get_pmf(result), 2)),
    c(0.00, 0.00, 0.00, 0.00, 0.01, 0.01, 0.02, 0.03,
      0.03, 0.04, 0.05, 0.06, 0.07, 0.07, 0.08, 0.09,
      0.10, 0.11, 0.11, 0.12)
  )
})

test_that("dist_spec returns error when mixed natural and unnatural parameters are specified", {
  expect_error(
    LogNormal(meanlog = 5, sd = 1, max = 20),
    "Incompatible combination."
  )
})

test_that("dist_spec returns error when the wrong number of parameters are given", {
  expect_error(LogNormal(sd = 1, max = 20), "must be specified")
  expect_error(Gamma(shape = 1, rate = 2, mean = 3), "must be specified")
})

test_that("c.dist_spec returns correct output for sum of two distributions", {
  dist1 <- LogNormal(meanlog = 5, sdlog = 1, max = 19)
  dist2 <- Gamma(shape = Normal(3, 0.5), rate = Normal(2, 0.5), max = 20)
  result <- dist1 + dist2
  expect_equal(get_parameters(result, 1)$meanlog, 5)
  expect_equal(get_parameters(result, 1)$sdlog, 1)
  expect_equal(get_parameters(get_parameters(result, 2)$shape)$mean, 3)
  expect_equal(get_parameters(get_parameters(result, 2)$shape)$sd, 0.5)
  expect_equal(get_parameters(get_parameters(result, 2)$rate)$mean, 2)
  expect_equal(get_parameters(get_parameters(result, 2)$rate)$sd, 0.5)
  expect_equal(length(result), 2)
})

test_that("collapse returns correct output for sum of two nonparametric distributions", {
  dist1 <- NonParametric(c(0.1, 0.2, 0.3, 0.4))
  dist2 <- NonParametric(c(0.1, 0.2, 0.3, 0.4))
  result <- collapse(c(dist1, dist2))
  expect_equal(get_distribution(result), "nonparametric")
  expect_equal(max(result), 6)
  expect_equal(ndist(result), 1)
  expect_equal(
    round(get_pmf(result), 2),
    c(0.01, 0.04, 0.10, 0.20, 0.25, 0.24, 0.16)
  )
})

test_that("`bound_dist` function can be applied to a convolution", {
  # Create distributions
  dist1 <- LogNormal(meanlog = 1.6, sdlog = 1, max = 19)
  dist2 <- Gamma(mean = 3, sd = 2, max = 19)

  # Compute combined distribution with default tolerance
  combined <- collapse(discretise(c(dist1, dist2)))
  
  # Compute combined distribution with larger tolerance
  combined_tolerance <- bound_dist(combined, tolerance = 0.001)

  combined_pmf <- get_pmf(combined)
  combined_tolerance_pmf <- get_pmf(combined_tolerance)

  # The length of the combined PMF should be greater with default tolerance
  expect_true(length(combined_pmf) > length(combined_tolerance_pmf))
  # Both should sum to 1
  expect_equal(sum(combined_pmf), 1)
  expect_equal(sum(combined_tolerance_pmf), 1)
  # The first 5 entries should be within 0.01 of each other
  expect_equal(
    combined_pmf[1:5], combined_tolerance_pmf[1:5], tolerance = 0.01
  )
  expect_equal(mean(combined), mean(combined_tolerance), tolerance = 0.1)
})

test_that("summary functions return correct output for fixed lognormal distribution", {
  dist <- discretise(LogNormal(mean = 3, sd = 1, max = 19))
  expect_equal(mean(dist), 3.0, tolerance = 0.01)
  expect_equal(EpiNow2:::sd(dist), 1.34, tolerance = 0.01)
  expect_equal(max(dist), 19L)
})

test_that("summary functions return correct output for uncertain gamma distribution", {
  dist <- Gamma(shape = Normal(3, 0.5), rate = Normal(2, 0.5), max = 19)
  expect_equal(mean(dist, ignore_uncertainty = TRUE), 1.5)
  expect_equal(max(dist), 19L)
})

test_that("mean returns correct output for sum of two distributions", {
  dist1 <- LogNormal(meanlog = 1, sdlog = 1, max = 19)
  dist2 <- Gamma(mean = 3, sd = 2, max = 19)
  dist <- dist1 + dist2
  expect_equal(mean(dist), c(4.48, 3), tolerance = 0.001)
  expect_equal(EpiNow2:::sd(dist), c(5.87, 2), tolerance = 0.001)
  ## shortened due to tolerance level
  expect_equal(max(dist), c(19L, 19L))
})

test_that("mean returns NA when applied to uncertain distributions", {
  dist <- Gamma(shape = Normal(3, 0.5), rate = Normal(2, 0.5), max = 19)
  expect_true(is.na(mean(dist)))
})

test_that("sd returns NA when applied to uncertain distributions", {
  dist <- Gamma(shape = Normal(3, 0.5), rate = Normal(2, 0.5), max = 19)
  expect_true(is.na(EpiNow2:::sd(dist)))
})

test_that("print.dist_spec correctly prints the parameters of the fixed lognormal", {
  dist <- discretise(LogNormal(meanlog = 1.5, sdlog = 0.5, max = 19))
  
  expect_output(print(dist),"- nonparametric distribution\\n  PMF: \\[0\\.00068 0\\.027 0\\.11 0\\.18 0\\.19 0\\.16 0\\.11 0\\.078 0\\.052 0\\.035 0\\.023 0\\.015 0\\.0099 0\\.0065 0\\.0044 0\\.003 0\\.002 0\\.0014 0\\.00095 0\\.00066\\]" )
})

test_that("print.dist_spec correctly prints the parameters of the uncertain gamma", {
  gamma <- Gamma(
    shape = Normal(3, 0.5), rate = Normal(2, 0.5), max = 19
  )
  
  expect_output(print(gamma), "- gamma distribution \\(max: 19\\):\\n  shape:\\n    - normal distribution:\\n      mean:\\n        3\\n      sd:\\n        0\\.5\\n  rate:\\n    - normal distribution:\\n      mean:\\n        2\\n      sd:\\n        0\\.5")
})

test_that("print.dist_spec correctly prints the parameters of the uncertain lognormal", {
  dist <- LogNormal(
    meanlog = Normal(1.5, 0.1), sdlog = Normal(0.5, 0.1), max = 19
  )
  
  expect_output(print(dist), "- lognormal distribution \\(max: 19\\):\\n  meanlog:\\n    - normal distribution:\\n      mean:\\n        1\\.5\\n      sd:\\n        0\\.1\\n  sdlog:\\n    - normal distribution:\\n      mean:\\n        0\\.5\\n      sd:\\n        0\\.1")
})

test_that("print.dist_spec correctly prints the parameters of a combination of distributions", {
  dist1 <- LogNormal(meanlog = 1.5, sdlog = 0.5, max = 19)
  dist2 <- Gamma(shape =  Normal(3, 0.5), rate = Normal(2, 0.5), max = 19)
  combined <- dist1 + dist2
  expect_output(print(combined), "Composite distribution:\\n- lognormal distribution \\(max: 19\\):\\n  meanlog:\\n    1\\.5\\n  sdlog:\\n    0\\.5\\n- gamma distribution \\(max: 19\\):\\n  shape:\\n    - normal distribution:\\n      mean:\\n        3\\n      sd:\\n        0\\.5\\n  rate:\\n    - normal distribution:\\n      mean:\\n        2\\n      sd:\\n        0\\.5")
})

test_that("plot.dist_spec returns a ggplot object", {
  dist <- LogNormal(meanlog = 1.6, sdlog = 0.5, max = 19)
  plot <- plot(dist)
  expect_s3_class(plot, "ggplot")
})

test_that("plot.dist_spec correctly plots a single distribution", {
  dist <- LogNormal(meanlog = 1.6, sdlog = 0.5, max = 19)
  plot <- plot(dist)
  expect_equal(length(plot$layers), 2)
  expect_equal(length(plot$facet$params$facets), 1)
})

test_that("plot.dist_spec correctly plots multiple distributions", {
  dist1 <- LogNormal(meanlog = 1.6, sdlog = 0.5, max = 19)
  dist2 <- Gamma(shape = Normal(3, 5), rate = Normal(1, 2), max = 19)
  combined <- dist1 + dist2
  plot <- plot(combined)
  expect_equal(length(plot$layers), 2)
  expect_equal(length(plot$facet$params$facets), 1)
})

test_that("plot.dist_spec correctly plots a combination of fixed distributions", {
  dist <- LogNormal(meanlog = 1.6, sdlog = 0.5, max = 19)
  combined <- dist + dist
  plot <- plot(combined)
  expect_equal(length(plot$layers), 2)
  expect_equal(length(plot$facet$params$facets), 1)
})

test_that("fix_dist works with composite delay distributions", {
  dist1 <- LogNormal(meanlog = Normal(1, 0.1), sdlog = 1, max = 19)
  dist2 <- Gamma(mean = 3, sd = 2, max = 19)
  dist <- dist1 + dist2
  expect_equal(ndist(collapse(discretise(fix_dist(dist)))), 1L)
})

test_that("composite delay distributions can be disassembled", {
  dist1 <- LogNormal(meanlog = Normal(1, 0.1), sdlog = 1, max = 19)
  dist2 <- Gamma(mean = 3, sd = 2, max = 19)
  dist <- dist1 + dist2
  expect_equal(EpiNow2:::extract_single_dist(dist, 1), dist1)
  expect_equal(EpiNow2:::extract_single_dist(dist, 2), dist2)
})

test_that("constrained distributions are correctly identified", {
  expect_false(is_constrained(Gamma(shape = 3, scale = 2)))
  expect_true(is_constrained(Gamma(shape = 3, scale = 2, max = 10)))
  expect_true(is_constrained(Gamma(shape = 3, scale = 2, tolerance = 0.1)))
  expect_false(is_constrained(
    Gamma(shape = 3, scale = 2) +
    Gamma(shape = 3, scale = 2, max = 10)
  ))
  expect_true(is_constrained(
    Gamma(shape = 3, scale = 2, max = 10) +
    Gamma(shape = 3, scale = 2, max = 10)
  ))
})

test_that("delay distributions can be specified in different ways", {
  expect_equal(
    unname(as.numeric(get_parameters(LogNormal(mean = 4, sd = 1)))),
    c(1.4, 0.25),
    tolerance = 0.1
  )
  expect_equal(
    round(get_pmf(discretise(LogNormal(mean = 4, sd = 1, max = 10))), 2),
    c(0.00, 0.00, 0.07, 0.27, 0.35, 0.21, 0.07, 0.02, 0.00, 0.00, 0.00)
  )
  expect_equal(
    round(get_pmf(discretise(LogNormal(mean = 4, sd = 1, tolerance = 0.1))), 2),
    c(0.00, 0.00, 0.08, 0.28, 0.36, 0.21, 0.07)
  )
   expect_equal(
    unname(as.numeric(get_parameters(Gamma(mean = 4, sd = 1)))),
    c(16, 4),
    tolerance = 0.1
  )
  expect_equal(
    round(get_pmf(discretise(Gamma(mean = 4, sd = 1, max = 7))), 2),
    c(0.00, 0.00, 0.08, 0.26, 0.35, 0.22, 0.08, 0.02)
  )
  expect_equal(
    round(get_pmf(discretise(Gamma(mean = 4, sd = 1, tolerance = 0.1))), 2),
    c(0.00, 0.00, 0.08, 0.27, 0.35, 0.22, 0.08)
  )
  expect_equal(
    unname(as.numeric(
      get_parameters(get_parameters(
        c(
          Gamma(
            shape = Normal(12, 3), rate = Normal(3, 0.5)
          ),
          Gamma(
            shape = Normal(16, 2), rate = Normal(4, 1)
          )
        ), 2
      )$shape)
    )),
    c(16, 2)
  )
  expect_equal(
    unname(as.numeric(
      get_parameters(get_parameters(
        Gamma(
          shape = Normal(16, 2), rate = Normal(4, 1)
        )
      )$rate)
    )),
    c(4, 1)
  )
  expect_equal(
    unname(as.numeric(get_parameters(Normal(mean = 4, sd = 1)))), c(4, 1)
  )
  expect_equal(
    round(get_pmf(discretise(Normal(mean = 4, sd = 1, max = 5))), 2),
    c(0.00, 0.01, 0.09, 0.26, 0.38, 0.26)
  )
  expect_equal(
    round(get_pmf(discretise(Normal(mean = 4, sd = 1, tolerance = 0.1))), 2),
    c(0.00, 0.01, 0.08, 0.24, 0.35, 0.24, 0.08)
  )
  expect_equal(get_pmf(discretise(Fixed(value = 3))), c(0, 0, 0, 1))
  expect_equal(get_parameters(Fixed(value = 3.5))$value, 3.5)
  expect_equal(
    get_pmf(NonParametric(c(0.1, 0.3, 0.2, 0.4))),
    c(0.1, 0.3, 0.2, 0.4)
  )
  expect_equal(
    round(get_pmf(NonParametric(c(0.1, 0.3, 0.2, 0.1, 0.1))), 2),
    c(0.12, 0.37, 0.25, 0.12, 0.12)
  )
  expect_equal(
    get_distribution(NonParametric(c(0.1, 0.3, 0.2, 0.1, 0.1))),
    "nonparametric"
  )})

test_that("get functions report errors", {
  expect_error(get_parameters("test"), "Object must be of class")
  expect_error(
    get_distribution(Gamma(mean = 4, sd = 1), 2),
    "cannot be greater than the number of distributions"
  )
  expect_error(get_pmf(Gamma(mean = 4, sd = 1)), "parametric")
  expect_error(
    get_parameters(NonParametric(c(0.1, 0.3, 0.2, 0.1, 0.1))),
    "nonparametric"
  )
  expect_error(get_parameters(c(
    Gamma(mean = 4, sd = 1), Gamma(mean = 4, sd = 1)
  )), "must be specified")
})
