skip_on_cran()
skip_on_os("windows")

# Build the ragged delay data for one delay type from a list of components.
# Parametric components have dist, params and max; others have a pmf.
delay_data <- function(components) {
  p <- vapply(components, function(x) is.null(x$pmf), logical(1))
  params <- lapply(components[p], `[[`, "params")
  pmfs <- lapply(components[!p], `[[`, "pmf")
  max_delay <- vapply(components[p], `[[`, numeric(1), "max")
  ids <- integer(length(components))
  ids[p] <- seq_len(sum(p))
  ids[!p] <- seq_len(sum(!p))
  list(
    len = sum(max_delay) + sum(lengths(pmfs) - 1) + 1,
    types_p = array(as.integer(p)), types_id = array(ids),
    types_groups = array(c(1L, length(components) + 1L)),
    max_delay = array(as.integer(max_delay)),
    np_pmf = array(as.numeric(unlist(pmfs))),
    np_groups = array(as.integer(cumsum(c(1, lengths(pmfs))))),
    params = array(as.numeric(unlist(params))),
    params_groups = array(as.integer(cumsum(c(1, lengths(params))))),
    dist = array(as.integer(vapply(components[p], `[[`, numeric(1), "dist")))
  )
}

# The combined PMF, by default forwards and without truncation
delay_pmf <- function(components, left_truncate = 0L, reverse_pmf = 0L,
                      cumulative = 0L, len = NULL) {
  d <- delay_data(components)
  get_delay_rev_pmf(
    1L, if (is.null(len)) d$len else len, d$types_p, d$types_id,
    d$types_groups, d$max_delay, d$np_pmf, d$np_groups, d$params,
    d$params_groups, d$dist, left_truncate, reverse_pmf, cumulative
  )
}

lnorm <- function(meanlog, sdlog, max) {
  list(dist = 1, params = c(meanlog, sdlog), max = max)
}
gam <- function(shape, rate, max) {
  list(dist = 2, params = c(shape, rate), max = max)
}
np <- function(pmf) list(pmf = pmf)

convolve_pmfs <- function(a, b) {
  vapply(seq_len(length(a) + length(b) - 1), function(k) {
    i <- seq_along(a)
    j <- k - i + 1
    keep <- j >= 1 & j <= length(b)
    sum(a[i[keep]] * b[j[keep]])
  }, numeric(1))
}

test_that("discretised_pmf matches primary censoring by numerical integration", {
  # With a uniform primary window of one day the censored CDF is
  # F_S(d) = int_{d - 1}^{d} F(u) du, and the PMF is its difference over F_S(n)
  censored_pmf <- function(cdf, n) {
    fs <- vapply(seq_len(n), function(d) {
      integrate(cdf, max(d - 1, 0), d, rel.tol = 1e-12)$value
    }, numeric(1))
    diff(c(0, fs)) / fs[n]
  }
  expect_equal(
    discretised_pmf(c(1.6, 0.42), 8, 1, 0),
    censored_pmf(function(x) plnorm(x, 1.6, 0.42), 8),
    tolerance = 1e-8
  )
  expect_equal(
    discretised_pmf(c(2.5, 0.5), 8, 2, 0),
    censored_pmf(function(x) pgamma(x, 2.5, 0.5), 8),
    tolerance = 1e-8
  )
})

test_that("delay PMFs sum to one", {
  cases <- list(
    list(lnorm(1.6, 0.42, 14)), list(gam(2.5, 0.5, 10)),
    list(lnorm(-1, 0.3, 1)), list(lnorm(2.5, 0.8, 60)),
    list(gam(20, 0.5, 60)), list(lnorm(1, 0.5, 0)),
    list(lnorm(1.6, 0.42, 14), gam(1.4, 0.38, 10), np(c(0.2, 0.3, 0.5)))
  )
  for (components in cases) {
    pmf <- delay_pmf(components)
    expect_false(anyNA(pmf))
    expect_equal(sum(pmf), 1, tolerance = 1e-12)
  }
  expect_equal(sum(delay_pmf(cases[[1]], left_truncate = 2L)), 1)
})

test_that("a zero-length or point-mass delay changes nothing", {
  expect_equal(discretised_pmf(c(1, 0.5), 1, 1, 0), 1)
  expect_equal(discretised_pmf(c(2, 1), 1, 2, 0), 1)
  alone <- delay_pmf(list(lnorm(1.6, 0.42, 14)))
  expect_equal(
    delay_pmf(list(lnorm(1.6, 0.42, 14), gam(2, 1, 0))), alone,
    tolerance = 1e-14
  )
  expect_equal(
    delay_pmf(list(np(1), lnorm(1.6, 0.42, 14), np(1))), alone,
    tolerance = 1e-14
  )
})

test_that("combining delays convolves their PMFs", {
  a <- discretised_pmf(c(1.6, 0.42), 15, 1, 0)
  b <- discretised_pmf(c(1.4, 0.38), 11, 2, 0)
  c <- c(0.2, 0.3, 0.5)
  expect_equal(
    delay_pmf(list(lnorm(1.6, 0.42, 14), gam(1.4, 0.38, 10), np(c))),
    convolve_pmfs(convolve_pmfs(a, b), c),
    tolerance = 1e-12
  )
  expect_equal(
    delay_pmf(list(np(c(0.5, 0.5)), np(c(0.5, 0.5)))), c(0.25, 0.5, 0.25)
  )
})

test_that("a parametric delay and its PMF give the same result", {
  # As when the parameters are fixed and the delay is passed as a PMF
  pmf <- discretised_pmf(c(0.6, 0.5), 11, 1, 0)
  expect_equal(
    delay_pmf(list(lnorm(1.6, 0.42, 14), lnorm(0.6, 0.5, 10))),
    delay_pmf(list(lnorm(1.6, 0.42, 14), np(pmf))),
    tolerance = 1e-14
  )
})

test_that("non-parametric delays pass through", {
  pmf <- c(0.1, 0.3, 0.4, 0.2)
  expect_equal(delay_pmf(list(np(pmf))), pmf)
  expect_equal(delay_pmf(list(np(pmf)), reverse_pmf = 1L), rev(pmf))
  expect_equal(delay_pmf(list(np(pmf)), len = 6L), c(pmf, 0, 0))
})

test_that("truncation, cumulative and reversed PMFs are as expected", {
  pmf <- c(0.2, 0.3, 0.5)
  expect_equal(
    delay_pmf(list(np(pmf)), left_truncate = 1L), c(0, 0.375, 0.625)
  )
  expect_equal(delay_pmf(list(np(pmf)), cumulative = 1L), c(0.2, 0.5, 1))
  expect_equal(
    delay_pmf(list(np(pmf)), cumulative = 1L, reverse_pmf = 1L),
    c(1, 0.5, 0.2)
  )
  expect_equal(discretised_pmf(c(1.2, 0.6), 12, 1, 3)[1:3], rep(0, 3))
  expect_equal(sum(discretised_pmf(c(1.2, 0.6), 12, 2, 3)), 1)
})

test_that("get_delay_type_max adds up the maximum of each delay", {
  # Two delay types: lognormal then PMF, and gamma alone
  d <- delay_data(
    list(lnorm(1.6, 0.42, 14), np(c(0.2, 0.3, 0.5)), gam(2, 1, 6))
  )
  expect_equal(
    get_delay_type_max(
      2L, d$types_p, d$types_id, array(c(1L, 3L, 4L)), d$max_delay,
      d$np_groups
    ),
    c(16, 6)
  )
})
