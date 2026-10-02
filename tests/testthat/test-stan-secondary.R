skip_on_cran()
skip_on_os("windows")

# test primary reports and observations
reports <- rep(10, 20)
obs <- rep(4, 20)
delay_rev_pmf <- rev(discretised_pmf(c(log(3), 0.1), 5, 1, 0))
scaled <- reports * 0.1
convolved <- rep(1e-5, 20) + convolve_to_report(scaled, delay_rev_pmf, 0)

check_equal <- function(args, target, dof = 0, dev = FALSE) {
  out <- do.call(calculate_secondary, args)
  out <- round(out, dof)
  if (dev) {
    return(out)
  }
  expect_equal(out, target)
}

test_that("calculate_secondary can calculate prevalence as expected", {
  check_equal(
    args = list(scaled, convolved, obs, 1, 1, 1, 1, 1, 20),
    target = c(1, 5, 5.1, 5.9, rep(6, 16)), dof = 1
  )
})

test_that("calculate_secondary can calculate incidence as expected", {
  check_equal(
    args = list(scaled, convolved, obs, 0, 1, 1, 1, 1, 20),
    target = c(1, 1, 1.1, 1.9, rep(2.0, 16)), dof = 1
  )
})

test_that("calculate_secondary can calculate incidence as expected", {
  check_equal(
    args = list(scaled, convolved, obs, 0, 1, 1, 1, 1, 20),
    target = c(1, 1, 1.1, 1.9, rep(2.0, 16)), dof = 1
  )
})

test_that("calculate_secondary can calculate incidence using only historic reports", {
  check_equal(
    args = list(scaled, convolved, obs, 0, 1, 1, 0, 1, 20),
    target = c(0, 0, 0, rep(1, 17)), dof = 0
  )
})

test_that("calculate_secondary can calculate incidence using only current reports", {
  check_equal(
    args = list(scaled, convolved, obs, 0, 0, 1, 1, 1, 20),
    target = rep(1, 20), dof = 0
  )
})

test_that("calculate_secondary can switch into prediction mode as expected", {
  check_equal(
    args = list(scaled, convolved, obs, 1, 0, 1, 1, 1, 20),
    target = c(1, rep(5, 19)), dof = 0
  )
  check_equal(
    args = list(scaled, convolved, obs, 1, 0, 1, 1, 1, 10),
    target = c(1, rep(5, 9), 6:15), dof = 0
  )
})

# hand-built inputs where each flag's contribution is easy to track
hand_scaled <- c(1, 2, 3, 4)
hand_conv <- c(10, 20, 30, 40)
hand_obs <- c(100, 200, 300, 400)

hand_secondary <- function(cumulative, historic, hist_add, current,
                           current_add, predict = 4, obs = hand_obs,
                           conv = hand_conv) {
  calculate_secondary(
    hand_scaled, conv, obs, cumulative, historic, hist_add,
    current, current_add, predict
  )
}

test_that("calculate_secondary matches the incidence preset by hand", {
  expect_equal(hand_secondary(0, 1, 1, 0, 0), hand_conv + 1e-6)
  # observations do not enter a non-cumulative target
  expect_equal(
    hand_secondary(0, 1, 1, 0, 0, obs = rev(hand_obs)),
    hand_secondary(0, 1, 1, 0, 0)
  )
})

test_that("calculate_secondary matches the prevalence preset by hand", {
  # previous observed total, plus current primary, minus historic primary
  expected <- c(0, hand_obs[1:3]) - hand_conv + hand_scaled
  expected[1] <- hand_scaled[1]
  expect_equal(hand_secondary(1, 1, 0, 1, 1), expected + 1e-6)
})

test_that("calculate_secondary combines additive historic and current reports", {
  expect_equal(
    hand_secondary(0, 1, 1, 1, 1), hand_conv + hand_scaled + 1e-6
  )
  expect_equal(
    hand_secondary(1, 1, 1, 0, 0),
    c(0, hand_obs[1:3]) + hand_conv + 1e-6
  )
  expect_equal(
    hand_secondary(1, 0, 0, 1, 0)[-1],
    hand_obs[1:3] - hand_scaled[-1] + 1e-6
  )
})

test_that("calculate_secondary returns only the offset with no primary terms", {
  expect_equal(hand_secondary(0, 0, 0, 0, 0), rep(1e-6, 4))
  expect_equal(hand_secondary(1, 0, 0, 0, 0), c(0, hand_obs[1:3]) + 1e-6)
})

test_that("calculate_secondary floors subtracted historic reports at zero", {
  out <- hand_secondary(1, 1, 0, 0, 0, obs = rep(5, 4))
  expect_equal(out, rep(1e-6, 4))
})

test_that("calculate_secondary accumulates its own output when predicting", {
  # observations are used up to `predict`, after which the previous
  # modelled value is carried forward
  out <- hand_secondary(1, 1, 1, 0, 0, predict = 2)
  expect_equal(out[1:2], c(10, 120) + 1e-6)
  expect_equal(out[3], out[2] + 30 + 1e-6)
  expect_equal(out[4], out[3] + 40 + 1e-6)
})
