test_that("Seeding times are correctly calculated", {
  gt1 <- LogNormal(mean = 5, sd = 1, max = 9)
  gt2 <- LogNormal(mean = 10, sd = 2, max = 14)
  delay1 <- LogNormal(mean = 5, sd = 1, max = 9)
  delay2 <- LogNormal(mean = 7, sd = 3, max = 14)
  expect_equal(
    EpiNow2:::get_seeding_time(delay1, gt1 + gt2), 23L ## 9 + 14
  )
  expect_equal(
    EpiNow2:::get_seeding_time(delay1 + delay2, gt1), 12L ## 5 + 7
  )
})

test_that("Short seeding times are rounded up to 1", {
  delay <- LogNormal(mean = 0.5, sd = 1, max = 2)
  gt <- Fixed(value = 0.5)
  # mean(delay) ≈ 0.5, max(gt) = 0.5 (Fixed value before discretisation),
  # so seeding_time = max(round(max(0.5, 0.5)), 1) = max(0, 1) = 1
  expect_equal(EpiNow2:::get_seeding_time(delay, gt), 1L)
})

test_that("Prevalence seeding covers the observation kernel", {
  gt <- Fixed(1)
  obs <- obs_opts(type = "prevalence", duration = Fixed(30))
  expect_equal(get_seeding_time(Fixed(0), gt, obs = obs), 32)
  expect_equal(get_seeding_time(Fixed(5), gt, obs = obs), 37)
  expect_equal(
    get_seeding_time(Fixed(30), gt, obs = obs_opts(type = "prevalence")),
    31
  )
  expect_equal(get_seeding_time(Fixed(2) + Fixed(3), gt, obs = obs), 37)
  expect_equal(get_seeding_time(Fixed(0), Fixed(40), obs = obs), 40)
  uncertain <- obs_opts(
    type = "prevalence",
    duration = LogNormal(meanlog = Normal(log(5), 0.1), sdlog = 0.2, max = 20)
  )
  expect_equal(get_seeding_time(Fixed(0), gt, obs = uncertain), 21)
  expect_equal(get_seeding_time(Fixed(5), gt), 5)
})
