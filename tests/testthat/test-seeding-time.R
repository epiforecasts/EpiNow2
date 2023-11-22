test_that("Seeding times are correctly calculated", {
  gt1 <- lognormal(meanlog = 5, sdlog = 1, max = 9)
  gt2 <- lognormal(meanlog = 10, sdlog = 2, max = 14)
  delay1 <- lognormal(meanlog = 5, sdlog = 1, max = 9)
  delay2 <- lognormal(meanlog = 7, sdlog = 3, max = 14)
  expect_equal(
    EpiNow2:::get_seeding_time(delay1, gt1 + gt2), 23L ## 10 + 15 - 1 - 1
  )
  expect_equal(
    EpiNow2:::get_seeding_time(delay1 + delay2, gt1), 12L ## 5 + 7
  )
})

test_that("Short seeding times are rounded up to 1", {
  delay <- lognormal(meanlog = 0.5, sdlog = 1, max = 2)
  gt <- fixed(value = 1)
  expect_equal(EpiNow2:::get_seeding_time(delay, gt), 1L)
})
