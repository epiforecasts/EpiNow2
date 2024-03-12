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
  gt <- Fixed(value = 1)
  expect_equal(EpiNow2:::get_seeding_time(delay, gt), 1L)
})
