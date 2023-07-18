test_that("Seeding times are correctly calculated", {
  gt1 <- dist_spec(mean = 5, sd = 1, max = 10)
  gt2 <- dist_spec(mean = 10, sd = 2, max = 15)
  delay1 <- dist_spec(mean = 5, sd = 1, max = 10)
  delay2 <- dist_spec(mean = 7, sd = 3, max = 15)
  expect_equal(
    EpiNow2:::get_seeding_time(delay1, gt1 + gt2), 23L ## 10 + 15 - 1 - 1
  )
  expect_equal(
    EpiNow2:::get_seeding_time(delay1 + delay2, gt1), 12L ## 5 + 7
  )
})

test_that("Short seeding times are rounded up to 1", {
  delay <- dist_spec(mean = 0.5, sd = 1, max = 2)
  gt <- dist_spec(mean = 1)
  expect_equal(EpiNow2:::get_seeding_time(delay, gt), 1L)
})
