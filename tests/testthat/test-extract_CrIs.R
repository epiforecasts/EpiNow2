context("extract_CrIs")

test_that("extract_CrIs return the expected credible intervals", {
  samples <- data.frame(value = 1:10, type = "car")
  summarised <- calc_CrIs(samples, summarise_by = "type",
                          CrIs = c(seq(0.1, 0.9, 0.1)))
  expect_equal(extract_CrIs(summarised), seq(90, 10, -10))
})