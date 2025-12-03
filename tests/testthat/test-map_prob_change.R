# Test map_prob_change function

test_that("map_prob_change categorises probabilities correctly", {
  # Test boundary values for each category
  expect_equal(
    as.character(map_prob_change(0.01)),
    "Increasing"
  )
  expect_equal(
    as.character(map_prob_change(0.04)),
    "Increasing"
  )

  expect_equal(
    as.character(map_prob_change(0.05)),
    "Likely increasing"
  )
  expect_equal(
    as.character(map_prob_change(0.39)),
    "Likely increasing"
  )

  expect_equal(
    as.character(map_prob_change(0.40)),
    "Stable"
  )
  expect_equal(
    as.character(map_prob_change(0.59)),
    "Stable"
  )

  expect_equal(
    as.character(map_prob_change(0.60)),
    "Likely decreasing"
  )
  expect_equal(
    as.character(map_prob_change(0.94)),
    "Likely decreasing"
  )

  expect_equal(
    as.character(map_prob_change(0.95)),
    "Decreasing"
  )
  expect_equal(
    as.character(map_prob_change(1.0)),
    "Decreasing"
  )
})

test_that("map_prob_change returns ordered factor", {
  result <- map_prob_change(0.5)

  expect_s3_class(result, "factor")
  expect_equal(
    levels(result),
    c("Increasing", "Likely increasing", "Stable",
      "Likely decreasing", "Decreasing")
  )
})

test_that("map_prob_change handles vectorised input", {
  probs <- c(0.01, 0.2, 0.5, 0.8, 0.99)
  result <- map_prob_change(probs)

  expect_length(result, 5)
  expect_equal(
    as.character(result),
    c("Increasing", "Likely increasing", "Stable",
      "Likely decreasing", "Decreasing")
  )
})

test_that("map_prob_change handles edge cases", {
  # Exactly at boundaries
  expect_equal(
    as.character(map_prob_change(0.05)),
    "Likely increasing"
  )
  expect_equal(
    as.character(map_prob_change(0.4)),
    "Stable"
  )
  expect_equal(
    as.character(map_prob_change(0.6)),
    "Likely decreasing"
  )
  expect_equal(
    as.character(map_prob_change(0.95)),
    "Decreasing"
  )
})

test_that("map_prob_change factor levels are in logical order", {
  result <- map_prob_change(c(0.01, 0.5, 0.99))

  # Factor levels should be in order from increasing to decreasing
  expect_equal(
    levels(result),
    c("Increasing", "Likely increasing", "Stable",
      "Likely decreasing", "Decreasing")
  )

  # Numeric conversion should reflect ordering
  expect_true(as.numeric(result[1]) < as.numeric(result[2]))
  expect_true(as.numeric(result[2]) < as.numeric(result[3]))
})
