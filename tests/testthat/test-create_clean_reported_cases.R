
test_that("create_clean_reported_cases runs without errors", {
  expect_no_error(create_clean_reported_cases(example_confirmed, 7))
})

test_that("create_clean_reported_cases returns a data table", {
  result <- create_clean_reported_cases(example_confirmed, 7)
  expect_s3_class(result, "data.table")
})

test_that("create_clean_reported_cases filters leading zeros correctly", {
  # Modify example_confirmed to have leading zeros
  modified_data <- example_confirmed
  modified_data[1:3, "confirm"] <- 0
  
  result <- create_clean_reported_cases(modified_data, 7)
  # Check if the first row with non-zero cases is retained
  expect_equal(result$date[1], min(modified_data$date[modified_data$confirm > 0]))
})

test_that("create_clean_reported_cases replaces zero cases correctly", {
  # Modify example_confirmed to have zero cases that should be replaced
  modified_data <- example_confirmed
  modified_data$confirm[10:16] <- 0
  threshold <- 10
  
  result <- create_clean_reported_cases(
    modified_data, 0, zero_threshold = threshold
  )
  # Check if zero cases within the threshold are replaced
  expect_equal(sum(result$confirm == 0, na.rm = TRUE), 0)
})
