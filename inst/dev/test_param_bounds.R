#!/usr/bin/env Rscript
# Test to find correct param_bounds format for pcd_as_stan_data

library(primarycensored)

cat("=== Test 1: Check function defaults ===\n\n")

# Get the function definition
cat("Function signature:\n")
print(formals(pcd_as_stan_data))

cat("\n=== Test 2: Try without param_bounds ===\n\n")

# Create minimal test data
set.seed(123)
delays <- rpois(50, 5) + 1

delay_df <- data.frame(
  delay = delays,
  delay_upper = delays + 1,
  n = 1,
  pwindow = 1,
  relative_obs_time = max(delays) + 10
)

dist_id <- pcd_stan_dist_id("lognormal")
primary_id <- 0

cat("Attempting call WITHOUT param_bounds:\n")
tryCatch({
  result <- pcd_as_stan_data(
    delay_df,
    dist_id = dist_id,
    primary_id = primary_id
  )
  cat("✓ SUCCESS! param_bounds has a default\n")
  cat("\nReturned stan_data structure:\n")
  str(result)
}, error = function(e) {
  cat("✗ FAILED - param_bounds is required\n")
  cat("Error:", e$message, "\n")
})

cat("\n=== Test 3: Look for helper functions ===\n\n")

# Search for functions that might provide defaults
all_fns <- ls("package:primarycensored")
cat("Functions with 'bound' or 'default' in name:\n")
bound_fns <- grep("bound|default|prior", all_fns, value = TRUE, ignore.case = TRUE)
print(bound_fns)

cat("\n=== Test 4: Check for examples ===\n\n")

cat("Trying example(pcd_as_stan_data):\n")
tryCatch({
  example(pcd_as_stan_data, package = "primarycensored")
}, error = function(e) {
  cat("No examples available\n")
})

cat("\n=== Test 5: Examine vignette ===\n\n")

# Get vignette code
vig_path <- system.file("doc", "primarycensored.R", package = "primarycensored")
if (file.exists(vig_path)) {
  cat("Reading vignette code from:", vig_path, "\n")
  code <- readLines(vig_path)

  # Find lines with param_bounds
  matches <- grep("param_bounds|pcd_as_stan_data", code, ignore.case = TRUE)
  if (length(matches) > 0) {
    cat("\nRelevant lines from vignette:\n")
    for (i in matches) {
      start <- max(1, i - 2)
      end <- min(length(code), i + 2)
      cat("\nLines", start, "to", end, ":\n")
      cat(code[start:end], sep = "\n")
      cat("---\n")
    }
  } else {
    cat("No relevant lines found in vignette\n")
  }
} else {
  cat("Vignette R code not found\n")
}

cat("\n=== Test 6: Check distribution-specific requirements ===\n\n")

cat("Lognormal has 2 parameters: meanlog, sdlog\n")
cat("Gamma has 2 parameters: shape, rate (or shape, scale)\n")
cat("Weibull has 2 parameters: shape, scale\n")
cat("\nSo param_bounds probably needs:\n")
cat("  list(lower = c(param1_lower, param2_lower),\n")
cat("       upper = c(param1_upper, param2_upper))\n")

cat("\n=== Test 7: Try with wide bounds ===\n\n")

# Try with very wide, uninformative bounds
wide_bounds <- list(
  lower = c(-100, 0.001),  # For lognormal: meanlog can be negative, sdlog > 0
  upper = c(100, 100)
)

cat("Attempting with wide bounds for lognormal:\n")
print(wide_bounds)

tryCatch({
  result <- pcd_as_stan_data(
    delay_df,
    dist_id = dist_id,
    primary_id = primary_id,
    param_bounds = wide_bounds,
    primary_param_bounds = list()  # Empty for uniform primary
  )
  cat("✓ SUCCESS with wide bounds!\n")
  cat("\nStan data includes:\n")
  cat("Names:", paste(names(result), collapse = ", "), "\n")

  # Check if there's anything about bounds in the result
  if ("param_lower" %in% names(result)) {
    cat("\nparam_lower:", result$param_lower, "\n")
  }
  if ("param_upper" %in% names(result)) {
    cat("param_upper:", result$param_upper, "\n")
  }

}, error = function(e) {
  cat("✗ FAILED even with wide bounds\n")
  cat("Error:", e$message, "\n")
})

cat("\n=== SUMMARY ===\n\n")
cat("To find the answer, check:\n")
cat("1. The output above for default values\n")
cat("2. primarycensored documentation: ?pcd_as_stan_data\n")
cat("3. Vignettes: vignette('primarycensored')\n")
cat("4. Package source: https://github.com/epinowcast/primarycensored\n")
