#!/usr/bin/env Rscript
# Find Stan fitting examples in primarycensored

library(primarycensored)

cat("=== Check for Stan-related vignettes ===\n\n")
vigs <- vignette(package = "primarycensored")
print(vigs$results[, c("Item", "Title")])

cat("\n=== Check 'using-stan-tools' or 'fitting-dists-with-stan' vignette ===\n\n")

# Try to find the fitting vignettes
pkg_path <- system.file(package = "primarycensored")
doc_dir <- file.path(pkg_path, "doc")

if (dir.exists(doc_dir)) {
  all_files <- list.files(doc_dir)
  cat("All doc files:\n")
  print(all_files)

  # Look for stan-related files
  stan_files <- grep("stan|fit", all_files, ignore.case = TRUE, value = TRUE)
  cat("\nStan-related files:\n")
  print(stan_files)

  # Try to read the R code from fitting vignette if it exists
  for (file in stan_files) {
    if (grepl("\\.R$", file)) {
      cat("\n=== Contents of", file, "===\n")
      code <- readLines(file.path(doc_dir, file))
      # Find pcd_as_stan_data usage
      relevant <- grep("pcd_as_stan_data|param_bounds", code, value = TRUE)
      if (length(relevant) > 0) {
        cat("Lines with pcd_as_stan_data:\n")
        cat(relevant, sep = "\n")
      }
    }
  }
}

cat("\n=== Try help on fitdistdoublecens ===\n\n")
cat("This might show an example of fitting:\n")
?fitdistdoublecens

cat("\n=== Look for test files that might show usage ===\n\n")

# Check if we can access package tests
test_path <- system.file("tests", package = "primarycensored")
if (dir.exists(test_path)) {
  cat("Found tests at:", test_path, "\n")
  test_files <- list.files(test_path, recursive = TRUE, pattern = "\\.R$")
  cat("Test files:\n")
  print(test_files)
}

cat("\n=== Check package examples ===\n\n")

# Try to run example from pcd_as_stan_data if it exists
cat("Trying example(pcd_as_stan_data)...\n")
tryCatch({
  example(pcd_as_stan_data, package = "primarycensored")
}, error = function(e) {
  cat("No examples found\n")
})

cat("\n=== Try example from fitdistdoublecens ===\n\n")
tryCatch({
  example(fitdistdoublecens, package = "primarycensored")
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
