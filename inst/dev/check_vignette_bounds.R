#!/usr/bin/env Rscript
# Check primarycensored vignette for proper param_bounds usage

library(primarycensored)

cat("=== Check vignette for examples ===\n\n")

# Get vignette
vig <- vignette("primarycensored", package = "primarycensored")

# Try to get the code from the vignette
cat("Vignette info:\n")
print(vig)

cat("\n=== Check function help for param_bounds ===\n\n")
cat("Running: ?pcd_as_stan_data\n\n")

# Check the function defaults
cat("Function formals:\n")
formals_list <- formals(pcd_as_stan_data)
print(formals_list$param_bounds)
print(formals_list$primary_param_bounds)

cat("\n=== Try viewing the actual vignette source ===\n\n")

# Find vignette file
pkg_path <- system.file(package = "primarycensored")
cat("Package path:", pkg_path, "\n")

vignette_dir <- file.path(pkg_path, "doc")
if (dir.exists(vignette_dir)) {
  vignette_files <- list.files(vignette_dir, pattern = "primarycensored")
  cat("Vignette files found:\n")
  print(vignette_files)

  # Try to find R code file
  r_file <- file.path(vignette_dir, "primarycensored.R")
  if (file.exists(r_file)) {
    cat("\n=== Vignette R code (first 200 lines) ===\n")
    code <- readLines(r_file, n = 200)
    # Find lines with pcd_as_stan_data or param_bounds
    relevant_lines <- grep("pcd_as_stan_data|param_bounds|priors", code,
                           ignore.case = TRUE, value = FALSE)
    if (length(relevant_lines) > 0) {
      cat("Relevant lines found at:", relevant_lines, "\n\n")
      # Show context around those lines
      for (line_num in relevant_lines) {
        start <- max(1, line_num - 3)
        end <- min(length(code), line_num + 3)
        cat("Lines", start, "-", end, ":\n")
        cat(code[start:end], sep = "\n")
        cat("\n---\n")
      }
    }
  }
}

cat("\n=== Check if there are example/helper functions ===\n\n")

# Look for functions that might set defaults
fns <- ls("package:primarycensored")
default_fns <- grep("default|bound", fns, value = TRUE, ignore.case = TRUE)
cat("Functions with 'default' or 'bound':\n")
print(default_fns)
