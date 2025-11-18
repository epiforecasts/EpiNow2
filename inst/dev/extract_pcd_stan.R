#!/usr/bin/env Rscript
# Extract primarycensored Stan functions to use with rstan

library(primarycensored)

cat("=== Extracting Stan functions from primarycensored ===\n\n")

# Get the Stan path
stan_path <- pcd_stan_path()
cat("Stan path:", stan_path, "\n\n")

# List all Stan files
stan_files <- list.files(stan_path, pattern = "\\.stan$", full.names = TRUE)
cat("Stan files found:\n")
print(basename(stan_files))

cat("\n=== Reading Stan function files ===\n\n")

# Read each file
for (file in stan_files) {
  cat("\n--- File:", basename(file), "---\n")
  content <- readLines(file)
  cat(content[1:min(30, length(content))], sep = "\n")
  if (length(content) > 30) {
    cat("... (", length(content) - 30, "more lines)\n")
  }
  cat("\n")
}

cat("\n=== Try pcd_load_stan_functions ===\n\n")

if (exists("pcd_load_stan_functions")) {
  # Try to load functions
  tryCatch({
    stan_fns <- pcd_load_stan_functions()
    cat("✓ Successfully loaded Stan functions\n")
    cat("Length:", nchar(stan_fns), "characters\n\n")

    # Show first part
    cat("First 1000 characters:\n")
    cat(substr(stan_fns, 1, 1000), "\n")
    cat("...\n")

    # Save to file for inspection
    writeLines(stan_fns, "inst/stan/primarycensored_functions.stan")
    cat("\n✓ Saved to inst/stan/primarycensored_functions.stan\n")

  }, error = function(e) {
    cat("✗ Error:", e$message, "\n")
  })
} else {
  cat("✗ pcd_load_stan_functions not found\n")
  cat("Trying to manually combine files...\n\n")

  # Manually read and combine
  all_functions <- character()
  for (file in stan_files) {
    content <- readLines(file)
    all_functions <- c(all_functions, content)
  }

  writeLines(all_functions, "inst/stan/primarycensored_functions.stan")
  cat("✓ Combined files saved to inst/stan/primarycensored_functions.stan\n")
}

cat("\n=== Check for distribution functions ===\n\n")

# Look for specific distribution functions we need
search_patterns <- c(
  "primarycensored_lpmf",
  "primarycensored_lcdf",
  "primarycensored_lpdf",
  "double.*censor",
  "truncat"
)

stan_content <- readLines("inst/stan/primarycensored_functions.stan")

for (pattern in search_patterns) {
  matches <- grep(pattern, stan_content, ignore.case = TRUE, value = TRUE)
  if (length(matches) > 0) {
    cat("\nPattern '", pattern, "' found in:\n", sep = "")
    cat(matches[1:min(5, length(matches))], sep = "\n")
    if (length(matches) > 5) cat("... and", length(matches) - 5, "more\n")
  }
}

cat("\n=== CONCLUSION ===\n\n")
cat("Next steps:\n")
cat("1. Review inst/stan/primarycensored_functions.stan\n")
cat("2. Create a Stan model that includes these functions\n")
cat("3. Use the primarycensored distributions in the model block\n")
cat("4. Compile with rstan instead of cmdstanr\n")
