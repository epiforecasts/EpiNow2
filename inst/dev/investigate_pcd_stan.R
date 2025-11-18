#!/usr/bin/env Rscript
# Investigate primarycensored Stan functions to use them properly

library(primarycensored)

cat("=== Investigating primarycensored Stan implementation ===\n\n")

# Get Stan path
stan_path <- pcd_stan_path()
cat("Stan path:", stan_path, "\n\n")

# List files
stan_files <- list.files(stan_path, pattern = "\\.stan$", full.names = FALSE)
cat("Stan files available:\n")
for (f in stan_files) {
  cat(" -", f, "\n")
}

cat("\n=== Reading Stan files to understand structure ===\n\n")

# Read each file
for (f in stan_files) {
  full_path <- file.path(stan_path, f)
  cat("\n==== File:", f, "====\n")
  content <- readLines(full_path)
  cat(paste(content, collapse = "\n"), "\n")
  cat("\n")
}

cat("\n=== Looking for distribution PMF/LPM functions ===\n\n")

# Search for lpmf, lcdf, etc
all_content <- character()
for (f in stan_files) {
  content <- readLines(file.path(stan_path, f))
  all_content <- c(all_content, content)
}

patterns <- c(
  "_lpmf\\(",
  "_lcdf\\(",
  "real.*lognormal",
  "real.*gamma",
  "primary.*censor"
)

for (pattern in patterns) {
  matches <- grep(pattern, all_content, value = TRUE)
  if (length(matches) > 0) {
    cat("\nPattern '", pattern, "':\n", sep = "")
    for (m in unique(matches)) {
      cat("  ", trimws(m), "\n", sep = "")
    }
  }
}

cat("\n=== Check if pcd_load_stan_functions exists ===\n\n")

if (exists("pcd_load_stan_functions", where = "package:primarycensored")) {
  cat("✓ pcd_load_stan_functions exists\n")
  cat("Attempting to load...\n\n")

  stan_fns <- pcd_load_stan_functions()
  cat("Loaded", nchar(stan_fns), "characters of Stan code\n\n")

  # Save for inspection
  writeLines(stan_fns, "inst/stan/primarycensored_extracted.stan")
  cat("Saved to inst/stan/primarycensored_extracted.stan\n\n")

  # Look for function signatures
  cat("Function signatures found:\n")
  fn_sigs <- grep("real.*\\(", strsplit(stan_fns, "\n")[[1]], value = TRUE)
  for (sig in fn_sigs[1:min(20, length(fn_sigs))]) {
    cat("  ", trimws(sig), "\n", sep = "")
  }

} else {
  cat("✗ pcd_load_stan_functions not found\n")
}

cat("\n=== Summary for estimate_dist implementation ===\n\n")
cat("To use primarycensored with rstan:\n")
cat("1. Use pcd_stan_path() to get include directory\n")
cat("2. Pass as include_paths to rstan::stan_model()\n")
cat("3. In Stan file, use #include to load the functions\n")
cat("4. Call the appropriate pmf/lpdf functions\n")
cat("\nNext: Review the extracted Stan code to find exact function names\n")
