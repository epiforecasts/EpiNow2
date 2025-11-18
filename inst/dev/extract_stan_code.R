#!/usr/bin/env Rscript
# Extract Stan code from primarycensored to use with rstan

library(primarycensored)

cat("=== Extracting Stan functions from primarycensored ===\n\n")

# Check what's available
cat("Available functions in primarycensored:\n")
fns <- ls("package:primarycensored")
stan_fns <- grep("stan", fns, value = TRUE, ignore.case = TRUE)
print(stan_fns)

cat("\n=== Check pcd_load_stan_functions ===\n\n")

if ("pcd_load_stan_functions" %in% fns) {
  cat("✓ pcd_load_stan_functions exists\n")
  cat("\nFunction signature:\n")
  print(args(pcd_load_stan_functions))

  cat("\n=== Try loading Stan functions ===\n\n")

  # Try to get the Stan functions
  tryCatch({
    stan_code <- pcd_load_stan_functions()
    cat("✓ Successfully loaded Stan functions\n")
    cat("\nStan code length:", nchar(stan_code), "characters\n")
    cat("\nFirst 500 characters:\n")
    cat(substr(stan_code, 1, 500), "\n...")

    # Save to file
    outfile <- "inst/dev/primarycensored_functions.stan"
    cat("\nSaving to:", outfile, "\n")
    writeLines(stan_code, outfile)
    cat("✓ Saved\n")

  }, error = function(e) {
    cat("✗ Error:", e$message, "\n")
  })

} else {
  cat("✗ pcd_load_stan_functions not found\n")
}

cat("\n=== Check pcd_stan_path ===\n\n")

if ("pcd_stan_path" %in% fns) {
  cat("✓ pcd_stan_path exists\n")
  stan_path <- pcd_stan_path()
  cat("Stan path:", stan_path, "\n")

  # List files in that directory
  if (dir.exists(stan_path)) {
    cat("\nFiles in Stan directory:\n")
    stan_files <- list.files(stan_path, pattern = "\\.stan$", full.names = FALSE)
    print(stan_files)

    # Read one of the files to see structure
    if (length(stan_files) > 0) {
      cat("\n=== Contents of first Stan file ===\n")
      first_file <- file.path(stan_path, stan_files[1])
      cat("File:", first_file, "\n\n")
      content <- readLines(first_file, n = 50)
      cat(content, sep = "\n")
    }
  }
} else {
  cat("✗ pcd_stan_path not found\n")
}

cat("\n=== Check if we can compile with rstan ===\n\n")

if (requireNamespace("rstan", quietly = TRUE)) {
  cat("✓ rstan is available\n")
  cat("\nCould we use primarycensored Stan code with rstan?\n")
  cat("YES - if we:\n")
  cat("  1. Extract Stan functions using pcd_load_stan_functions()\n")
  cat("  2. Create a Stan model that includes these functions\n")
  cat("  3. Compile with rstan::stan_model()\n")
  cat("  4. Use the same data format as primarycensored expects\n")
} else {
  cat("✗ rstan not available\n")
}

cat("\n=== CONCLUSION ===\n\n")
cat("We can use primarycensored Stan code with rstan by:\n")
cat("1. Getting Stan functions via pcd_load_stan_functions() or pcd_stan_path()\n")
cat("2. Creating our own Stan model that uses these functions\n")
cat("3. Compiling with rstan instead of cmdstanr\n")
cat("4. This would avoid cmdstanr dependency!\n")
