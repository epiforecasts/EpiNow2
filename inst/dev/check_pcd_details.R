#!/usr/bin/env Rscript
# Check details of pcd_as_stan_data and fitting workflow

library(primarycensored)

cat("=== Details of pcd_as_stan_data ===\n\n")

# Get the function signature
cat("Function signature:\n")
cat("------------------\n")
args(pcd_as_stan_data)

# Get help
cat("\nHelp documentation:\n")
cat("-------------------\n")
help(pcd_as_stan_data, package = "primarycensored")

cat("\n=== Check fitdistdoublecens (alternative fitting function) ===\n\n")
args(fitdistdoublecens)
cat("\n")
help(fitdistdoublecens, package = "primarycensored")

cat("\n=== Example: Prepare data and check structure ===\n\n")
set.seed(123)
delays <- rpois(100, lambda = 5) + 1

cat("Creating stan data with pcd_as_stan_data...\n")
stan_data <- pcd_as_stan_data(
  delay = delays,
  delay_upper = delays + 1,
  pwindow = 1,
  swindow = 1,
  D = max(delays) + 10
)

cat("\nStan data structure:\n")
cat("--------------------\n")
cat("Names:", paste(names(stan_data), collapse = ", "), "\n\n")
str(stan_data)

cat("\n=== Key data elements ===\n")
cat("N (number of obs):", stan_data$N, "\n")
cat("n (sample size):", stan_data$n, "\n")
cat("n_obs:", stan_data$n_obs, "\n")
cat("primary_dist:", stan_data$primary_dist, "\n")
cat("primary_dist_id:", stan_data$primary_dist_id, "\n")

cat("\n=== Check what dist_id means ===\n")
cat("Available distributions from pcd_distributions():\n")
dists <- pcd_distributions()
print(dists)

cat("\n=== Check Stan model parameters ===\n")
cat("For this we need to look at the actual Stan file or fit a model\n")
cat("Stan files location:\n")
print(pcd_stan_path())

cat("\nStan function files:\n")
print(pcd_stan_functions())

cat("\n=== Try to inspect Stan model code ===\n")
stan_files <- pcd_stan_files()
cat("Main model file:\n")
print(stan_files)

# Try to read the model file
if (length(stan_files) > 0) {
  cat("\nReading main Stan model file:\n")
  cat("------------------------------\n")
  model_code <- readLines(stan_files[1], n = 100)
  # Look for parameters block
  param_start <- grep("parameters \\{", model_code)
  if (length(param_start) > 0) {
    cat("Parameters block (first 20 lines):\n")
    cat(model_code[param_start:(min(param_start + 20, length(model_code)))], sep = "\n")
  }
}
