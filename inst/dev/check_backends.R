#!/usr/bin/env Rscript
# Check what backends are supported

library(primarycensored)

cat("=== Check fitdistdoublecens arguments ===\n\n")
args(fitdistdoublecens)

cat("\n=== Check fitdistdoublecens source ===\n\n")
# Look at the function to see if it has backend options
fitdistdoublecens

cat("\n=== Check for method dispatch ===\n\n")
methods(fitdistdoublecens)

cat("\n=== Can we extract Stan code from primarycensored? ===\n\n")

# Check if we can get the Stan code
stan_files <- pcd_stan_files()
cat("Stan model files:\n")
print(stan_files)

if (length(stan_files) > 0) {
  cat("\nMain model file exists at:", stan_files[1], "\n")
  cat("We could potentially compile this with rstan...\n")
}

cat("\n=== Check Stan function path ===\n\n")
stan_path <- pcd_stan_path()
cat("Stan include path:", stan_path, "\n")
cat("This contains the Stan functions we'd need\n")

cat("\n=== Workaround for rstan: read_stan_csv ===\n\n")
cat("CmdStan output can be converted to rstan stanfit using:\n")
cat("  rstan::read_stan_csv(fit$output_files())\n")
cat("This allows fitting with cmdstanr but post-processing with rstan\n")

cat("\n=== Check if primarycensored has rstan support ===\n\n")
fns <- ls("package:primarycensored")
rstan_fns <- grep("rstan|stanfit", fns, value = TRUE, ignore.case = TRUE)
if (length(rstan_fns) > 0) {
  cat("Functions with rstan in name:\n")
  print(rstan_fns)
} else {
  cat("No rstan-specific functions found\n")
}

cat("\n=== Summary ===\n\n")
cat("Options for rstan support:\n")
cat("1. Use cmdstanr for fitting, convert to stanfit with read_stan_csv\n")
cat("2. Extract Stan code from primarycensored and compile with rstan\n")
cat("3. Keep old dist_fit.stan for rstan backend\n")
cat("4. Make estimate_dist() cmdstanr-only, keep estimate_delay() for rstan\n")
