#!/usr/bin/env Rscript
# Test script to explore primarycensored package functionality
# Run this to understand what functions are available for estimate_dist()

cat("=== Testing primarycensored package ===\n\n")

# Check if primarycensored is installed
if (!requireNamespace("primarycensored", quietly = TRUE)) {
  stop("primarycensored package not installed. Install with:\n",
       "  install.packages('primarycensored')")
}

# Load the package
library(primarycensored)

cat("1. List all exported functions from primarycensored:\n")
cat("---------------------------------------------------\n")
exports <- ls("package:primarycensored")
cat(paste(exports, collapse = "\n"), "\n\n")

# Look for fitting/model related functions
cat("2. Functions that might be relevant for fitting:\n")
cat("-------------------------------------------------\n")
fitting_funcs <- grep("fit|stan|model|cmdstan|data", exports,
                      value = TRUE, ignore.case = TRUE)
cat(paste(fitting_funcs, collapse = "\n"), "\n\n")

# Check for distribution functions (we already know these exist)
cat("3. Distribution functions (d/p/q/r):\n")
cat("------------------------------------\n")
dist_funcs <- grep("^[dpqr]primary", exports, value = TRUE)
cat(paste(dist_funcs, collapse = "\n"), "\n\n")

# Try to get help on key functions
cat("4. Checking for model/data preparation functions:\n")
cat("--------------------------------------------------\n")

# Check if pcd_cmdstan_model exists
if ("pcd_cmdstan_model" %in% exports) {
  cat("✓ pcd_cmdstan_model() exists\n")
  cat("  Help:\n")
  tryCatch({
    print(help(pcd_cmdstan_model, package = "primarycensored"))
  }, error = function(e) {
    cat("  (Help not available)\n")
  })
} else {
  cat("✗ pcd_cmdstan_model() NOT found\n")
}

# Check if pcd_as_stan_data exists
if ("pcd_as_stan_data" %in% exports) {
  cat("✓ pcd_as_stan_data() exists\n")
} else {
  cat("✗ pcd_as_stan_data() NOT found\n")
}

# Check for similar functions
similar <- grep("pcd.*stan|stan.*data|as.*data", exports,
                value = TRUE, ignore.case = TRUE)
if (length(similar) > 0) {
  cat("\nRelated functions found:\n")
  cat(paste("  -", similar, collapse = "\n"), "\n")
}

cat("\n5. Generate example delay data and test dprimarycensored:\n")
cat("----------------------------------------------------------\n")
set.seed(123)
# Simulate some delay observations
true_meanlog <- log(5)
true_sdlog <- 0.5
delays <- rlnorm(100, meanlog = true_meanlog, sdlog = true_sdlog)
delays <- round(delays)
delays <- delays[delays > 0]

cat("Generated", length(delays), "delay observations\n")
cat("Range:", min(delays), "to", max(delays), "\n")
cat("Mean:", mean(delays), "\n\n")

# Test dprimarycensored with lognormal
cat("Testing dprimarycensored with lognormal:\n")
test_x <- 0:10
pmf <- dprimarycensored(
  x = test_x,
  pdist = plnorm,
  pwindow = 1,
  swindow = 1,
  D = 20,
  meanlog = 1.6,
  sdlog = 0.5
)
cat("PMF for delays 0-10:\n")
print(round(pmf, 4))
cat("Sum:", sum(pmf), "\n\n")

cat("6. Check package documentation/vignettes:\n")
cat("------------------------------------------\n")
vigs <- vignette(package = "primarycensored")
if (!is.null(vigs$results) && nrow(vigs$results) > 0) {
  cat("Available vignettes:\n")
  print(vigs$results[, c("Item", "Title")])
} else {
  cat("No vignettes found\n")
}

cat("\n7. Package description:\n")
cat("-----------------------\n")
desc <- packageDescription("primarycensored")
cat("Version:", desc$Version, "\n")
cat("Title:", desc$Title, "\n")
if (!is.null(desc$Description)) {
  cat("Description:", strwrap(desc$Description, width = 70), sep = "\n  ")
}

cat("\n\n=== Test complete ===\n")
cat("\nNext steps:\n")
cat("1. If pcd_cmdstan_model() exists, check its documentation\n")
cat("2. Look at package vignettes for fitting examples\n")
cat("3. Check GitHub repo for more examples\n")
cat("4. Update estimate_dist_prototype.R based on findings\n")
