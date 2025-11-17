#!/usr/bin/env Rscript
# Check the primarycensored vignette for fitting examples

cat("=== Checking primarycensored vignette ===\n\n")

# Try to view the getting started vignette
cat("Opening 'Getting Started' vignette...\n")
cat("You can also view it in a browser with:\n")
cat("  vignette('primarycensored', package = 'primarycensored')\n\n")

# Try to read the vignette source
vignettes <- vignette(package = "primarycensored")
if (!is.null(vignettes$results) && nrow(vignettes$results) > 0) {
  # Get the primarycensored vignette
  vig_path <- system.file("doc", "primarycensored.Rmd",
                          package = "primarycensored")

  if (file.exists(vig_path)) {
    cat("Found vignette source at:", vig_path, "\n")
    cat("First 200 lines of vignette:\n")
    cat("============================\n\n")

    vig_content <- readLines(vig_path, n = 200)
    cat(vig_content, sep = "\n")
  } else {
    cat("Vignette source not found. Try viewing the HTML version:\n")
    cat("  browseVignettes('primarycensored')\n")
  }
} else {
  cat("No vignettes found in installation.\n")
}

cat("\n\n=== Alternative: Check examples from help ===\n\n")

cat("Example from ?pcd_cmdstan_model:\n")
cat("--------------------------------\n")
example_code <- '
# From the help file:
if (!is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
  # Create model
  model <- pcd_cmdstan_model(compile = FALSE)

  # Or with compilation:
  # model <- pcd_cmdstan_model()
}
'
cat(example_code)

cat("\n\nExample workflow (conceptual):\n")
cat("------------------------------\n")
workflow <- '
# 1. Prepare data
delays <- c(...)  # Your delay observations
stan_data <- pcd_as_stan_data(
  delay = delays,
  delay_upper = delays + 1,
  pwindow = 1,
  swindow = 1,
  D = max(delays) + 10
)

# 2. Get/compile model
model <- pcd_cmdstan_model()

# 3. Fit model
fit <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  ...
)

# 4. Extract parameters
draws <- fit$draws()
# Or:
summary <- fit$summary()
'
cat(workflow)

cat("\n\nTo see actual working examples, run:\n")
cat("  browseVignettes('primarycensored')\n")
cat("or visit: https://primarycensored.epinowcast.org/\n")
