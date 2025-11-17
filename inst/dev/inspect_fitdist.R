#!/usr/bin/env Rscript
# Inspect fitdistdoublecens to see how it calls pcd_as_stan_data

library(primarycensored)

cat("=== Source code of fitdistdoublecens ===\n\n")
print(fitdistdoublecens)

cat("\n\n=== Method dispatch for fitdistdoublecens ===\n\n")
methods(fitdistdoublecens)

cat("\n\n=== Arguments of fitdistdoublecens ===\n\n")
args(fitdistdoublecens)

cat("\n\n=== Try running with example data ===\n\n")

# Create example data
set.seed(123)
delays <- rpois(100, 5) + 1

# Create data frame as fitdistdoublecens might expect
delay_data <- data.frame(
  delay_lower = delays - 1,
  delay_upper = delays,
  n = rep(1, length(delays))
)

cat("Data structure:\n")
str(delay_data)

cat("\n=== Try calling fitdistdoublecens ===\n\n")
cat("This will show us what parameters it passes internally...\n\n")

tryCatch({
  # Try with minimal args
  result <- fitdistdoublecens(
    delay_data,
    dist = "lnorm",
    samples = 100,  # Small number for quick test
    chains = 1
  )
  cat("Success! Result structure:\n")
  str(result)
}, error = function(e) {
  cat("Error:", e$message, "\n")
  cat("This tells us what's missing...\n")
})
