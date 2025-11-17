#!/usr/bin/env Rscript
# Check distribution IDs in primarycensored

library(primarycensored)

cat("=== Available distributions ===\n\n")
dists <- pcd_distributions()
print(dists)

cat("\n=== Check pcd_stan_dist_id function ===\n\n")
cat("Function signature:\n")
print(formals(pcd_stan_dist_id))

cat("\nHelp:\n")
help(pcd_stan_dist_id)

cat("\n=== Test distribution IDs ===\n\n")

# Try different distribution names
test_dists <- c("lognormal", "gamma", "LogNormal", "Gamma", "weibull")

for (d in test_dists) {
  tryCatch({
    id <- pcd_stan_dist_id(d)
    cat(d, "->", id, "\n")
  }, error = function(e) {
    cat(d, "-> ERROR:", e$message, "\n")
  })
}

cat("\n=== Check dist_id parameter in pcd_as_stan_data ===\n\n")
args(pcd_as_stan_data)
