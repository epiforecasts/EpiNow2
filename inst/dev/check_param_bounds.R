#!/usr/bin/env Rscript
# Check what param_bounds should look like

library(primarycensored)

cat("=== Check pcd_as_stan_data arguments ===\n\n")

# Get the function formals
cat("Function arguments:\n")
print(names(formals(pcd_as_stan_data)))

cat("\n=== Try calling with minimal args to see error ===\n\n")

delays <- rpois(100, 5) + 1
delay_df <- data.frame(
  delay = delays,
  delay_upper = delays + 1,
  n = 1,
  pwindow = 1,
  relative_obs_time = max(delays) + 10
)

dist_id <- pcd_stan_dist_id("lognormal")
primary_id <- 0

# Try without param_bounds to see what the default should be
tryCatch({
  result <- pcd_as_stan_data(
    delay_df,
    dist_id = dist_id,
    primary_id = primary_id
  )
  cat("Success without param_bounds!\n")
  cat("Structure:\n")
  str(result)
}, error = function(e) {
  cat("Error without param_bounds:\n", e$message, "\n\n")

  # Try with empty bounds
  cat("Trying with empty list...\n")
  tryCatch({
    result <- pcd_as_stan_data(
      delay_df,
      dist_id = dist_id,
      primary_id = primary_id,
      param_bounds = list(),
      primary_param_bounds = list()
    )
    cat("Success with empty list!\n")
  }, error = function(e2) {
    cat("Error with empty list:\n", e2$message, "\n")
  })
})

cat("\n=== Check if there are default bound functions ===\n\n")
fns <- ls("package:primarycensored")
bound_fns <- grep("bound", fns, value = TRUE, ignore.case = TRUE)
cat("Functions with 'bound' in name:\n")
print(bound_fns)
