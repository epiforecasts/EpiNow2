#!/usr/bin/env Rscript
# Get exact help for pcd_as_stan_data

library(primarycensored)

cat("=== pcd_as_stan_data Help ===\n\n")
help(pcd_as_stan_data, package = "primarycensored")

cat("\n=== Function formals ===\n")
print(formals(pcd_as_stan_data))

cat("\n=== Try with proper data frame ===\n\n")
set.seed(123)
delays <- rlnorm(100, log(5), 0.5) |> round() |> as.integer()

# Create a proper data frame
delay_data <- data.frame(
  delay = delays,
  delay_upper = delays + 1,
  n = 1  # Each row is one observation
)

cat("Data frame structure:\n")
str(delay_data)

cat("\nAttempting pcd_as_stan_data with data frame...\n")
stan_data <- pcd_as_stan_data(
  delay_data,
  pwindow = 1
)

cat("\nSuccess! Stan data structure:\n")
str(stan_data)

cat("\n\nKey elements:\n")
cat("N:", stan_data$N, "\n")
cat("obs_t:", head(stan_data$obs_t, 10), "\n")
if ("dist_id" %in% names(stan_data)) {
  cat("dist_id:", stan_data$dist_id, "\n")
}
