# Vendor Stan functions from primarycensored into EpiNow2
#
# Usage: Rscript inst/dev/vendor-primarycensored.R

library(primarycensored)

output_file <- "inst/stan/functions/primarycensored.stan"

# primarycensored_ode, dist_lcdf, primary_lpdf, and
# expgrowth_lpdf are passed by reference (e.g. to ode_rk45)
# so the dependency resolver does not detect them
# automatically. We list them explicitly here.
funcs <- c(
  "primarycensored_lpmf",
  "primarycensored_ode",
  "dist_lcdf",
  "primary_lpdf",
  "expgrowth_lpdf"
)

stan_code <- pcd_load_stan_functions(
  functions = funcs,
  dependencies = TRUE,
  write_to_file = TRUE,
  output_file = output_file
)

# Workaround for epinowcast/primarycensored#295:
# pcd_load_stan_functions() appends _lcdf to
# truncation_bounds functions that return vector, breaking
# rstan. Rename them until the upstream fix lands.
code <- readLines(output_file)
code <- gsub(
  "primarycensored_analytical_truncation_bounds_lcdf",
  "primarycensored_analytical_truncation_bounds",
  code
)
code <- gsub(
  "primarycensored_truncation_bounds_lcdf",
  "primarycensored_truncation_bounds",
  code
)
writeLines(code, output_file)

version <- packageVersion("primarycensored")
all_funcs <- unique(unlist(lapply(
  funcs, pcd_stan_function_deps
)))
cat(
  "Vendored", length(all_funcs), "Stan functions",
  "from primarycensored", as.character(version), "\n"
)
cat("Functions:", paste(all_funcs, collapse = ", "), "\n")
cat("Written to:", output_file, "\n")
