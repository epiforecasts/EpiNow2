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
  "primarycensored_sone_pmf_vectorized",
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
