# Vendor Stan functions from primarycensored into EpiNow2
#
# Usage: Rscript inst/dev/vendor-primarycensored.R

library(primarycensored)

output_file <- "inst/stan/functions/primarycensored.stan"
header_dir <- "inst/include"

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

# The Stan functions that have C++ versions are declared without a body, and
# the header that implements them is copied to inst/include, where
# inst/include/epinow2.hpp includes it. This needs a primarycensored that
# supports `cpp = TRUE`.
if (!"cpp" %in% names(formals(pcd_load_stan_functions))) {
  stop(
    "This primarycensored does not support `cpp = TRUE`. ",
    "Install a version with C++ Stan functions.",
    call. = FALSE
  )
}
invisible(pcd_load_stan_functions(
  functions = funcs,
  dependencies = TRUE,
  write_to_file = TRUE,
  output_file = output_file,
  cpp = TRUE
))
header_files <- pcd_copy_stan_header(header_dir)
cat("Copied header files:", toString(header_files), "\n")

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
