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

# Versions of primarycensored with C++ versions of some Stan functions
# declare those without a body and ship a header that implements them.
# inst/include/epinow2.hpp includes the header.
has_cpp <- "cpp" %in% names(formals(pcd_load_stan_functions))
load_args <- list(
  functions = funcs,
  dependencies = TRUE,
  write_to_file = TRUE,
  output_file = output_file
)
if (has_cpp) {
  load_args$cpp <- TRUE
}
invisible(do.call(pcd_load_stan_functions, load_args))
if (has_cpp) {
  header_files <- pcd_copy_stan_header(header_dir)
  cat("Copied header files:", toString(header_files), "\n")
}

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
