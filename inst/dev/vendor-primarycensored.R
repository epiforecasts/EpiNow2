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

# The Stan functions that have C++ versions are declared without a body.
# primarycensored copies the header that implements them next to the Stan
# file, so both are written to a temporary directory first. The header goes
# to inst/include, where inst/include/epinow2.hpp includes it. This needs a
# primarycensored with C++ Stan functions.
if (!exists("pcd_stan_header", envir = asNamespace("primarycensored"))) {
  stop(
    "This primarycensored has no C++ Stan functions. ",
    "Install a version that does.",
    call. = FALSE
  )
}
vendor_dir <- tempfile("primarycensored")
dir.create(vendor_dir)
invisible(pcd_load_stan_functions(
  functions = funcs,
  dependencies = TRUE,
  write_to_file = TRUE,
  output_file = file.path(vendor_dir, basename(output_file))
))
invisible(file.copy(
  file.path(vendor_dir, basename(output_file)), output_file,
  overwrite = TRUE
))
header <- setdiff(list.files(vendor_dir), basename(output_file))
if (length(header) == 0) {
  stop("primarycensored did not write a C++ header.", call. = FALSE)
}
invisible(file.copy(
  file.path(vendor_dir, header), header_dir,
  recursive = TRUE, overwrite = TRUE
))
cat("Copied header files to", header_dir, ":", toString(header), "\n")

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
