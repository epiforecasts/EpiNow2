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

invisible(pcd_load_stan_functions(
  functions = funcs,
  dependencies = TRUE,
  write_to_file = TRUE,
  output_file = output_file
))

# Patches applied to the generated file
#
# These keep the vendored functions usable by Stan interfaces that interpret
# the model rather than compiling it. Each patch must still apply, so a
# failure here is a signal that upstream has changed and the patch needs
# revisiting rather than something to skip.
patches <- list(
  list(
    # An early return inside a branch on a parameter-dependent value has no
    # compiled path in `stanli` when the function is reached from an ODE
    # right-hand side, as it is here through `dist_lcdf`. The single-exit
    # form is equivalent because Stan's `||` short-circuits, so `log(y)` is
    # still never evaluated for non-positive `y`.
    name = "lognormal_lcdf_underflows single exit",
    from = paste(
      "int lognormal_lcdf_underflows(real y, real mu, real sigma) {",
      "  if (y <= 0) {",
      "    return 1;",
      "  }",
      "  return (log(y) - mu) / sigma < -38 ? 1 : 0;",
      "}",
      sep = "\n"
    ),
    to = paste(
      "int lognormal_lcdf_underflows(real y, real mu, real sigma) {",
      "  return (y <= 0 || (log(y) - mu) / sigma < -38) ? 1 : 0;",
      "}",
      sep = "\n"
    )
  )
)

contents <- paste(readLines(output_file), collapse = "\n")
for (patch in patches) {
  if (!grepl(patch$from, contents, fixed = TRUE)) {
    stop(
      "Patch '", patch$name, "' no longer applies to the vendored file. ",
      "Check whether upstream has changed and update or drop the patch.",
      call. = FALSE
    )
  }
  contents <- sub(patch$from, patch$to, contents, fixed = TRUE)
  cat("Applied patch:", patch$name, "\n")
}
writeLines(contents, output_file)

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
