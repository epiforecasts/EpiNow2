# Include inst/include/epinow2.hpp at the top of each rstan model.
#
# Some EpiNow2 Stan functions are declared in Stan without a body and
# implemented in C++ under inst/include. rstantools includes
# stan_meta_header.hpp inside the model namespace, after the Stan functions
# that call them, so that is too late. This script runs from configure after
# rstantools::rstan_config() and adds the header include just after the Stan
# model header in each generated src/stanExports_*.h.
include_line <- "#include <epinow2.hpp>"
anchor <- "#include <stan/model/model_header.hpp>"
files <- list.files(
  "src",
  pattern = "^stanExports_.*\\.h$", full.names = TRUE
)
for (file in files) {
  code <- readLines(file)
  if (any(code == include_line)) {
    next
  }
  at <- match(anchor, trimws(code))
  if (is.na(at)) {
    # nolint next: undesirable_function_linter.
    stop("Could not find '", anchor, "' in ", file, call. = FALSE)
  }
  writeLines(append(code, include_line, after = at), file)
}
