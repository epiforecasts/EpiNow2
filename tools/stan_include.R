# Include inst/include/epinow2.hpp before the model code in each rstan
# model. rstantools' stan_meta_header.hpp is included too late for this.
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
