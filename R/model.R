## adapted from:
## https://github.com/epinowcast/epinowcast/blob/HEAD/R/model-tools.R#L321
## COPYRIGHT HOLDER: epinowcast authors
## MIT License
epinow2_model <- function(model = "estimate_infections",
                          include = system.file("stan", package = "EpiNow2"),
                          compile = TRUE, threads = FALSE, target_dir = tempdir(),
                          stanc_options = list(), cpp_options = list(), verbose = FALSE,
                          ...) {
  model_file <- system.file(
    "stan", paste0(model, ".stan"),
    package = "EpiNow2"
  )
  if (verbose) {
    message(sprintf("Using model %s.", model))
    message(sprintf("include is %s.", paste(include, collapse = ", ")))
  }

  if (compile) {
    monitor <- suppressMessages
    if (verbose) {
      monitor <- function(x) {
        return(x)
      }
    }
    cpp_options$stan_threads <- threads
    model <- monitor(cmdstanr::cmdstan_model(
      model_file,
      include_paths = include,
      stanc_options = stanc_options,
      cpp_options = cpp_options,
      ...
    ))
  }
  return(model)
}
