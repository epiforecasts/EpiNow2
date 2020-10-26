
target_dir <- "inst/stan/functions"
function_files <- c("pmfs.stan", "convolve.stan", "gaussian_process.stan",
                   "rt.stan", "infections.stan", "observation_model.stan",
                   "generated_quantities.stan")
functions <- paste0("\n functions{ \n",
                    paste(purrr::map_chr(function_files, 
                                   ~ paste(readLines(file.path(target_dir, .)), collapse = "\n")),
                          collapse = "\n"), 
                    "\n }")

rstan::expose_stan_functions(rstan::stanc(model_code = functions))
