#' Clean Nowcasts for a Supplied Date
#'
#' @description This function removes nowcasts in the format produced by `EpiNow2` from a target
#' directory for the date supplied.
#' @param date Date object. Defaults to today's date
#' @param nowcast_dir Character string giving the filepath to the nowcast results directory. Defaults 
#' to the current directory.
#' @importFrom purrr walk
#' @importFrom futile.logger flog.info
#' @return NULL
#' @export
clean_nowcasts <- function(date = NULL, nowcast_dir = ".") {
  if (is.null(date)) {
    date <- Sys.Date()
  }
  dirs <- list.dirs(nowcast_dir, recursive = FALSE)
  purrr::walk(dirs,
              function(dir) {
                remove_dir <- file.path(dir, date)
                if (dir.exists(remove_dir)) {
                  futile.logger::flog.info("Removing files from: %s", remove_dir)
                  lapply(list.files(file.path(remove_dir)),
                         function(file){
                           file.remove(
                             file.path(remove_dir, file)
                           )
                         })
                }
                
              })
  
  
}

#' Format Credible Intervals
#' 
#' @param value List of value to map into a string. Requires,
#'  `point`, `lower`, and `upper.`
#' @param CrI Numeric, credible interval to report. Defaults to 90
#' @param reverse Logical, defaults to FALSE. Should the reported 
#' credible interval be switched.
#' @return A character vector formatted for reporting
#' @export
#' @examples
#' value <- list(median = 2, lower_90 = 1, upper_90 = 3)
#' make_conf(value)
make_conf <- function(value, CrI = 90, reverse = FALSE) {
  CrI <- list(lower = value[[paste0("lower_", CrI)]],
              upper = value[[paste0("upper_", CrI)]])
  conf <- paste0(value$median, " (", 
                 ifelse(!reverse, CrI$lower, CrI$upper), " -- ", 
                 ifelse(!reverse, CrI$upper, CrI$lower), ")")
  return(conf)
}


#' Categorise the Probability of Change for Rt
#'
#' @description Categorises a numeric variable into "Increasing" (< 0.05), 
#' "Likely increasing" (<0.2), "Unsure" (< 0.8), "Likely decreasing" (< 0.95), "Decreasing" (<= 1)
#' @param var Numeric variable to be categorised
#'
#' @return A character variable.
#' @export
#' @examples
#' var <- seq(0.01, 1, 0.01)
#' var
#'  
#' map_prob_change(var)
map_prob_change <- function(var) {
  var <- ifelse(var < 0.05, "Increasing",
                ifelse(var < 0.2, "Likely increasing",
                       ifelse(var < 0.8, "Unsure",
                              ifelse(var < 0.95, "Likely decreasing",
                                     "Decreasing"))))
  var <- factor(var, levels = c("Increasing", "Likely increasing", "Unsure", 
                                "Likely decreasing", "Decreasing"))
  return(var)
}

#' Convert Growth Rates to Reproduction numbers.
#'
#' @description See [here](https://www.medrxiv.org/content/10.1101/2020.01.30.20019877v3.full.pdf) 
#' for justification.
#' @param r Numeric, rate of growth estimates
#' @param gamma_mean Numeric, mean of the gamma distribution
#' @param gamma_sd Numeric, standard deviation of the gamma distribution
#' @return Numeric vector of reproduction number estimates
#' @export
#' @examples
#' growth_to_R(0.2, 4, 1)
growth_to_R <- function(r, gamma_mean, gamma_sd) {
  k <- (gamma_sd / gamma_mean)^2
  R <- (1 + k * r * gamma_mean)^(1/k)
  return(R)
}
  
#' Convert Reproduction Numbers to Growth Rates
#'
#' @description See [here](https://www.medrxiv.org/content/10.1101/2020.01.30.20019877v3.full.pdf) 
#' for justification.
#' @param R Numeric, Reproduction number estimates
#' @inheritParams growth_to_R
#' @return Numeric vector of reproduction number estimates
#' @export
#' @examples
#' R_to_growth(2.18, 4, 1)  
R_to_growth <- function(R, gamma_mean, gamma_sd) {
  k <- (gamma_sd / gamma_mean)^2
  r <- (R^k - 1) / (k * gamma_mean)
  return(r)
}  


#' Allocate Delays into Required Stan Format
#'
#' @param delay_var List of numeric delays
#' @param no_delays Numeric, number of delays
#' @return A numeric array
allocate_delays <- function(delay_var, no_delays) {
  if (no_delays > 0) {
    out <- unlist(delay_var)
  }else{
    out <- numeric(0)
  }
  return(array(out))
}

#' Match Input Output Arguments with Supported Options
#'
#' @param input_args A character vector of input arguments (can be partial).
#' @param supported_args A character vector of supported output arguments.
#' @param logger A character vector indicating the logger to target messages at. Defaults 
#' to no logging.
#' @param level Character string defaulting to "info". Logging level see documentation 
#' of futile.logger for details. Supported options are "info" and "debug"
#'
#' @return A logical vector of named output arguments
#' @export
#' @importFrom  futile.logger flog.info flog.debug
#' @examples
#' # select nothing
#' match_output_arguments(supported_args = c("fit", "plots", "samples"))
#' 
#' # select just plots
#' match_output_arguments("plots", supported_args = c("fit", "plots", "samples"))
#' 
#' # select plots and samples
#' match_output_arguments(c("plots", "samples"),
#'                        supported_args = c("fit", "plots", "samples"))
#' 
#' # lazily select arguments
#' match_output_arguments("p",
#'                        supported_args = c("fit", "plots", "samples"))
match_output_arguments <- function(input_args = c(),
                                   supported_args =  c(),
                                   logger = NULL,
                                   level = "info") {
  
  if (level %in% "info") {
    flog_fn <- futile.logger::flog.info
  }else if (level %in% "debug") {
    flog_fn <- futile.logger::flog.debug
  }
  # make supported args a logical vector
  output_args <- rep(FALSE, length(supported_args))
  names(output_args) <- supported_args
  
  # get arguments supplied and linked to supported args
  found_args <- lapply(input_args, function(arg){
    supported_args[grepl(arg, supported_args)]
  })
  found_args <- unlist(found_args)
  found_args <- unique(found_args)
  
  # tell the user about what has been passed in
  if (!is.null(logger)) {
    if (length(found_args) > 0) {
      flog_fn("Producing following optional outputs: %s",
              paste(found_args, collapse = ", "),
              name = logger)
    }else{
      flog_fn("No optional output specified",
              name = logger)
    }
  }
  # assign true false to supported arguments based on found arguments
  output_args[names(output_args) %in% found_args] <- TRUE
  return(output_args)
}

# Safe Directory Create
# Used to create a directory for the functions

safe_dir_create <- function(dir_name){
  dir_of_interest <- file.path(dir_name)
  if(dir.exists(dir_of_interest)){
    invisible()
  } else {
    dir.create(dir_of_interest, recursive = TRUE)
  }
}

# Safe Copy Stan Model
# This copies over all of the associated functions and stan files
# when the user users the internal model. To be used as internal function only.
# Users shouldn't need to access this directly.

safe_file_copy <- function(files, x){
  tryCatch(file.copy(files, x, overwrite = FALSE),
           error = message("File Exists, Using Existing File\n"))
}

copy_models <- function(dir_path){
  # First copy functions
  safe_dir_create(file.path(dir_path, "functions"))
  
  app_file_loc <- system.file(file.path("stan", "functions"), package = "EpiNow2")
  
  functions_to_copy <- list.files(app_file_loc, full.names = TRUE)
  
  lapply(functions_to_copy, safe_file_copy, x = file.path(dir_path, "functions"))
  # Then Copy data
  safe_dir_create(file.path(dir_path, "data"))
  
  app_file_loc <- system.file(file.path("stan", "data"), package = "EpiNow2")
  
  functions_to_copy <- list.files(app_file_loc, full.names = TRUE)
  
  lapply(functions_to_copy, safe_file_copy, x = file.path(dir_path, "data"))
  
  # Copy Models
  stan_file_loc <- system.file(file.path("stan", "estimate_infections.stan"), 
                               package = "EpiNow2")
  
  if(!file.exists(file.path(dir_path,
                       "estimate_infections.stan"))){
    tryCatch(R.utils::copyFile(stan_file_loc, file.path(dir_path,
                                                        "estimate_infections.stan"), overwrite = FALSE),
             error = message("Model already exists"))
  }
  
}


# Helper function for Formatting List for CmdStanR
make_cmdstan_list <-function(stan_args, method = "sampling"){

  if (method == "sampling") {
    out <- list(
      data = stan_args$data,
      seed = as.integer(abs(stan_args$seed)),
      refresh = as.integer(abs(stan_args$refresh)),
      init = stan_args$init,
      save_latent_dynamics = ifelse(!is.null(stan_args$save_latent_dynamics) & is.logical(stan_args$save_latent_dynamics), stan_args$save_latent_dynamics, FALSE),
      output_dir = stan_args$output_dir,
      chains = as.integer(stan_args$chains),
      parallel_chains = stan_args$parallel_chains,
      threads_per_chain = stan_args$threads_per_chain,
      iter_warmup = stan_args$iter_warmup,
      iter_sampling = stan_args$iter_sampling,
      save_warmup = stan_args$save_warmup,
      thin = stan_args$thin,
      max_treedepth = stan_args$max_treedepth,
      adapt_engaged = ifelse(!is.null(stan_args$adapt_engaged) & is.logical(stan_args$adapt_engaged), stan_args$adapt_engaged, TRUE),
      adapt_delta = stan_args$adapt_delta,
      step_size = stan_args$step_size,
      metric = stan_args$metric,
      metric_file = stan_args$metric_file,
      inv_metric = stan_args$inv_metric,
      init_buffer = stan_args$init_buffer,
      term_buffer = stan_args$term_buffer,
      window = stan_args$window,
      fixed_param = ifelse(!is.null(stan_args$fixed_param) & is.logical(stan_args$fixed_param), stan_args$fixed_param, FALSE),
      validate_csv = ifelse(!is.null(stan_args$validate_csv) & is.logical(stan_args$validate_csv), stan_args$validate_csv, TRUE),
      show_messages = ifelse(!is.null(stan_args$verbose) & is.logical(stan_args$verbose), stan_args$verbose, TRUE)
    )
  } else {
    out <- list(
      data = stan_args$data,
      seed = as.integer(abs(stan_args$seed)),
      refresh = as.integer(abs(stan_args$refresh)),
      init = stan_args$init,
      save_latent_dynamics = ifelse(!is.null(stan_args$save_latent_dynamics) & is.logical(stan_args$save_latent_dynamics), stan_args$save_latent_dynamics, FALSE),
      output_dir = stan_args$output_dir,
      algorithm = stan_args$algorithm,
      iter = stan_args$iter,
      grad_samples = stan_args$grad_samples,
      elbo_samples = stan_args$elbo_samples,
      eta = stan_args$eta,
      adapt_engaged = ifelse(!is.null(stan_args$adapt_engaged) & is.logical(stan_args$adapt_engaged), stan_args$adapt_engaged, TRUE),
      adapt_iter = stan_args$adapt_iter,
      tol_rel_obj = stan_args$tol_rel_obj,
      eval_elbo = stan_args$eval_elbo,
      output_samples = stan_args$output_samples
    )
  }
  
  out
  
} 


# Backends

backends_available <- function(){
  c("rstan", "cmdstan")
}

# Algorithms Available

algoriths_available <- function(){
  c("sampling", "meanfield", "fullrank")
}

#' Clear EpiNow2 Cache
#' 
#' This function clears anything that may exist in the EpiNow2 Cache. Most
#' Commonly if you are using the CmdStan backend, you'll have items in your 
#' cache.
#' @section Warning:
#' This will removed cached models and requires that `rappdirs` is installed
#' @export 
#' @returns invisible

clear_epinow2_cache <- function(){
  requireNamespace("rappdirs")
  app_loc <- rappdirs::user_cache_dir("EpiNow2")
  unlink(app_loc, recursive = TRUE)
  invisible()
}

#' Expose internal package stan functions in R
#'
#' @description This function exposes internal stan functions in R from a user
#' supplid list of target files. 
#' @param files A character vector indicating the target files
#' @param target_dir A character string indicating the target directory for the file
#' @param ... Additional arguments passed to `rstan::expose_stan_functions`.
#' @return NULL
#' @export
#' @importFrom rstan expose_stan_functions stanc
#' @importFrom purrr map_chr
#' @examples
#' \donttest{
#' expose_stan_fns("rt.stan", target_dir = system.file("stan/functions", package = "EpiNow2"))
#' 
#' # test by updating Rt
#' update_Rt(rep(1, 10), log(1.2), rep(0.1, 9), rep(10, 0), numeric(0), 0)
#' }
expose_stan_fns <- function(files, target_dir, ...) {
  functions <- paste0("\n functions{ \n",
                      paste(purrr::map_chr(files, 
                                           ~ paste(readLines(file.path(target_dir, .)), collapse = "\n")),
                            collapse = "\n"), 
                      "\n }")
  rstan::expose_stan_functions(rstan::stanc(model_code = functions), ...)
  return(invisible(NULL))

}

#' @importFrom stats glm median na.omit pexp pgamma plnorm quasipoisson rexp rgamma rlnorm rnorm rpois runif sd var
globalVariables(
  c("bottom", "cases", "confidence", "confirm", "country_code", "crps", 
    "cum_cases", "Date", "date_confirm", "date_confirmation", "date_onset", 
    "date_onset_sample", "date_onset_symptoms", "date_onset.x", "date_onset.y",
    "date_report", "day", "doubling_time", "effect",  "Effective reproduction no.",
    "estimates", "Expected change in daily cases", "fit_meas", "goodness_of_fit", 
    "gt_sample", "import_status", "imported", "index", "latest", "little_r", 
    "lower", "max_time", "mean_R", "Mean(R)", "metric", "mid_lower", "mid_upper",
    "min_time", "model", "modifier", "n", "New", "confirmed cases by infection date",
    "overall_little_r", "params", "prob_control", "provnum_ne", "R0_range",
    "region", "region_code", "report_delay", "results_dir", "rt", "rt_type",
    "sample_R", "sampled_r", "sd_R", "sd_rt", "Std(R)", "t_end", "t_start",
    "target_date", "time", "time_varying_r", "top", "total", "type", "upper", 
    "value", "var", "vars", "viridis_palette", "window", ".", "%>%",
    "New confirmed cases by infection date", "Data", "R", "reference",
    ".SD", "day_of_week", "forecast_type", "measure" ,"numeric_estimate", 
    "point", "strat", "estimate", "breakpoint", "variable", "value.V1", "central_lower", "central_upper",
    "mean_sd", "sd_sd", "average_7",  "..lowers", "..upper_CrI", "..uppers", "timing",
    "stop_timeout"))

