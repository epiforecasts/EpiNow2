#' Clean Nowcasts for a Supplied Date
#'
#' @description This function removes nowcasts in the format produced by `EpiNow2` from a target
#' directory for the date supplied.
#' @param date Date object. Defaults to today's date
#' @param nowcast_dir Character string giving the filepath to the nowcast results directory.
#' @importFrom purrr walk
#' @importFrom futile.logger flog.info
#' @return NULL
#' @export
clean_nowcasts <- function(date = NULL, nowcast_dir = NULL) {
  
  if (is.null(date)) {
    date <- Sys.Date()
  }
  
  if (is.null(nowcast_dir)) {
    nowcast_dir <- "."
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
#' @param round_type Function, type of rounding to apply. Defaults to `round`.
#' @param digits Numeric, defaults to 0. Amount of rounding to apply
#'
#' @return A character vector formatted for reporting
#' @export
#'
#' @examples
#' 
#' value <- list(point = 1, lower = 0, upper = 3)
#' 
#' make_conf(value, round_type = round, digits = 0)
make_conf <- function(value, round_type = NULL, digits = 0) {
  
  if (is.null(round_type)) {
    round_type <- round
  }
  paste0(round_type(value$point, digits), " (", 
         round_type(value$lower, digits), " -- ", 
         round_type(value$upper, digits), ")")
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
#' 
#' var <- seq(0.01, 1, 0.01)
#' 
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
#'
#' @return Numeric vector of reproduction number estimates
#' @export
#'
#' @examples
#' 
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
#'
#' @examples
#' 
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
#'
#' @return A numeric array
allocate_delays <- function(delay_var, no_delays) {
  if (no_delays > 0) {
    out <- unlist(delay_var)
  }else{
    out <- 1
  }
  return(array(out))
}


#' Timeout Error
#' @param fit A stan fit object
#' @return Nothing
#' @importFrom futile.logger flog.error
stop_timeout <- function(fit) {
  if (is.null(fit)) {
    futile.logger::flog.error("fitting timed out - try increasing max_execution_time",
                              name = "Epinow2.epinow.estimate_infections.fit")
    stop("model fitting timed out - try increasing max_execution_time")
  }
  return(invisible(NULL))
}



#' Setup Logging
#'
#' @description Sets up `futile.logger` logging, which is integrated into `EpiNow2`. See the 
#' documentation for `futile.logger` for full details. By default `EpiNow2` prints all logs at 
#' the "INFO" level and returns them to the console.
#' @param threshold Character string indicating the logging level see (?futile.logger 
#' for details of the available options). Defaults to "INFO".
#' @param file Character string indicating the path to save logs to. By default logs will be
#' written to the console.
#' @param mirror_to_console Logical, defaults to `FALSE`. If saving logs to a file should they 
#' also be duplicated in the console.
#' @param name Character string defaulting to EpiNow2. This indicates the name of the logger to setup.
#' The default logger for EpiNow2 is called EpiNow2. Nested options include: Epinow2.epinow which controls 
#' all logging for `epinow` and nested functions, EpiNow2.epinow.estimate_infections (logging in
#'  `estimate_infections`), and EpiNow2.epinow.estimate_infections.fit (logging in fitting functions).
#' @importFrom futile.logger flog.threshold flog.appender appender.tee appender.file
#' @return Nothing
#' @export
#'
#' @examples
#' 
#' # Set up info only logs with errors only 
#' # for logging related to epinow (or nested) calls
#' # (info logs are enabled by default.)
#' setup_logging("Info", name = "EpiNow2")
#' setup_logging("ERROR", name = "EpiNow2.epinow")
setup_logging <- function(threshold = "INFO", file = NULL,
                          mirror_to_console = FALSE, name = "EpiNow2") {
  if (is.null(name)) {
    name <- "ROOT"
  }
  message("Setting up logging for the ", name, " logger")
  message("Logging threshold set at: ", threshold)
  futile.logger::flog.threshold(threshold, name = name)
  
  if (!is.null(file)) {
    message("Writing logs to: ", file)
    if (mirror_to_console) {
      futile.logger::flog.appender(futile.logger::appender.tee(file), name = name)
    }else{
      futile.logger::flog.appender(futile.logger::appender.file(file), name = name)  
    }
  }else{
    message("Writing logs to the console")
    futile.logger::flog.appender(futile.logger::appender.console(), name = name)
  }
  return(invisible(NULL))
}

#' Set up Future Backend
#' @description A utility function that aims to streamline the set up 
#' of the required future backend with sensible defaults for most users of `regional_epinow`.
#' More advanced users are recommended to setup their own `future` backend based on their
#' available resources. 
#' @param strategies A vector length 1 to 2 of strategies to pass to `future::plan`. Nesting 
#' of parallisation is from the top level down. The default is to set up nesting parallisation
#' with both using `future::multiprocess`. For single level parallisation use a single strategy
#' or `future::plan` directly. See `?future::plan` for options.
#' @param min_cores_per_worker Numeric, the minimum number of cores per worker. 
#' Defaults to 4 which assumes 4 MCMC chains are in use per region.
#' @inheritParams regional_epinow
#' @importFrom futile.logger flog.error flog.info flog.debug
#' @importFrom future availableCores plan tweak 
#' @export
#' @return Numeric number of cores to use per worker. If greater than 1 pass to
#' `stan_args = list(cores = "output from setup future")` or use `future = TRUE`. If only a single strategy is 
#' used then nothing is returned.
setup_future <- function(reported_cases, strategies = c("multiprocess", "multiprocess"),
                         min_cores_per_worker = 4) {
  
  if (length(strategies) > 2 | length(strategies) == 0) {
    futile.logger::flog.error("1 or 2 strategies should be used")
    stop("1 or 2 strategies should be used")
  }
  
  if (is.null(reported_cases$region)) {
    futile.logger::flog.error("Reported cases must contain a region")
    stop("Exactly 2 strategies should be used")
  }
  
  if (length(strategies) == 1) {
    workers <- future::availableCores()
    futile.logger::flog.info("Using %s workers with 1 core per worker",
                             workers)
    future::plan(strategies, workers = workers,
                 gc = TRUE, earlySignal = TRUE)
    cores_per_worker <- 1
    return(invisible(NULL))
  }else{
    jobs <- length(unique(reported_cases$region))
    workers <- min(ceiling(future::availableCores() / min_cores_per_worker), jobs)
    cores_per_worker <- max(1, round(future::availableCores() / workers, 0))
    
    futile.logger::flog.info("Using %s workers with %s cores per worker",
                             workers, cores_per_worker)
    
    future::plan(list(future::tweak(strategies[1], workers = workers, 
                                    gc = TRUE, earlySignal = TRUE), 
                      future::tweak(strategies[2], workers = cores_per_worker)))
    return(cores_per_worker)
  }
}

# Safe Directory Create
# Used to create a directory for the functions

safe_dir_create <- function(dir_name){
  dir_of_interest <- file.path(dir_name)
  if(dir.exists(dir_of_interest)){
    invisible()
  } else {
    dir.create(dir_of_interest)
  }
}

# Safe Copy Stan Model
# This copies over all of the associated functions and stan files
# when the user users the internal model. To be used as internal function only.
# Users shouldn't need to access this directly.

copy_models <- function(dir_path){
  # First copy functions
  app_file_loc <- system.file(file.path("stan", "functions"), package = "EpiNow2")
  R.utils::copyDirectory(app_file_loc, file.path(dir_path, "functions"))
  # Copy Models
  stan_file_loc <- system.file(file.path("stan", "estimate_infections.stan"), 
                               package = "EpiNow2")
  R.utils::copyFile(stan_file_loc, file.path(dir_path,
                                             "estimate_infections.stan"))
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
    "mean_sd", "sd_sd"))

