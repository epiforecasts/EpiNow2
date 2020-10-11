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
#' @param round_type Function, type of rounding to apply. Defaults to `round`.
#' @param digits Numeric, defaults to 0. Amount of rounding to apply
#' @return A character vector formatted for reporting
#' @export
#' @examples
#' value <- list(point = 1, lower = 0, upper = 3)
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
      flog_fn("Producing following optional outputs: %s", paste(found_args, collapse = ", "),
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

