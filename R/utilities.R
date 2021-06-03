#' Clean Nowcasts for a Supplied Date
#'
#' @description `r lifecycle::badge("stable")`
#' This function removes nowcasts in the format produced by `EpiNow2` from a target
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
  purrr::walk(
    dirs,
    function(dir) {
      remove_dir <- file.path(dir, date)
      if (dir.exists(remove_dir)) {
        futile.logger::flog.info("Removing files from: %s", remove_dir)
        lapply(
          list.files(file.path(remove_dir)),
          function(file) {
            file.remove(
              file.path(remove_dir, file)
            )
          }
        )
      }
    }
  )
}

#' Format Credible Intervals
#'
#' @description `r lifecycle::badge("stable")`
#' Combines a list of values into formatted credible intervals.
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
  CrI <- list(
    lower = value[[paste0("lower_", CrI)]],
    upper = value[[paste0("upper_", CrI)]]
  )
  conf <- paste0(
    value$median, " (",
    ifelse(!reverse, CrI$lower, CrI$upper), " -- ",
    ifelse(!reverse, CrI$upper, CrI$lower), ")"
  )
  return(conf)
}


#' Categorise the Probability of Change for Rt
#'
#' @description `r lifecycle::badge("stable")`
#' Categorises a numeric variable into "Increasing" (< 0.05),
#' "Likely increasing" (<0.4), "Stable" (< 0.6), "Likely decreasing" (< 0.95), "Decreasing" (<= 1)
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
    ifelse(var < 0.4, "Likely increasing",
      ifelse(var < 0.6, "Stable",
        ifelse(var < 0.95, "Likely decreasing",
          "Decreasing"
        )
      )
    )
  )
  var <- factor(var, levels = c(
    "Increasing", "Likely increasing", "Stable",
    "Likely decreasing", "Decreasing"
  ))
  return(var)
}

#' Convert Growth Rates to Reproduction numbers.
#'
#' @description `r lifecycle::badge("questioning")`
#' See [here](https://www.medrxiv.org/content/10.1101/2020.01.30.20019877v3.full.pdf)
#' for justification. Now handled internally by stan so may be removed in future updates if
#' no user demand.
#' @param r Numeric, rate of growth estimates
#' @param gamma_mean Numeric, mean of the gamma distribution
#' @param gamma_sd Numeric, standard deviation of the gamma distribution
#' @return Numeric vector of reproduction number estimates
#' @export
#' @examples
#' growth_to_R(0.2, 4, 1)
growth_to_R <- function(r, gamma_mean, gamma_sd) {
  k <- (gamma_sd / gamma_mean)^2
  R <- (1 + k * r * gamma_mean)^(1 / k)
  return(R)
}

#' Convert Reproduction Numbers to Growth Rates
#'
#' @description `r lifecycle::badge("questioning")`
#' See [here](https://www.medrxiv.org/content/10.1101/2020.01.30.20019877v3.full.pdf)
#' for justification. Now handled internally by stan so may be removed in future updates if
#' no user demand.
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
#' @description `r lifecycle::badge("stable")`
#' Allocate delays for stan. Used in `delay_opts`.
#' @param delay_var List of numeric delays
#' @param no_delays Numeric, number of delays
#' @return A numeric array
allocate_delays <- function(delay_var, no_delays) {
  if (no_delays > 0) {
    out <- unlist(delay_var)
  } else {
    out <- numeric(0)
  }
  return(array(out))
}

#' Allocate Empty Parameters to a List
#'
#' @description `r lifecycle::badge("stable")`
#' Allocate missing parameters to be empty two dimensional arrays. Used
#' internally by `simulate_infections.`
#' @param data A list of parameters
#' @param params A character vector of parameters to allocate to
#' empty if missing.
#' @param n Numeric, number of samples to assign an empty array
#' @return A list of parameters some allocated to be empty
#' @examples
#' data <- list(x = 1, y = 2, z = 30)
#' EpiNow2:::allocate_empty(data, params = c("x", "t"))
allocate_empty <- function(data, params, n = 0) {
  for (param in params) {
    if (!exists(param, data)) {
      data[[param]] <- array(0, dim = c(n, 0))
    }
  }
  return(data)
}
#' Match User Supplied Arguments with Supported Options
#'
#' @description `r lifecycle::badge("stable")`
#' Match user supplied arguments with supported options and return a logical list for
#' internal usage
#' @param input_args A character vector of input arguments (can be partial).
#' @param supported_args A character vector of supported output arguments.
#' @param logger A character vector indicating the logger to target messages at. Defaults
#' to no logging.
#' @param level Character string defaulting to "info". Logging level see documentation
#' of futile.logger for details. Supported options are "info" and "debug"
#' @return A logical vector of named output arguments
#' @importFrom  futile.logger flog.info flog.debug
#' @examples
#' # select nothing
#' EpiNow2:::match_output_arguments(supported_args = c("fit", "plots", "samples"))
#'
#' # select just plots
#' EpiNow2:::match_output_arguments("plots", supported_args = c("fit", "plots", "samples"))
#'
#' # select plots and samples
#' EpiNow2:::match_output_arguments(c("plots", "samples"),
#'   supported_args = c("fit", "plots", "samples")
#' )
#'
#' # lazily select arguments
#' EpiNow2:::match_output_arguments("p",
#'   supported_args = c("fit", "plots", "samples")
#' )
match_output_arguments <- function(input_args = c(),
                                   supported_args = c(),
                                   logger = NULL,
                                   level = "info") {
  if (level %in% "info") {
    flog_fn <- futile.logger::flog.info
  } else if (level %in% "debug") {
    flog_fn <- futile.logger::flog.debug
  }
  # make supported args a logical vector
  output_args <- rep(FALSE, length(supported_args))
  names(output_args) <- supported_args

  # get arguments supplied and linked to supported args
  found_args <- lapply(input_args, function(arg) {
    supported_args[grepl(arg, supported_args)]
  })
  found_args <- unlist(found_args)
  found_args <- unique(found_args)

  # tell the user about what has been passed in
  if (!is.null(logger)) {
    if (length(found_args) > 0) {
      flog_fn("Producing following optional outputs: %s",
        paste(found_args, collapse = ", "),
        name = logger
      )
    } else {
      flog_fn("No optional output specified",
        name = logger
      )
    }
  }
  # assign true false to supported arguments based on found arguments
  output_args[names(output_args) %in% found_args] <- TRUE
  return(output_args)
}


#' Expose internal package stan functions in R
#'
#' @description `r lifecycle::badge("stable")`
#' his function exposes internal stan functions in R from a user
#' supplied list of target files. Allows for testing of stan functions in R and potentially
#' user use in R code.
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
  functions <- paste0(
    "\n functions{ \n",
    paste(purrr::map_chr(
      files,
      ~ paste(readLines(file.path(target_dir, .)), collapse = "\n")
    ),
    collapse = "\n"
    ),
    "\n }"
  )
  rstan::expose_stan_functions(rstan::stanc(model_code = functions), ...)
  return(invisible(NULL))
}



#' Convert mean and sd to log mean for a log normal distribution
#'
#' @description `r lifecycle::badge("stable")`
#' Convert from mean and standard deviation to the log mean of the
#' lognormal distribution. Useful for defining distributions supported by
#' `estimate_infections`, `epinow`, and `regional_epinow`.
#' @param mean Numeric, mean of a distribution
#' @param sd Numeric, standard deviation of a distribution
#'
#' @return The log mean of a lognormal distribution
#' @export
#'
#' @examples
#'
#' convert_to_logmean(2, 1)
convert_to_logmean <- function(mean, sd) {
  log(mean^2 / sqrt(sd^2 + mean^2))
}

#' Convert mean and sd to log standard deviation for a log normal distribution
#'
#' @description `r lifecycle::badge("stable")`
#' Convert from mean and standard deviation to the log standard deviation of the
#' lognormal distribution. Useful for defining distributions supported by
#' `estimate_infections`, `epinow`, and `regional_epinow`.
#' @param mean Numeric, mean of a distribution
#' @param sd Numeric, standard deviation of a distribution
#'
#' @return The log standard deviation of a lognormal distribution
#' @export
#'
#' @examples
#'
#' convert_to_logsd(2, 1)
convert_to_logsd <- function(mean, sd) {
  sqrt(log(1 + (sd^2 / mean^2)))
}


#' Update a List
#'
#' @description `r lifecycle::badge("stable")`
#' Used to handle updating settings in a list. For example when making
#' changes to `opts_list` output.
#' @param defaults A list of default settings
#' @param optional A list of optional settings to override defaults
#' @return A list
#' @export
update_list <- function(defaults = list(), optional = list()) {
  if (length(optional) != 0) {
    defaults <- defaults[setdiff(names(defaults), names(optional))]
    updated <- c(defaults, optional)
  } else {
    updated <- defaults
  }
  return(updated)
}

#' Adds a day of the week vector
#'
#' @param dates Vector of dates
#' @param week_effect Numeric from 1 to 7 defaults to 7
#'
#' @return A numeric vector containing the period day of the week index
#' @export
#' @importFrom lubridate wday
#' @examples
#' dates <- seq(as.Date("2020-03-15"), by = "days", length.out = 15)
#' # Add date based day of week
#' add_day_of_week(dates, 7)
#'
#' # Add shorter week
#' add_day_of_week(dates, 4)
add_day_of_week <- function(dates, week_effect = 7) {
  if (week_effect == 7) {
    day_of_week <- lubridate::wday(dates, week_start = 1)
  } else {
    day_of_week <- as.numeric(dates - min(dates)) %% week_effect
    day_of_week <- ifelse(day_of_week == 0, week_effect, day_of_week)
  }
  return(day_of_week)
}

#' Set to Single Threading
#' 
#' This function sets the threads used by data.table to 1 in the parent function
#' and then restores the initial data.table threads when the function exits.
#' This is primarily used as an internal function inside of other functions
#' and will generally not be used on its own.
#' 
#' @importFrom data.table getDTthreads setDTthreads
#' @keywords internal
#' @return an environment in the parent frame named "dt_settings"
#' @examples 
#' \donttest{
#' data.table::setDTthreads(2)
#' test_function <- function(){
#'   set_dt_single_thread()
#'   
#'   print(data.table::getDTthreads())
#' 
#' }
#' test_function()
#' data.table::getDTthreads()
#' }
#' @export

set_dt_single_thread <- function() {
  
  a <- list2env(x = list(dt_previous_threads = data.table::getDTthreads()))
  
  assign(deparse(substitute(dt_settings)), a, envir = parent.frame())
  
  data.table::setDTthreads(1)
  
  do.call("on.exit", 
          list(quote(data.table::setDTthreads(dt_settings$dt_previous_threads))), 
          envir = parent.frame()) 
}

#' @importFrom stats glm median na.omit pexp pgamma plnorm quasipoisson rexp rgamma rlnorm rnorm rpois runif sd var
globalVariables(
  c(
    "bottom", "cases", "confidence", "confirm", "country_code", "crps",
    "cum_cases", "Date", "date_confirm", "date_confirmation", "date_onset",
    "date_onset_sample", "date_onset_symptoms", "date_onset.x", "date_onset.y",
    "date_report", "day", "doubling_time", "effect", "Effective reproduction no.",
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
    ".SD", "day_of_week", "forecast_type", "measure", "numeric_estimate",
    "point", "strat", "estimate", "breakpoint", "variable", "value.V1", "central_lower", "central_upper",
    "mean_sd", "sd_sd", "average_7", "..lowers", "..upper_CrI", "..uppers", "timing",
    "dataset", "last_confirm", "report_date", "secondary", "id"
  )
)
