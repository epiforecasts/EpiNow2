#' Clean Nowcasts for a Supplied Date
#'
#' @description This function removes nowcasts in the format produced by `EpiNow` from a target
#' directory for the date supplied.
#' @param date Date object. Defaults to todays date
#' @param nowcast_dir Character string giving the filepath to the nowcast results directory.
#'
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
                  message("Removing files from: ", remove_dir)
                  lapply(list.files(file.path(remove_dir)),
                         function(file){
                           file.remove(
                             file.path(remove_dir, file)
                           )
                         })
                  
                }
                
              })
  
  
}


#' Add Dates from Daata
#'
#' @description Pulls the last n dates from a vector
#' @param dates Character vector of dates to pull from.
#' @param n Number of dates required
#' @return Character vector of dates of length N
#' @export
#'
#' @examples
#'
#' dates <- rep(1:10)
#'
#' add_dates(dates, 3)
add_dates <- function(dates, n) {
  dates <-
    dates[seq(length(dates) + 1 - n,
              length(dates))]
  
  return(dates)
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
#' @importFrom purrr map_chr
#' @examples
#' 
#' value <- list(list(point = 1, lower = 0, upper = 3))
#' 
#' make_conf(value, round_type = round, digits = 0)
make_conf <- function(value, round_type = NULL, digits = 0) {
  
  if (is.null(round_type)) {
    round_type <- round
  }
  purrr::map_chr(value, ~ paste0(round_type(.$point, digits),
                                 " (", 
                                 round_type(.$lower, digits),
                                 " -- ", 
                                 round_type(.$upper, digits),
                                 ")"))
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

#' Extract a the Maximum Value of a Variable Based on a Filter
#'
#' @param df Datatable with the following variables: `type` and `si_dist`
#' @param max_var Character string containing the variable name to pull out the maximum R estimate for.
#' @param sel_var Character string indicating the variable to extract
#' @param type_selected The nowcast type to extract.
#' @importFrom data.table as.data.table
#' @return A character string containing the maximum variable
#' @export
#'
#' @examples
#'
#' df <- data.table::data.table(type = c("nowcast", "other"),
#'                              var = c(1:10),
#'                              sel = "test")
#'                              
#' pull_max_var(df, max_var = "var", sel_var = "var", type_selected = "nowcast")
pull_max_var <- function(df, max_var = NULL,
                         sel_var = NULL, type_selected = NULL) {
  
  
  df <- data.table::as.data.table(df)[type %in% type_selected]
  df <- df[df[[max_var]] == max(df[[max_var]])][[sel_var]]
  
  return(df)
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
    "New confirmed cases by infection date", "Data", "R", "reference"))
