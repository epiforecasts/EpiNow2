

setup_dt <- function(reported_cases) {
  suppressMessages(data.table::setDTthreads(threads = 1))
  reported_cases <- data.table::setDT(reported_cases)
}

#' Setup Target Folder for Saving
#'
#' @param target_date Date, defaults to maximum found in the data if not specified.
#' @param target_folder Character string specifying where to save results (will create if not present).
#'
#' @return A modified target date
setup_target_folder <- function(target_folder = NULL, target_date) {
  
  if (!is.null(target_folder)) {
    latest_folder <- file.path(target_folder, "latest")
    target_folder <- file.path(target_folder, target_date)
    
    if (!dir.exists(target_folder)) {
      dir.create(target_folder, recursive = TRUE)
    }
  }
  return(target_folder)
}


#' Save Observed Data
#'
#' @param reported_cases 
#' @inheritParams setup_target_folder
#' @inheritParams estimate_infections
#' @return NULL
save_input <- function(reported_cases, target_folder) {
  if (!is.null(target_folder)) {
    latest_date <- reported_cases[confirm > 0][date == max(date)]$date
    
    saveRDS(latest_date, paste0(target_folder, "/latest_date.rds"))
    saveRDS(reported_cases, paste0(target_folder, "/reported_cases.rds"))
  }
  return(invisible(NULL))
}



save_estimate_infections <- function(estimates, target_folder = NULL, 
                                     samples = TRUE, fit = TRUE) {
  if (!is.null(target_folder)) {
    if (samples) {
      saveRDS(estimates$samples, paste0(target_folder, "/estimate_samples.rds"))
    }
    saveRDS(estimates$summarised, paste0(target_folder, "/summarised_estimates.rds"))
    
    if (fit) {
      saveRDS(estimates$fit, paste0(target_folder, "/model_fit.rds"))
    }
  }
  return(invisible(NULL))
}


save_forecast_infections <- function(forecast, target_folder = NULL, samples = TRUE) {
  if (!is.null(target_folder)) {
    if (samples) {
      saveRDS(forecast$samples, paste0(target_folder, "/forecast_samples.rds"))
    }
    saveRDS(forecast$summarised, paste0(target_folder, "/summarised_forecast.rds"))
  }
  return(invisible(NULL))
}