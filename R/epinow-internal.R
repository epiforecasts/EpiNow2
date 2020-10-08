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
#' @inheritParams epinow
#' @return NULL
save_input <- function(reported_cases, target_folder) {
  if (!is.null(target_folder)) {
    latest_date <- reported_cases[confirm > 0][date == max(date)]$date
    
    saveRDS(latest_date, paste0(target_folder, "/latest_date.rds"))
    saveRDS(reported_cases, paste0(target_folder, "/reported_cases.rds"))
  }
  return(invisible(NULL))
}