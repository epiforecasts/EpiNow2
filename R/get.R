#' Get Folders with Nowcast Results
#'
#' @param results_dir A character string giving the directory in which results
#'  are stored (as produced by `regional_rt_pipeline`).
#'
#' @return A named character vector containing the results to plot.
#' @export
get_regions <- function(results_dir) {
  
  # Regions to include - based on folder names
  regions <- list.files(results_dir)
  
  ## Put into alphabetical order
  regions <- regions[order(regions)]
  
  names(regions) <- regions
  
  
  return(regions)
}



#' Load nowcast results
#'
#' @param file Character string giving the result files name.
#' @param region Character string giving the region of interest.
#' @param date Target date (in the format `"yyyy-mm-dd`).
#' @param result_dir Character string giving the location of the target directory 
#'
#' @return An R object read in from the targeted .rds file
#' @export
load_nowcast_result <- function(file = NULL, region = NULL, 
                                date = target_date, result_dir = results_dir) {
  file_path <- file.path(result_dir, region, date, file)
  object <- readRDS(file_path)
  
  return(object)
}

#' Get Timeseries from EpiNow2
#'
#' @param results_dir A character string indicating the folder containing the `EpiNow2`
#' results to extract.
#' @param date A Character string (in the format "yyyy-mm-dd") indicating the date to extract
#' data for. Defaults to "latest" which finds the latest results available.
#' @param summarised Logical, defaults to `FALSE`. Should full or summarised results be 
#' returned. 
#' @return A list of reproduction number estimates and nowcast cases
#' @export
#' @importFrom purrr map_dfr safely
#' @examples
#'
#'
#' \dontrun{
#' ## Assuming epiforecasts/covid is one repo higher
#' ## Summary results
#' get_timeseries("../covid/_posts/global/nowcast/results/", 
#'                summarised = TRUE)
#' 
#' ## Simulations
#' get_timeseries("../covid/_posts/global/nowcast/results/")
#' }
#' ## Code
#' get_timeseries
get_timeseries <- function(results_dir = NULL, date = NULL,
                           summarised = FALSE) {
  
  ## Assign to latest likely date if not given
  if (is.null(date)) {
    date <- "latest"
  }
  
  if (summarised) {
    nowcast <- "summarised_nowcast.rds"
    rt_index <- 1
  } else{
    nowcast <- "nowcast.rds"
    rt_index <- 3
  }
  
  ## Find all regions
  regions <- list.files(results_dir)
  names(regions) <- regions
  
  load_data <- purrr::safely(EpiNow2::load_nowcast_result)
  
  ## Get rt values and combine
  rt <- purrr::map_dfr(regions, ~ load_data("time_varying_params.rds", .,
                                            result_dir = results_dir,
                                            date = date)[[1]][[rt_index]],
                       .id = "region")
  
  
  ## Get incidence values and combine
  incidence <- purrr::map_dfr(regions, ~ load_data(nowcast, .,
                                                   result_dir = results_dir,
                                                   date = date)[[1]],
                              .id = "region")
  
  out <- list(rt, incidence)
  names(out) <- c("rt", "incidence")
  
  return(out)
}
