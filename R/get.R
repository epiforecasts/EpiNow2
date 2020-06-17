
#' Combine total and imported case counts
#'
#' @param total_cases Dataframe with following variables: `date` and `cases`.
#' @param linelist Dataframe with at least the following variables: `date_confirm`, `import_status`
#' @param cases_from A character string containing a date in the format `"yyyy-mm-dd"`. Applies a 
#' filter to returned cases.
#' @importFrom lubridate ymd
#' @return A tibble containing cases by date locally and imported
#' @export
get_local_import_case_counts <- function(total_cases, linelist = NULL, cases_from = NULL) {
  
  .Deprecated()
  
  total_cases <- total_cases %>%
    dplyr::rename(total = cases) %>%
    dplyr::mutate(date = as.Date(date))
  
  imported_cases <- linelist %>%
    tidyr::drop_na(date_confirm) %>%
    dplyr::count(date_confirm, import_status) %>%
    dplyr::filter(import_status %in% "imported") %>%
    dplyr::select(date = date_confirm, imported = n)
  
  cases <- total_cases %>%
    dplyr::full_join(imported_cases, by = "date") %>%
    tidyr::complete(date = seq(min(.$date), max(.$date), by = "day"),
                    fill = list(imported = 0, total = 0)) %>%
    dplyr::mutate(local = ifelse(total >= imported, total - imported, 0),
                  overflow = ifelse(total < imported, imported - total, 0))
  
  
  ## Deal with imported cases but no overall cases by reducing later number of reported cases
  for(index in 1:nrow(cases)) {
    
    overflow <- cases$overflow[index]
    
    j <- index
    
    while(overflow > 0 | j > nrow(cases)) {
      j <- j + 1
      
      cases$local[j] <- cases$total[j] - overflow
      if (cases$local[j] < 0) {
        overflow <- -cases$local[j]
        cases$local[j] <- 0
      }else{
        overflow <- 0
      }
      
    }
  }
  
  cases <- cases  %>%
    dplyr::select(date, local, imported) %>%
    tidyr::gather(key = "import_status", value = "cases", local, imported)
  
  if (!is.null(cases_from)) {
    cases <- cases %>% 
      dplyr::filter(date >= lubridate::ymd(cases_from))
  }
  
  return(cases)
}



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

#' Get Timeseries from EpiNow
#'
#' @param results_dir A character string indicating the folder containing the `EpiNow`
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
  
  load_data <- purrr::safely(EpiNow::load_nowcast_result)
  
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
