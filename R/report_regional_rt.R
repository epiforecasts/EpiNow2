#' Regional Rt
#'
#' @description Estimates Rt by region
#' @param cases A dataframe of cases (`confirm`) by date of confirmation (`date`), import status (`import_status`; ("imp)), and region (`region`).
#' @param merge_onsets Logical defaults to `FALSE`. Should available onset data be used. Typically if `regional_delay` is
#' @param case_limit Numeric, the minimum number of cases in a region required for that region to be evaluated. Defaults to 10.
#' set to `FALSE` this should also be `FALSE`
#' @param verbose Logical, defaults to `FALSE`. Should progress messages be shown for each reigon?
#' @param ... Pass additional arguments to `rt_pipeline`
#' @inheritParams report_rt
#' @return NULL
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom data.table as.data.table setDT copy setorder
#' @importFrom purrr safely
#' @examples
#'  \dontrun{
#' ## Save everything to a temporary directory 
#' ## Change this to inspect locally
#' target_dir <- tempdir() 
#' 
#' ## Construct example distributions
#' ## reporting delay dist
#' delay_dist <- suppressWarnings(
#'                EpiNow2::get_dist_def(rexp(25, 1/10), 
#'                                     samples = 5, bootstraps = 1))
#' 
#' ## Uses example case vector from EpiSoon
#' cases <- data.table::setDT(EpiSoon::example_obs_cases)
#' cases <- cases[, `:=`(confirm = as.integer(cases))][,
#'                   cases := NULL]
#' 
#' cases <- data.table::rbindlist(list(
#'   data.table::copy(cases)[, region := "testland"],
#'   cases[, region := "realland"]))
#'   
#' ## Run basic nowcasting pipeline
#' report_regional_rt(cases = cases,
#'             target_folder = target_dir)
#'}
report_regional_rt <- function(cases = NULL, 
                        target_folder = "results", 
                        target_date = NULL,
                        case_limit = 20,
                        cores = 1,
                        verbose = FALSE,
                        ...) {
  
  ## Set input to data.table
  cases <- data.table::as.data.table(cases)
  
  if (!is.null(onset_modifier)) {
    onset_modifier <- data.table::as.data.table(onset_modifier)
  }
  
  if (is.null(target_date)) {
    target_date <- as.character(max(cases$date))
  }

  message("Running pipeline for ", target_date)
   
  
  ## Check for regions more than required cases
  eval_regions <- data.table::copy(cases)[,.(confirm = sum(confirm, na.rm = TRUE)), 
                                          by = c("region", "date")][
                      confirm >= case_limit]$region
  
  eval_regions <- unique(eval_regions)
  
  ## Exclude zero regions
  cases <- cases[!is.na(region)][region %in% eval_regions]
  
  message("Running the pipeline for: ",
          paste(eval_regions, collapse = ", "))

  ## regional pipelines
  regions <- unique(cases$region)

  message("Running pipelines by region")
  ## Function to run the pipeline in a region
  run_region <- function(target_region, 
                         cases,
                         ...) { 
    message("Running Rt pipeline for ", target_region)
    data.table::setDTthreads(threads = cores)
    
    regional_cases <- cases[region %in% target_region][, region := NULL]
    
    EpiNow2::report_rt(
      cases = regional_cases,
      target_folder = file.path(target_folder, target_region),
      target_date = target_date, 
      verbose = verbose,
      ...)
    
    return(invisible(NULL))}
  

  safe_run_region <- purrr::safely(run_region)
  
  ## Run regions (make parallel using future::plan)
  future.apply::future_lapply(regions, safe_run_region,
                              cases = cases,
                              ...,
                              future.scheduling = Inf)

    
  return(invisible(NULL))
}