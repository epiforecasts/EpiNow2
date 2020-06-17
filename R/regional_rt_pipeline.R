#' Regional Realtime Pipeline
#'
#' @description Runs a pipeline by region.
#' @param cases A dataframe of cases (`confirm`) by date of confirmation (`date`), import status (`import_status`; ("imp)), and region (`region`).
#' @param linelist A dataframe of of cases (by row) containing the following variables:
#' `import_status` (values "local" and "imported"), `date_onset`, `date_confirm`, `report_delay`, and `region`. If a national linelist is not available a proxy linelist may be 
#' used but in this case `merge_onsets` should be set to `FALSE`.
#' @param merge_onsets Logical defaults to `FALSE`. Should available onset data be used. Typically if `regional_delay` is
#' @param case_limit Numeric, the minimum number of cases in a region required for that region to be evaluated. Defaults to 10.
#' set to `FALSE` this should also be `FALSE`
#' @param verbose Logical, defaults to `FALSE`. Should progress messages be shown for each reigon?
#' @param ... Pass additional arguments to `rt_pipeline`
#' @inheritParams rt_pipeline
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
#'                EpiNow::get_dist_def(rexp(25, 1/10), 
#'                                     samples = 5, bootstraps = 1))
#' 
#' ## Uses example case vector from EpiSoon
#' cases <- data.table::setDT(EpiSoon::example_obs_cases)
#' cases <- cases[, `:=`(confirm = as.integer(cases), import_status = "local")][,
#'                   cases := NULL]
#' 
#' cases <- data.table::rbindlist(list(
#'   data.table::copy(cases)[, region := "testland"],
#'   cases[, region := "realland"]))
#'   
#' ## Run basic nowcasting pipeline
#' regional_rt_pipeline(cases = cases,
#'             delay_defs = delay_dist,
#'             target_folder = target_dir)
#'}
regional_rt_pipeline <- function(cases = NULL, linelist = NULL, 
                                 delay_defs = NULL, incubation_defs = NULL,
                                 target_folder = "results", 
                                 target_date = NULL,
                                 merge_onsets = FALSE,
                                 case_limit = 40,
                                 onset_modifier = NULL,
                                 dt_threads = 1,
                                 verbose = FALSE,
                                 ...) {
  
  ## Set input to data.table
  cases <- data.table::as.data.table(cases)
  if (!is.null(linelist)) {
    linelist <- data.table::as.data.table(linelist)
  }
  
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
  
  rm(eval_regions)
  ## Make sure all dates have cases numbers
  cases_grid <- cases[,.(date = seq(min(date), max(date), by = "days"), 
                         import_status = list(list("local", "imported"))),
                      by = "region"][,
                       .(import_status = unlist(import_status)), 
                       by = c("date", "region")]
  
  cases <- cases[cases_grid, on = c("date", "region", "import_status")][is.na(confirm), confirm := 0]
  cases <- data.table::setorder(cases, region, import_status, date)
 
  rm(cases_grid)
  
  ## regional pipelines
  regions <- unique(cases$region)

  message("Running pipelines by region")
  ## Function to run the pipeline in a region
  run_region <- function(target_region, 
                         cases,
                         linelist,
                         onset_modifier,
                         ...) { 
    message("Running Rt pipeline for ", target_region)
    data.table::setDTthreads(threads = dt_threads)
    
    regional_cases <- cases[region %in% target_region][, region := NULL]
    
    rm(cases)
    
    if (!is.null(linelist) & merge_onsets) {
      regional_linelist <- linelist[region %in% target_region][, 
                                    region := NULL]
    }else{
      regional_linelist <- linelist
    }
    
    rm(linelist)
    
    if (!is.null(onset_modifier)) {
      region_onset_modifier <- onset_modifier[region %in% target_region]
      region_onset_modifier <- region_onset_modifier[,region := NULL]
      
    }else{
      region_onset_modifier <- NULL
    }
    
    rm(onset_modifier)
    
    EpiNow::rt_pipeline(
      cases = regional_cases,
      linelist = regional_linelist,
      onset_modifier = region_onset_modifier,
      target_folder = file.path(target_folder, target_region),
      target_date = target_date, 
      merge_actual_onsets = merge_onsets, 
      delay_defs = delay_defs,
      incubation_defs = incubation_defs,
      verbose = verbose,
      ...)
    
    return(invisible(NULL))}
  

  safe_run_region <- purrr::safely(run_region)
  
  ## Run regions (make parallel using future::plan)
  future.apply::future_lapply(regions, safe_run_region,
                              cases = cases,
                              linelist = linelist,
                              onset_modifier = onset_modifier,
                              ...,
                              future.scheduling = Inf)

    
  return(invisible(NULL))
}