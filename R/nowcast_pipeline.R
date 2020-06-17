#' Impute Cases Date of Infection
#'
#' @param reported_cases A dataframe of reported cases
#' @param linelist A linelist of report dates and onset dates
#' @param merge_actual_onsets Logical, defaults to `TRUE`.
#'  Should linelist onset dates be used where available?
#' @param verbose Logical, defaults to `FALSE`. Should internal nowcasting progress messages be returned.
#' @param nowcast_lag Numeric, defaults to 4. The number of days by which to lag nowcasts. Helps reduce bias due to case upscaling.
#' @param delay_defs A data.table that defines the delay distributions (model, parameters and maximum delay for each model). 
#' See `get_delay_dist` for an example of the structure.
#' @param incubation_defs A data.table that defines the incubation distributions (model, parameters and maximum delay for each model). 
#' See `get_delay_dist` for an example of the structure.
#' @param onset_modifier data.frame containing a `date` variable and a function `modifier` variable. This is used 
#' to modify estimated cases by onset date. `modifier` must be a function that returns a proportion when called 
#' (enables inclusion of uncertainty) and takes the following arguments: `n` (samples to return) and `status` ("local" or "import").
#' @param approx_delay  Logical, defaults to `FALSE`. Should delay sampling be approximated using case counts. Not appropriate
#' when case numbers are low. Useful for high cases counts as decouples run time and resource usage from case count.
#' @param max_delay Numeric, maximum delay to allow. Defaults to 120 days
#' @param max_upscale Numeric, maximum upscaling of cases allowed at each time point. Defaults to 100 times the observed 
#' cases.
#' @inheritParams generate_pseudo_linelist
#' @inheritParams sample_delay
#' @inheritParams plot_pipeline
#' @return A dataframe of nowcast cases from each step of the nowcasting process (defined by `type`).
#' @export
#' @importFrom lubridate days
#' @importFrom purrr map safely map_dfr map_lgl compact map2_dbl transpose
#' @importFrom future.apply future_lapply
#' @importFrom data.table .N as.data.table := rbindlist setDT setDTthreads
#' @examples
#' ## Construct example distributions
#' ## reporting delay dist
#' delay_dist <- suppressWarnings(
#'                EpiNow::get_dist_def(rexp(25, 1 / 10), 
#'                                     samples = 1, bootstraps = 1))
#' ## incubation delay dist
#' incubation_dist <- delay_dist
#' 
#' ## Uses example case vector from EpiSoon
#' cases <- data.table::setDT(EpiSoon::example_obs_cases)
#' cases <- cases[, `:=`(confirm = as.integer(cases), import_status = "local")]
#' 
#' ## Basic nowcast
#' nowcast <- nowcast_pipeline(reported_cases = cases, 
#'                             target_date = max(cases$date),
#'                             delay_defs = delay_dist,
#'                             incubation_defs = incubation_dist)
#'                             
#' nowcast
#' 
nowcast_pipeline <- function(reported_cases = NULL, linelist = NULL,
                             target_date = NULL,
                             earliest_allowed_onset = NULL,
                             merge_actual_onsets = FALSE,
                             approx_delay = FALSE,
                             max_delay = 120,
                             verbose = FALSE,
                             delay_defs = NULL,
                             incubation_defs = NULL,
                             nowcast_lag = 8, 
                             max_upscale = 5,
                             onset_modifier = NULL) {

  

# Balance input delay and incubation samples ------------------------------

  balance_dfs <- function(df1, df2) {
    if (nrow(df1) > nrow(df2)) {
      df2 <- data.table::rbindlist(list(
        df2,
        df2[sample(1:nrow(df2), (nrow(df1) - nrow(df2)), replace = TRUE), ]
      ))
    }
    return(df2)
  }
  
  incubation_defs <- balance_dfs(delay_defs, incubation_defs)
  delay_defs <- balance_dfs(incubation_defs, delay_defs)
  
# Organise inputted linelist ----------------------------------------------

  if (!approx_delay) {
    ## Split linelist into day chunks
    ## Used to merge actuals with estimated onsets
    if (merge_actual_onsets & !is.null(linelist)) {
      linelist <- data.table::as.data.table(linelist)
      ## Group linelists by day
      linelist_by_day <- EpiNow::split_linelist_by_day(
        data.table::copy(linelist)[import_status == "local"]
        )
      
      ## Filter out imported cases and repeat linelist step
      imported_linelist <- data.table::copy(linelist)[import_status == "imported"]

      
      if (nrow(imported_linelist) > 0) {
        imported_linelist_by_day <- EpiNow::split_linelist_by_day(imported_linelist)
      }
    }else{
      linelist_by_day <- NULL
      imported_linelist_by_day <- NULL
    }
  }
  




# Organise input case counts ----------------------------------------------

  reported_cases <- data.table::as.data.table(reported_cases)
  
  ## Filter reported cases based on the nowcasting date
  reported_cases <- reported_cases[date <= target_date]

  ## Split cases into local and imported
  local_cases <- data.table::copy(reported_cases)[import_status == "local"]

  imported_cases <- reported_cases[import_status == "imported"]

 
 
# Generate a pseudo linelist ----------------------------------------------

  if (!approx_delay) {
    if (verbose) {
      message("Generating a pseudo linelists")
    }
    
    populate_list <- function(case_df = NULL, linelist_df = NULL) {
        generate_pseudo_linelist(count_linelist = linelist_from_case_counts(case_df),
                                 observed_linelist = linelist_df, 
                                 merge_actual_onsets =  merge_actual_onsets)
    }
    
    populated_linelist <- populate_list(local_cases, linelist_by_day)
    
    if (sum(imported_cases$confirm) > 0) {
      imported_populated_linelist <- populate_list(imported_cases, imported_linelist_by_day)
    }
  }

# Argument conversion -----------------------------------------------------

if (!is.null(onset_modifier)) {
  onset_modifier <- data.table::as.data.table(onset_modifier)
}
 
# Nowcasting for each samples or vector of samples ------------------------

  nowcast_inner <- function(dist_def = NULL, max_upscale, verbose = NULL) {
    
    suppressMessages(data.table::setDTthreads(threads = 1))
    
    delay_def <- dist_def$delay
    incubation_def <- dist_def$incubation
    
    ## Define sample delay fn
    sample_delay_fn <- function(n, ...) {
      EpiNow::dist_skel(n = n, 
                        model = delay_def$model[[1]], 
                        params = delay_def$params[[1]],
                        max_value = delay_def$max_value[[1]], 
                        ...)
    }
    
    ## Define an incubation fn
    sample_incubation_fn <- function(n, ...) {
      EpiNow::dist_skel(n = n, 
                        model = incubation_def$model[[1]], 
                        params = incubation_def$params[[1]],
                        max_value = incubation_def$max_value[[1]], 
                        ...)
    }
  
    if (!approx_delay) {
      ## Sample onset dates using reporting delays
      if (verbose) {
        message("Sampling from reporting delay linelist")
      }
      
      sampled_linelist <- sample_delay(linelist = populated_linelist,
                                       delay_fn = sample_delay_fn,
                                       earliest_allowed_onset = earliest_allowed_onset)
      
      if (sum(imported_cases$confirm) > 0) {
        
        imported_sampled_linelist <- sample_delay(linelist = imported_populated_linelist,
                                                  delay_fn = sample_delay_fn,
                                                  earliest_allowed_onset = earliest_allowed_onset)
      }
      
      
      ## Function to summarise cases
      summarise_cases <- function(df) {
        df_cnt <- df[, .(cases = .N), by = date_onset]
        
        df_cnt <- df_cnt[df_cnt[,.(date_onset= seq(min(date_onset), max(date_onset), by = "days"))], on=.(date_onset)]
        df_cnt <- df_cnt[is.na(cases), cases := 0 ][,.(date = date_onset, cases)]
        return(df_cnt)
      }
      
      ## Summarise local cases
      cases_by_onset <- summarise_cases(sampled_linelist)

      # Summarise imported cases
      
      if (sum(imported_cases$confirm) > 0) {
        imported_cases_by_onset <- summarise_cases(imported_sampled_linelist)
      }
    }else{
      ## Apply to local cases 
      cases_by_onset <- sample_approx_dist(cases = local_cases[, cases := confirm], 
                                           dist_fn = sample_delay_fn,
                                           max_value = max_delay,
                                           earliest_allowed_mapped = earliest_allowed_onset)
      
      ## Apply to imported cases if present
      if (sum(imported_cases$confirm) > 0) {
        imported_cases_by_onset <- sample_approx_dist(cases = imported_cases[, cases := confirm], 
                                                      dist_fn = sample_delay_fn,
                                                      max_value = max_delay,
                                                      earliest_allowed_mapped = earliest_allowed_onset)
      }
      
      
      
      
    }

    
    ## Add in type variables and modify by onset if needed
    cases_by_onset <- cases_by_onset[, `:=`(type = "onset", import_status = "local")]
    
    ## Adjusted onset cases based on proportion if supplied
    if (!is.null(onset_modifier)) {
      cases_by_onset <- cases_by_onset[onset_modifier, on = 'date'][!is.na(cases)][,
                cases := as.integer(purrr::map2_dbl(cases, modifier, ~ .x * .y(n = 1, status = "local")))][, modifier := NULL]
      
    }
    
    ## Add in variable type and modify if needed for imported cases
    if (sum(imported_cases$confirm) > 0) {
      imported_cases_by_onset <- imported_cases_by_onset[, `:=`(type = "onset",
                                                                import_status = "imported")]
      
      if (!is.null(onset_modifier)) {
        imported_cases_by_onset <-  imported_cases_by_onset[onset_modifier, on = 'date'][!is.na(cases)][,
                 cases := as.integer(purrr::map2_dbl(cases, modifier, ~ .x * .y(n = 1, status = "import")))][,modifier := NULL]
      }
    }
      
    
    if (verbose) {
      message("Upscaling onsets")
    }

    ## Upscale
    cases_by_onset_upscaled <- data.table::setDT(EpiNow::adjust_for_truncation(
      cases = cases_by_onset$cases,
      dates = cases_by_onset$date,
      cum_freq = sample_delay_fn(1:nrow(cases_by_onset), dist = TRUE),
      samples = 1,
      max_upscale = max_upscale
    )[[1]])[, `:=`(type = "onset_upscaled", import_status = "local")]

    if (sum(imported_cases$confirm) > 0) {
      ## Upscale
      imported_cases_by_onset_upscaled <- data.table::setDT(EpiNow::adjust_for_truncation(
        cases = imported_cases_by_onset$cases,
        dates = imported_cases_by_onset$date,
        cum_freq = sample_delay_fn(1:nrow(imported_cases_by_onset), dist = TRUE),
        samples = 1,
        max_upscale = max_upscale
      )[[1]])[, `:=`(type = "onset_upscaled", import_status = "imported")]
    }
    
    
    if (verbose) {
      message("Upscaling infections")
    }
    
    ## Scale cases from onset to infection and upscale
    cases_by_infection <- sample_approx_dist(
      cases = cases_by_onset_upscaled,
      dist_fn = sample_incubation_fn,
      max_value = max_delay
      )[, `:=`(type = "infection", import_status = "local")]
    
    
    cases_by_infection_upscaled <- data.table::setDT(EpiNow::adjust_for_truncation(
      cases =  cases_by_infection$cases,
      dates =  cases_by_infection$date,
      cum_freq = sample_incubation_fn(1:nrow(cases_by_infection), dist = TRUE),
      confidence_adjustment = sample_delay_fn(1:nrow(cases_by_infection), dist = TRUE),
      samples = 1,
      max_upscale = max_upscale
    )[[1]])[, `:=`(type = "infection_upscaled", import_status = "local")]
    
    ## Apply to imported cases if present
    if (sum(imported_cases$confirm) > 0) {
      imported_cases_by_infection <- sample_approx_dist(
        cases = imported_cases_by_onset_upscaled, 
        dist_fn = sample_incubation_fn,
        max_value = max_delay)[, `:=`(type = "infection", import_status = "imported")]
      
      imported_cases_by_infection_upscaled <- data.table::setDT(EpiNow::adjust_for_truncation(
        cases = imported_cases_by_infection$cases,
        dates = imported_cases_by_infection$date,
        cum_freq = sample_incubation_fn(1:nrow(imported_cases_by_infection), dist = TRUE),
        confidence_adjustment = sample_delay_fn(1:nrow(imported_cases_by_infection), dist = TRUE),
        samples = 1,
        max_upscale = max_upscale
      )[[1]])[, `:=`(type = "infection_upscaled", import_status = "imported")]
      
    }
    
  
    ## Combine output
    out <- rbindlist(list(
      cases_by_onset,
      cases_by_onset_upscaled,
      cases_by_infection,
      cases_by_infection_upscaled
    ), fill = TRUE)

    ## Add in imported cases for nowcast if present
    if (sum(imported_cases$confirm) > 0) {
      
      ## Combine output
      out <- rbindlist(list(
        out,
        imported_cases_by_onset,
        imported_cases_by_onset_upscaled,
        imported_cases_by_infection,
        imported_cases_by_infection_upscaled
      ), fill = TRUE)
      
    }

    ## Add confidence if missing
    out[is.na(confidence), confidence := 1]
    
    return(out)
  }



# Nowcast samples ---------------------------------------------------------
  if (verbose) {
    message("Nowcasting using fitted delay distributions")
  }
  
  dist_defs <- list(delay = split(delay_defs[, index := 1:.N], by = "index"),
                    incubation = split(incubation_defs[, index := 1:.N], by = "index"))
  
  dist_defs <- purrr::transpose(dist_defs)

  out <- future.apply::future_lapply(dist_defs,
                                     nowcast_inner, 
                                     verbose = FALSE,
                                     max_upscale = max_upscale,
                                     future.scheduling = 20,
                                     future.packages = c("EpiNow", "data.table"))
    
  
  out <- data.table::rbindlist(out, idcol = "sample")
  
  ## Add a nowcast lag across samples
  out <- out[date <= (max(date, na.rm = TRUE) - lubridate::days(nowcast_lag))]

  return(out)
}
