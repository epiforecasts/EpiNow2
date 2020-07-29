#' Summarise Real-time Results
#'
#' @param regions An character string containing the list of regions to extract results for 
#' (must all have results for the same target date).
#' @param summaries A list of summary data frames as output by `epinow` 
#' @param results_dir An optional character string indicating the location of the results directory to extract results 
#' from.
#' @param target_date A character string indicating the target date to extract results for. All regions must have results 
#' for this date.
#' @param region_scale A character string indicating the name to give the regions being summarised.
#' @importFrom purrr partial map_chr map_dbl map_chr
#' @importFrom data.table setorderv melt merge.data.table dcast
#' @return A list of summary data
#' @export
#' @examples
#' \dontrun{
#' # see ?regional_epinow for code to generate regional results to use with this example.
#' 
#' region_sum_tab <- summarise_results(regions = regions,
#'                                     summaries = purrr::map(out$regional, ~ .$summary))
#'                   
#' region_sum_tab
#' }
#' 
summarise_results <- function(regions,
                              summaries,
                              results_dir,
                              target_date,
                              region_scale = "Region") {
  
  if (missing(target_date)) {
    target_date <- NULL
  }
  
  if(is.null(target_date)){
    target_date <- "latest"
  }
   
  if (missing(summaries)) {
    summaries <- NULL
  }
  
  if (missing(results_dir)) {
    results_dir <- NULL
  }
  
  if (is.null(results_dir)) {
    if (is.null(summaries)){ 
      stop("Either a results directory or a list of summary data frames must be supplied")
      }
  }else{
    if (!is.null(summaries)) {
      stop("Both a results directory and a list of summary data frames have been supplied.")
    }
  }
  
  if (is.null(summaries)) {
    ## Utility functions
    load_data <- purrr::partial(get_raw_result,
                                date = target_date,
                                result_dir = results_dir)
    
    
    estimates <- purrr::map(regions, ~ load_data(file = "summary.rds", region = .))
    names(estimates) <- regions
  }else{
    estimates <- summaries
  }

  
  estimates <- data.table::rbindlist(estimates, idcol = "region")
  
  numeric_estimates  <- data.table::copy(estimates)[measure %in% c("New confirmed cases by infection date",
                                                    "Effective reproduction no.")][,
                                           .(
                                             lower = numeric_estimate[[1]]$lower,
                                             upper =numeric_estimate[[1]]$upper,
                                             mid_lower = numeric_estimate[[1]]$mid_lower,
                                             mid_upper = numeric_estimate[[1]]$mid_upper
                                           ), by = .(region, measure)][,
                                              metric :=  
                                                factor(measure, levels = c("New confirmed cases by infection date",
                                                                          "Effective reproduction no."))][,
                                              measure := NULL]
  
  
  numeric_estimates <- data.table::merge.data.table(numeric_estimates, 
                                                    estimates[measure %in% "Expected change in daily cases"][,
                                                              .(region, `Expected change in daily cases` = estimate)],
                                                    by = "region", all.x = TRUE)
  ## Rank countries by incidence countries
  high_inc_regions <- unique(
    data.table::setorderv(numeric_estimates, cols = "upper", order = -1)$region)
  
  numeric_estimates <- numeric_estimates[, region := factor(region, levels = high_inc_regions)]
  
  ## Clean up joined estimate table and munge into a presentation format
  estimates <- estimates[, numeric_estimate := NULL][, 
    measure := factor(measure, levels = c("New confirmed cases by infection date",
                                          "Expected change in daily cases",
                                          "Effective reproduction no.",
                                          "Rate of growth",
                                          "Doubling/halving time (days)"))]
  
  estimates <- data.table::dcast(estimates, region ~ ..., value.var = "estimate")
  
  estimates <- estimates[, (region_scale) := region][, region := NULL]
  
  estimates <- estimates[, c(region_scale, 
                             colnames(estimates)[-ncol(estimates)]), with = FALSE]
  
  out <- list(estimates, numeric_estimates, high_inc_regions)
  
  names(out) <- c("table", "data", "regions_by_inc")
  
  return(out)
}


#' Generate Regional Summary Output
#'
#' @param summary_dir A character string giving the directory
#'  in which to store summary of results.
#' @param target_date A character string giving the target date for which to extract results
#' (in the format "yyyy-mm-dd"). Defaults to latest available estimates.
#' @param return_summary Logical, defaults to `TRUE`. Should summary measures be returned.
#' @return A list of summary measures and plots
#' @export
#' @inheritParams summarise_results
#' @inheritParams plot_summary
#' @inheritParams summarise_key_measures
#' @inheritParams regional_epinow
#' @inheritParams get_regional_results
#' @importFrom purrr map_chr
#' @importFrom ggplot2 coord_cartesian guides guide_legend ggsave ggplot_build
#' @importFrom cowplot get_legend
#' @examples
#' \donttest{
#' # Construct example distributions
#' generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
#'                         mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
#'                         sd = EpiNow2::covid_generation_times[1, ]$sd,
#'                         sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
#'                         max = 30)
#'                           
#' incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
#'                           mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
#'                           sd = EpiNow2::covid_incubation_period[1, ]$sd,
#'                           sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
#'                           max = 30)
#'                    
#' reporting_delay <- list(mean = log(10),
#'                         mean_sd = 0.8,
#'                         sd = log(2),
#'                         sd_sd = 0.1,
#'                         max = 30)
#'                         
#' # Uses example case vector from EpiSoon
#' cases <- EpiNow2::example_confirmed[1:30]
#' 
#' cases <- data.table::rbindlist(list(
#'   data.table::copy(cases)[, region := "testland"],
#'   cases[, region := "realland"]))
#'   
#' # Run basic nowcasting pipeline
#' regional_out <- regional_epinow(reported_cases = cases,
#'                                 generation_time = generation_time,
#'                                 incubation_period = incubation_period,
#'                                 reporting_delay = reporting_delay,
#'                                 samples = 2000, warmup = 200, cores = 4,
#'                                 adapt_delta = 0.95, chains = 4, verbose = TRUE,
#'                                 summary = FALSE)
#'
#' results_dir <- tempdir()             
#' 
#' regional_summary(regional_output = regional_out$regional,
#'                  reported_cases = cases,
#'                  summary_dir = results_dir,
#'                  region_scale = "Country")
#'
#' }
#' 
regional_summary <- function(regional_output,
                             reported_cases,
                             results_dir, 
                             summary_dir,
                             target_date,
                             region_scale = "Region",
                             return_summary = TRUE) {
  
  if (missing(summary_dir) & !return_summary) {
    stop("Either allow results to be returned or supply a directory for results to be saved into")
  }
  
  if (missing(summary_dir)) {
    summary_dir <- NULL
  }
  
  if (missing(results_dir)) {
    results_dir <- NULL
  }
   
  if (missing(target_date)) {
    target_date <- NULL
  }
  if (!is.null(results_dir) & !missing(regional_output)) {
    stop("Only one of results_dir and regional_output should be specified")
  }
  
  if (missing(regional_output)) {
    regional_output <- NULL
    if (!is.null(results_dir)) {
     message("Extracting results from: ", results_dir)
      
     regions <- EpiNow2::get_regions(results_dir)
     
       if (is.null(target_date)) {
         target_date <- "latest"
       }
    }
  }else{
    regions <- names(regional_output)
  }
  

  ## Get estimates
  results <- get_regional_results(regional_output,
                                  results_dir = results_dir,
                                  forecast = FALSE)
  
  ## Get latest date
  latest_date <- max(results$estimates$summarised$date, na.rm = TRUE)
  
  if (!is.null(summary_dir)) {
    ## Make summary directory
    if (!dir.exists(summary_dir)) {
      dir.create(summary_dir)
    }
    saveRDS(latest_date, file.path(summary_dir, "latest_date.rds"))
  }
  
  if (!is.null(regional_output)) {
    regional_summaries <- purrr::map(regional_output, ~ .$summary)
  }else{
    regional_summaries <- NULL
  }
  
  ## Summarise results as a table
  summarised_results <- summarise_results(regions, 
                                          summaries = regional_summaries,
                                          results_dir = results_dir,
                                          target_date = target_date,
                                          region_scale = region_scale)
  
  force_factor <- function(df) {
    df[,`Expected change in daily cases` :=
         factor(`Expected change in daily cases`,
                levels = c("Increasing", "Likely increasing", "Unsure", 
                           "Likely decreasing", "Decreasing"))] 
    
  }
  
  summarised_results$table <- force_factor(summarised_results$table)
  
  summarised_results$data <- force_factor(summarised_results$data)
  
  if (!is.null(summary_dir)) {
    data.table::fwrite(summarised_results$table, file.path(summary_dir, "summary_table.csv"))
    data.table::fwrite(summarised_results$data,  file.path(summary_dir, "summary_data.csv"))
  }

  ## Summarise results to csv
  sum_key_measures <- summarise_key_measures(regional_results = results,
                                             results_dir = results_dir, 
                                             summary_dir = summary_dir, 
                                             type = tolower(region_scale),
                                             date = target_date) 
   

  ## Adaptive add a logscale to the summary plot based on range of observed cases
  log_cases <- (max(summarised_results$data[metric %in% "New confirmed cases by infection date"]$upper, na.rm = TRUE) / 
             min(summarised_results$data[metric %in% "New confirmed cases by infection date"]$lower, na.rm = TRUE)) > 1000

  
  ## Summarise cases and Rts
  summary_plot <- EpiNow2::plot_summary(summarised_results$data,
                                        x_lab = region_scale, 
                                        log_cases = log_cases)
  
  if (!is.null(summary_dir)) {
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(file.path(summary_dir, "summary_plot.png"),
                        dpi = 330, height = 12, width = ifelse(length(regions) > 60, 24, 12))
      )
    )
  }
  
  
  high_plots <- report_plots(
    summarised_estimates = results$estimates$summarised[region %in% summarised_results$regions_by_inc[1:6]], 
    reported = reported_cases[region %in% summarised_results$regions_by_inc[1:6]]
  )
  
  high_plots$summary <- NULL
  high_plots <- purrr::map(high_plots,
                            ~ . + ggplot2::facet_wrap(~ region, scales = "fixed"))
  
  
  if (!is.null(summary_dir)) {
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(file.path(summary_dir, "high_infections_rt_plot.png"),
                        high_plots$reff, dpi = 400, width = 12, height = 12)
      ))
    
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(file.path(summary_dir, "high_infections_plot.png"), 
                        high_plots$infections, dpi = 400, width = 12, height = 12)
      ))
    
     suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(file.path(summary_dir, "high_reports_plot.png"), 
                        high_plots$reports, dpi = 400, width = 12, height = 12)
      ))
  }
  
  plots_per_row <- ifelse(length(regions) < 60, 3, 5)
  
  plots <- report_plots(summarised_estimates = results$estimates$summarised, 
                        reported = reported_cases)
  
  plots$summary <- NULL
  plots <- purrr::map(plots,
                       ~ . + ggplot2::facet_wrap(~ region, scales = "free_y",
                                                 ncol = plots_per_row))
  
  
  if (!is.null(summary_dir)) {
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(file.path(summary_dir, "rt_plot.png"), 
                        plots$reff, dpi = 330, width = 24,
                        height = 4 * round(length(regions) / plots_per_row, 0), 
                        limitsize = FALSE)
        
      ))
    
    suppressWarnings(
      suppressMessages( 
        ggplot2::ggsave(file.path(summary_dir, "infections_plot.png"), 
                        plots$infections, dpi = 330, width = 24, 
                        height =  4 * round(length(regions) / plots_per_row, 0),
                        limitsize = FALSE)
      ))
    
     suppressWarnings(
      suppressMessages( 
        ggplot2::ggsave(file.path(summary_dir, "reported_cases_plot.png"), 
                        plots$reports, dpi = 330, width = 24, 
                        height =  4 * round(length(regions) / plots_per_row, 0),
                        limitsize = FALSE)
      ))
  }

  
  
  
  if (return_summary) {
    out <- list()
    out$latest_date <- latest_date
    out$results <- results
    out$summarised_results <- summarised_results
    out$summary_plot <- summary_plot
    out$summarised_measures <- sum_key_measures
    out$high_plots <- high_plots
    out$plots <- plots
    
    return(out)
  }else{
    return(invisible(NULL))
  }
}

#' Summarise rt and cases
#'
#' @param regional_results A list of dataframes as produced by `get_regional_results`
#' @param results_dir Character string indicating the directory from which to extract results.
#' @param summary_dir Character string the directory into which to save results as a csv.
#' @param type Character string, the region identifier to apply (defaults to region).
#' @inheritParams get_regional_results
#' @return A list of summarised Rt and cases by date of infection
#' @export
#' @importFrom data.table setnames fwrite
#' @examples 
#' \dontrun{
#' 
#' 
#' summarise_key_measures(regional_results)
#' 
#' 
#' }
summarise_key_measures <- function(regional_results,
                                   results_dir, summary_dir, 
                                   type = "region", date) {
  
  if (missing(regional_results)) {
    regional_results <- NULL
  }
  
  if (is.null(regional_results)) {
    if (missing(results_dir)) {
      results_dir <- NULL
    }
    
    if (is.null(results_dir)) {
      stop("Missing results directory")
    }
    
    if (missing(date)) {
      date <- "latest"
    }
    
    timeseries <- EpiNow2::get_regional_results(results_dir = results_dir,
                                                date = date, forecast = FALSE)
  }else{
    timeseries <- regional_results 
  }
  

  

  ## Clean and save Rt estimates
  rt <- timeseries$estimates$summarised[variable == "R", 
                        .(region, date, type, median = round(median, 1),
                          lower_90 = round(bottom, 1), upper_90 = round(top, 1),
                          lower_50 = round(lower, 1), upper_50 = round(upper, 1))]
  
  data.table::setnames(rt, "region", type)
  
  if (!missing(summary_dir)) {
     if (!is.null(summary_dir)) {
       data.table::fwrite(rt, paste0(summary_dir, "/rt.csv"))
     }
  }
  
  ## Clean and save case estimates
  cases <- timeseries$estimates$summarised[variable == "infections", 
                       .(region, date, type, median = round(median, 1), lower_90 = round(bottom, 0), 
                         upper_90 = round(top, 0), lower_50 = round(lower, 0), 
                         upper_50 = round(upper, 0))]
  
  data.table::setnames(cases, "region", type)
  
  
  if (!missing(summary_dir)) {
    if (!is.null(summary_dir)) {
      data.table::fwrite(cases, paste0(summary_dir, "/cases.csv"))
    }
  }
  
  out <- list()
  out$rt <- rt
  out$cases <- cases
  
  return(out)
}
