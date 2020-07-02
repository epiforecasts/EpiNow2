#' Summarise Realtime Results
#'
#' @param regions A character string containing the list of regions to extract results for 
#' (must all have results for the same target date).
#' @param results_dir A character string indicating the location of the results directory to extract results 
#' from.
#' @param target_date A character string indicating the target date to extract results for. All regions must have results 
#' for this date.
#' @param region_scale A character string indicating the name to give the regions being summarised.
#' @importFrom purrr partial map_chr map_dbl map_chr
#' @importFrom data.table setorderv melt
#' @return A list of summary data
#' @export
#' @examples
#' \dontrun{
#' # see ?regional_summary for code to generate results for this example
#' regions <- list(realland = "realland", testland = "testland")
#' 
#' out <- summarise_results(regions = regions,
#'                   results_dir = "../test")
#'                   
#' out
#' }
#' 
summarise_results <- function(regions = NULL,
                              results_dir,
                              target_date,
                              region_scale = "Region") {
  
  if (missing(target_date)) {
    target_date <- "latest"
  }
   
  ## Utility functions
  load_data <- purrr::partial(get_raw_result,
                              date = target_date,
                              result_dir = results_dir)
  
  
  estimates <- purrr::map(regions, ~ load_data(file = "summary.rds", region = .))
  
  names(estimates) <- names(regions)
  
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
                                              measure := metric]
  
  ## Rank countries by incidence countires
  high_inc_regions <- unique(
    data.table::setorderv(numeric_estimates, cols = "upper", order = -1)$region)
  
  numeric_estimates <- numeric_estimates[, region := factor(region, levels = high_inc_regions)]
  
  
  estimates <- estimates[, (region_scale) := region][, region := NULL]
  
  estimates <- estimates[, c((region_scale), 
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
#' @param csv_region_label Character string indicating the label to assign to a region when saving to .csv.
#' @param return_summary Logical, defaults to `TRUE`. Should summary measures be returned.
#' @return A list of summary measures and plots
#' @export
#' @inheritParams summarise_results
#' @inheritParams plot_summary
#' @inheritParams summarise_key_measures
#' @importFrom purrr map_chr
#' @importFrom ggplot2 coord_cartesian guides guide_legend ggsave ggplot_build
#' @importFrom cowplot get_legend
#' @examples
#' 
#' \dontrun{
#' ## Requires additional packages:
#' library(EpiSoon)
#' library(forecastHybrid)
#' 
#' ## Construct example distributions
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
#' ## Uses example case vector from EpiSoon
#' cases <- EpiNow2::example_confirmed[1:50]
#' 
#' cases <- data.table::rbindlist(list(
#'   data.table::copy(cases)[, region := "testland"],
#'   cases[, region := "realland"]))
#'   
#' ## Run basic nowcasting pipeline
#' out <- regional_epinow(reported_cases = cases,
#'                        target_folder = "../test",
#'                        generation_time = generation_time,
#'                        incubation_period = incubation_period,
#'                        reporting_delay = reporting_delay,
#'                        forecast_model = function(y, ...){
#'                          EpiSoon::forecastHybrid_model(
#'                             y = y[max(1, length(y) - 21):length(y)],
#'                             model_params = list(models = "aefz", weights = "equal"),
#'                             forecast_params = list(PI.combination = "mean"), ...)},
#'                        samples = 1000, warmup = 500, cores = 2, chains = 2,
#'                        verbose = TRUE)
#'                        
#'## Example asssumes that CovidGlobalNow (github.com/epiforecasts/covid-global) is  
#'## in the directory above the root.
#' regional_summary(results_dir = "../test",
#'                  summary_dir = "../test-summary",
#'                  region_scale = "Country")
#'
#' }
#' 

regional_summary <- function(results_dir, 
                             summary_dir,
                             target_date = "latest",
                             region_scale = "Region",
                             csv_region_label = "region",
                             log_cases = FALSE,
                             return_summary = TRUE) {
  
  if (missing(summary_dir) & !return_summary) {
    stop("Either allow results to be returned or supply a directory for results to be saved into")
  }
   
  message("Extracting results from: ", results_dir)
  
  ## Make summary directory
  if (!dir.exists(summary_dir)) {
    dir.create(summary_dir)
  }
  
  regions <- EpiNow2::get_regions(results_dir)
  
  if (target_date %in% "latest") {
    plot_date <- Sys.Date() 
  }else{
    plot_date <- as.Date(target_date)
  }
  
  ## Get estimates
  results <- EpiNow2::get_regional_results(results_dir, forecast = FALSE)
  
  ## Get latest date
  latest_date <- max(results[, .SD[date == max(date)], by = .(region)]$date)
  
  if (!missing(summary_dir)) {
    saveRDS(latest_date, file.path(summary_dir, "latest_date.rds"))
  }

  ## Summarise results as a table
  results <- EpiNow2::summarise_results(regions, results_dir,
                                       target_date = target_date,
                                       region_scale = region_scale)
  
  message("Saving results summary table")
  
  force_factor <- function(df) {
    df[,`Expected change in daily cases` :=
         factor(`Expected change in daily cases`,
                levels = c("Increasing", "Likely increasing", "Unsure", 
                           "Likely decreasing", "Decreasing"))] 
    
  }
  
  results$table <- force_factor(results$table)
  
  results$data <- force_factor(results$data)
  
  if (!missing(summary_dir)) {
    saveRDS(results$table, file.path(summary_dir, "summary_table.rds"))
    saveRDS(results$data, file.path(summary_dir, "summary_data.rds"))
  }

  ## Summarise results to csv
  message("Saving Rt and case csvs")
  
  sum_key_measures <- EpiNow2::summarise_key_measures(results_dir = results_dir, 
                                                      summary_dir = ifelse(missing(summary_dir), NULL, summary_dir), 
                                                      type = csv_region_label,
                                                      date = target_date) 
  
  
  message("Plotting results summary")
  
  ## Summarise cases and Rts
  summary_plot <- EpiNow2::plot_summary(results$data,
                                       x_lab = region_scale, 
                                       log_cases = log_cases)
  
  if (!missing(summary_dir)) {
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(file.path(summary_dir, "summary_plot.png"),
                        dpi = 330, height = 12, width = ifelse(length(regions) > 60, 24, 12))
      )
    )
  }
  
  
  message("Plotting summary Rt and case plots")
  
  
  
  
  ## Plot highest incidence countries
  high_cases_rt_plot <- suppressWarnings(
    suppressMessages(
      plot_grid(regions[names(regions) %in% results$regions_by_inc[1:6]], 
                plot_object = "bigr_eff_plot.rds",
                results_dir, target_date = target_date, ncol = 2))
  )
  
  
  ## Check the plots for forecast and adapt date and legend accordingly
  data_date <- as.Date(max(
    ggplot2::ggplot_build(high_cases_rt_plot[[1]])$layout$panel_scales_x[[1]]$range$range
  ), origin = "1970-01-01"
  )
  
  legend <- 'gtable' %in% class(try(cowplot::get_legend(high_cases_rt_plot[[1]]), silent = TRUE))
  
  
  ## Adapt legend
  high_cases_rt_plot <- suppressWarnings( suppressMessages(
    high_cases_rt_plot &
      ggplot2::coord_cartesian(ylim = c(0, 3)) &
      ggplot2::scale_x_date(date_breaks = "1 week",
                            date_labels = "%b %d",
                            limits = c(as.Date(NA_character_),
                                       max(data_date, plot_date))) &
      ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))
  )
  )
  
  if (!missing(summary_dir)) {
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(file.path(summary_dir, "high_cases_rt_plot.png"),
                        high_cases_rt_plot, dpi = 400, width = 12, height = 12)
      ))
  }

  
  
  high_cases_plot <- suppressWarnings(
    suppressMessages(
      EpiNow2::plot_grid(regions[names(regions) %in% results$regions_by_inc[1:6]],
                        plot_object = "plot_cases.rds",
                        results_dir, target_date = target_date, ncol = 2) &
        ggplot2::scale_x_date(date_breaks = "1 week",
                              date_labels = "%b %d",
                              limits = c(as.Date(NA_character_), 
                                         max(data_date, plot_date))) &
        ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))
    ))
  
  if (!missing(summary_dir)) {
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(file.path(summary_dir, "high_cases_plot.png"), 
                        high_cases_plot, dpi = 400, width = 12, height = 12)
      ))
  }

  
  
  message("Plotting overall Rt and case plots")
  
  plots_per_row <- ifelse(length(regions) < 60, 3, 5)
  
  ## Plot all countries
  rt_plot <- suppressWarnings(
    suppressMessages(
      EpiNow2::plot_grid(regions, plot_object = "bigr_eff_plot.rds",
                        results_dir, target_date = target_date, ncol = plots_per_row) &
        ggplot2::coord_cartesian(ylim = c(0, 3)) &
        ggplot2::scale_x_date(date_breaks = "1 week",
                              date_labels = "%b %d",
                              limits = c(as.Date(NA_character_), max(data_date, plot_date))) &
        ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))
    ))
  
  if (!missing(summary_dir)) {
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(file.path(summary_dir, "rt_plot.png"), 
                        rt_plot, dpi = 330, width = 24, height = 4 * round(length(regions) / plots_per_row, 0), limitsize = FALSE)
        
      ))
  }

  
  cases_plot <- 
    plot_grid(regions, plot_object = "plot_cases.rds",
              results_dir, target_date = target_date, ncol = plots_per_row) &
    ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))
  
  
  
  if (!missing(summary_dir)) {
    suppressWarnings(
      suppressMessages( 
        ggplot2::ggsave(file.path(summary_dir, "cases_plot.png"), 
                        cases_plot, dpi = 330, width = 24, height =  4 * round(length(regions) / plots_per_row, 0), limitsize = FALSE)
      ))
  }

  
  
  if (return_summary) {
    out <- list()
    out$latest_date <- latest_date
    out$summary <- results
    out$summary_plot <- summary_plot
    out$summarised_measures <- sum_key_measures
    out$high_cases_rt_plot <- high_cases_rt_plot
    out$high_cases_plot <- high_cases_plot
    out$rt_plot <- rt_plot
    out$cases_plot <- cases_plot
    
    return(out)
  }else{
    return(invisible(NULL))
  }
}

#' Summarise rt and cases
#'
#' @param results_dir Character string indicating the directory from which to extract results.
#' @param summary_dir Character string the directory into which to save results as a csv.
#' @param type Character string, the region identifier to apply (defaults to region).
#' @inheritParams get_regional_results
#' @return A list of summarised Rt and cases by date of infection
#' @export
#' @importFrom data.table setnames fwrite
#' @examples 
#' \dontrun{
#' # see ?regional_summary for code to produce test results
#' summarise_key_measures(results_dir = "../test")
#' 
#' 
#' }
summarise_key_measures <- function(results_dir, summary_dir, 
                                   type = "country", date) {
  
  if (missing(results_dir)) {
    stop("Missing results directory")
  }
  
  if (missing(date)) {
    date <- "latest"
  }
  
  timeseries <- EpiNow2::get_regional_results(results_dir, date = date, forecast = FALSE)
  
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