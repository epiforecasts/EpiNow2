#' Summarise a nowcast
#'
#' @param nowcast A dataframe as produced by `nowcast_pipeline`
#' @return A summarised dataframe
#' @export
#' @importFrom data.table copy setorder
#' @importFrom purrr map_dbl
#' @importFrom HDInterval hdi
#' @importFrom lubridate days
summarise_cast <- function(nowcast) {
  
  get_conf <- function(conf, import_status) {
    if(length(conf) == 2) {
      out <- conf[which(import_status == "local")]
    }else if(length(conf) == 1) {
      out <- conf
    }
    return(out)
  }
  
  ## Make an explict copy
  summarised_cast <- data.table::copy(nowcast)
  
  ## SUmmarises cases by reference across sample, data and type
  summarised_cast <- summarised_cast[
    , .(cases = sum(cases), confidence = get_conf(confidence, import_status)),
    by = .(sample, date, type)]
  
  ## Create CI and other summary measures
  summarised_cast <- summarised_cast[, .(
    bottom  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.9)), ~ .[[1]])),
    top = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.9)), ~ .[[2]])),
    lower  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.5)), ~ .[[1]])),
    upper = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.5)), ~ .[[2]])),
    median = as.numeric(median(cases, na.rm = TRUE)),
    mean = as.numeric(mean(cases, na.rm = TRUE)),
    confidence = as.numeric(mean(confidence, na.rm = TRUE))
  ), by = .(date, type)]
  
  data.table::setorder(summarised_cast, date)  
  
  return(summarised_cast)
  
}



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
#' @importFrom stringr str_split
#' @return A list of summary data
#' @export
summarise_results <- function(regions = NULL,
                              results_dir = "results",
                              target_date = NULL,
                              region_scale = "Region") {
  
  ## Utility functions
  load_data <- purrr::partial(load_nowcast_result,
                              date = target_date, result_dir = results_dir)
  
  
  ## Make reporting table
  estimates <- data.table::data.table(
    region = names(regions),
    `New confirmed cases by infection date` = 
      purrr::map(regions, ~ load_data("current_cases.rds", .)),
    `Expected change in daily cases` = map_prob_change(
      purrr::map_dbl(regions, ~ load_data("prob_control_latest.rds", .))
    ),
    `Effective reproduction no.` =
      purrr::map(regions, ~ load_data("bigr_eff_latest.rds", .)),
    `Doubling/halving time (days)` = 
      purrr::map_chr(regions, ~ load_data("doubling_time_latest.rds", .))) 
  
  
  ## Make estimates numeric
  numeric_estimates <- estimates[,
                                 .(
                                   region,
                                   `New confirmed cases by infection date`, 
                                   `Effective reproduction no.`, 
                                   `Expected change in daily cases`  
                                 )] 
  
  numeric_estimates <- 
    data.table::melt(numeric_estimates,
                     measure.vars = c("New confirmed cases by infection date",
                                      "Effective reproduction no."),
                     variable.name = "metric", value.name = "value")
  
  numeric_estimates  <-  numeric_estimates[,
                                           `:=`(
                                             lower = purrr::map_dbl(value, ~ .[[1]]$lower),
                                             upper = purrr::map_dbl(value, ~ .[[1]]$upper),
                                             mid_lower = purrr::map_dbl(value, ~ .[[1]]$mid_lower),
                                             mid_upper = purrr::map_dbl(value, ~ .[[1]]$mid_upper)
                                           )][,
                                              metric :=  
                                                factor(metric, levels = c("New confirmed cases by infection date",
                                                                          "Effective reproduction no."))]
  
  ## Rank countries by incidence countires
  high_inc_regions <- unique(
    data.table::setorderv(numeric_estimates, 
                          cols = "upper", order = -1)$region)
  
  numeric_estimates <- numeric_estimates[,
                                         region :=  
                                           factor(region, levels = high_inc_regions)]
  
  estimates <- 
    estimates[,
              `:=`(
                `New confirmed cases by infection date` = EpiNow::make_conf(
                  purrr::map(`New confirmed cases by infection date`, ~ .[[1]]),
                  digits = 0),
                `Effective reproduction no.` = EpiNow::make_conf(
                  purrr::map(`Effective reproduction no.`, ~ .[[1]]),
                  digits = 1))]
  
  
  
  estimates <- 
    estimates[, (region_scale) := region][, region := NULL]
  
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
#' (in the format "yyyy-mm-dd").
#' @param csv_region_label Character string indicating the label to assign to a region when saving to .csv.
#' @return NULL
#' @export
#' @inheritParams summarise_results
#' @inheritParams plot_summary
#' @inheritParams summarise_to_csv
#' @importFrom purrr map_chr
#' @importFrom ggplot2 coord_cartesian guides guide_legend ggsave ggplot_build
#' @importFrom cowplot get_legend
#' @examples
#' 
#' \dontrun{
#' 
#'## Example asssumes that CovidGlobalNow (github.com/epiforecasts/covid-global) is  
#'## in the directory above the root.
#' regional_summary(results_dir = "../covid-global/national",
#'                  summary_dir = "../covid-global/national-summary",
#'                  target_date = "2020-03-19",
#'                  region_scale = "Country")
#'
#' }
#' 

regional_summary <- function(results_dir = NULL, 
                             summary_dir = NULL,
                             target_date = NULL,
                             region_scale = "Region",
                             csv_region_label = "region",
                             log_cases = FALSE) {
  
  
  message("Extracting results from: ", results_dir)
  
  ## Make summary directory
  if (!dir.exists(summary_dir)) {
    dir.create(summary_dir)
  }
  
  regions <- EpiNow::get_regions(results_dir)
  
  if (target_date %in% "latest") {
    plot_date <- Sys.Date() 
  }else{
    plot_date <- as.Date(target_date)
  }
  
  ## Get latest date
  latest_date <- 
    purrr::map_chr(regions, ~ as.character(
      EpiNow::load_nowcast_result("latest_date.rds", region = .,
                                  target_date, results_dir)))
  latest_date <- as.Date(latest_date)
  latest_date <- max(latest_date, na.rm = TRUE)
  
  saveRDS(latest_date, file.path(summary_dir, "latest_date.rds"))
  
  ## Summarise results as a table
  results <- EpiNow::summarise_results(regions, results_dir,
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
  
  saveRDS(results$table, file.path(summary_dir, "summary_table.rds"))
  saveRDS(results$data, file.path(summary_dir, "summary_data.rds"))
  
  
  ## Summarise results to csv
  message("Saving Rt and case csvs")
  
  
  EpiNow::summarise_to_csv(results_dir = results_dir, 
                           summary_dir = summary_dir, 
                           type = csv_region_label,
                           date = target_date) 
  
  
  message("Plotting results summary")
  
  ## Summarise cases and Rts
  summary_plot <- EpiNow::plot_summary(results$data,
                                       x_lab = region_scale, 
                                       log_cases = log_cases)
  
  
  suppressWarnings(
    suppressMessages(
      ggplot2::ggsave(file.path(summary_dir, "summary_plot.png"),
                      dpi = 330, height = 12, width = ifelse(length(regions) > 60, 24, 12))
    )
  )
  
  
  
  
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
  
  
  suppressWarnings(
    suppressMessages(
      ggplot2::ggsave(file.path(summary_dir, "high_cases_rt_plot.png"),
                      high_cases_rt_plot, dpi = 400, width = 12, height = 12)
    ))
  
  
  high_cases_plot <- suppressWarnings(
    suppressMessages(
      EpiNow::plot_grid(regions[names(regions) %in% results$regions_by_inc[1:6]],
                        plot_object = "plot_cases.rds",
                        results_dir, target_date = target_date, ncol = 2) &
        ggplot2::scale_x_date(date_breaks = "1 week",
                              date_labels = "%b %d",
                              limits = c(as.Date(NA_character_), 
                                         max(data_date, plot_date))) &
        ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))
    ))
  
  
  suppressWarnings(
    suppressMessages(
      ggplot2::ggsave(file.path(summary_dir, "high_cases_plot.png"), 
                      high_cases_plot, dpi = 400, width = 12, height = 12)
    ))
  
  
  message("Plotting overall Rt and case plots")
  
  plots_per_row <- ifelse(length(regions) < 60, 3, 5)
  
  ## Plot all countries
  rt_plot <- suppressWarnings(
    suppressMessages(
      EpiNow::plot_grid(regions, plot_object = "bigr_eff_plot.rds",
                        results_dir, target_date = target_date, ncol = plots_per_row) &
        ggplot2::coord_cartesian(ylim = c(0, 3)) &
        ggplot2::scale_x_date(date_breaks = "1 week",
                              date_labels = "%b %d",
                              limits = c(as.Date(NA_character_), max(data_date, plot_date))) &
        ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))
    ))
  
  suppressWarnings(
    suppressMessages(
      ggplot2::ggsave(file.path(summary_dir, "rt_plot.png"), 
                      rt_plot, dpi = 330, width = 24, height = 4 * round(length(regions) / plots_per_row, 0), limitsize = FALSE)
      
    ))
  
  cases_plot <- 
    plot_grid(regions, plot_object = "plot_cases.rds",
              results_dir, target_date = target_date, ncol = plots_per_row) &
    ggplot2::theme(legend.position = ifelse(legend, "bottom", "none"))
  
  suppressWarnings(
    suppressMessages( 
      ggplot2::ggsave(file.path(summary_dir, "cases_plot.png"), 
                      cases_plot, dpi = 330, width = 24, height =  4 * round(length(regions) / plots_per_row, 0), limitsize = FALSE)
    ))
  
  
  return(invisible(NULL))
}

#' Summarise rt and cases as a csv
#'
#' @param results_dir Character string indicating the directory from which to extract results
#' @param summary_dir Character string the directory into which to save results
#' @param type Character string, the region identifier to apply
#' @inheritParams get_timeseries
#' @return NULL
#' @export
#' @importFrom data.table as.data.table setnames fwrite
summarise_to_csv <- function(results_dir = NULL, summary_dir = NULL, 
                             type = "country", date = NULL) {
  
  
  
  timeseries <- EpiNow::get_timeseries(results_dir, date = date, summarised = TRUE)
  
  ## Clean and save Rt estimates
  rt <- data.table::as.data.table(timeseries$rt)[type %in% "nowcast", 
                        .(region, date, type = rt_type, median = round(median, 1),
                          lower_90 = round(bottom, 1), upper_90 = round(top, 1),
                          lower_50 = round(lower, 1), upper_50 = round(upper, 1), 
                          prob_control = round(prob_control, 2))]
  
  data.table::setnames(rt, "region", type)
  
  
  data.table::fwrite(rt, paste0(summary_dir, "/rt.csv"))
  
  ## Clean and save case estimates
  cases <- data.table::as.data.table(timeseries$incidence)[type %in% "nowcast", 
                       .(region, date, median = round(median, 1), lower_90 = round(bottom, 0), 
                         upper_90 = round(top, 0), lower_50 = round(lower, 0), 
                         upper_50 = round(upper, 0), confidence = round(confidence, 2))]
  
  data.table::setnames(cases, "region", type)
  
  
  data.table::fwrite(cases, paste0(summary_dir, "/cases.csv"))
  
  return(invisible(NULL))
}