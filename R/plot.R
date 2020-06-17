#' Plot a Time Series with Confidence.
#'
#' @param data Dataframe containing the follwoing variables: `date`, `median`, `type`, `bottom`,
#'  `top`, `lower`, `upper`,  and `confidence`
#' @param outer_alpha Numeric, outer alpha level.
#' @param inner_alpha Numeric, inner alpha level.
#' @param plot_median Logical, defaults to `FALSE`. Should the median be plotted.
#' @param legend Character string defaults to "none". Should a legend be displayed.
#' @return A `ggplot2` object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line scale_x_date geom_ribbon theme element_text scale_fill_manual theme labs guide_legend guides
#' @importFrom cowplot theme_cowplot
#' @importFrom data.table as.data.table copy .N rbindlist
plot_confidence <- function(data, outer_alpha = 0.1, inner_alpha = 0.2,
                            plot_median = TRUE, legend = "none") {
  
  plot <-
    ggplot2::ggplot(data, ggplot2::aes(x = date,
                                       y = median,
                                       group = type))
  
  if (plot_median) {
    plot <- plot +
      ggplot2::geom_col(alpha = 0.4)
  }
  
  plot <- plot +
    ggplot2::geom_line(ggplot2::aes(y = bottom, alpha = confidence)) +
    ggplot2::geom_line(ggplot2::aes(y = top, alpha =  confidence)) +
    ggplot2::scale_alpha(range = c(0, 0.5)) +
    cowplot::theme_cowplot() +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::scale_fill_manual(values = c("Nowcast" = "#344b85", "Forecast" = "#b33c00")) +
    ggplot2::labs(fill = "Estimate")
  
  
  ## Confident ribbons
  data <- data.table::as.data.table(data)
  conf_data <- data.table::copy(data)[confidence == 1][type %in% "nowcast"]
  
  if (nrow(conf_data) > 0) {
    plot <- plot +
      ggplot2::geom_ribbon(data = conf_data,
                           ggplot2::aes(ymin = bottom, ymax = top, fill = "Nowcast"),
                           alpha = outer_alpha) +
      ggplot2::geom_ribbon(data = conf_data,
                           ggplot2::aes(ymin = lower, ymax = upper, fill = "Nowcast"),
                           alpha = inner_alpha)
    
  }
  
  ## Not confident ribbons
  varying_conf_data <- data.table::rbindlist(list(conf_data[.N],
                                                  data[confidence != 1]), fill = TRUE)
  
  if (nrow(varying_conf_data) > 1) {
    for (i in seq(2, nrow(varying_conf_data))) {
      plot <- plot +
        ggplot2::geom_ribbon(data = varying_conf_data[seq(i - 1, i),],
                             ggplot2::aes(ymin = bottom, ymax = top, fill = "Nowcast"),
                             alpha = varying_conf_data$confidence[i] * outer_alpha) +
        ggplot2::geom_ribbon(data = varying_conf_data[seq(i - 1, i),],
                             ggplot2::aes(ymin = lower, ymax = upper, fill = "Nowcast"),
                             alpha = varying_conf_data$confidence[i] * inner_alpha)
      
    }
  }
  
  plot <- plot +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE,
                                                 override.aes = list(alpha = 0.3)),
                    alpha = FALSE, color = FALSE) + 
    ggplot2::theme(legend.position = legend)
  
  return(plot)
}


#' Add a Forecast to a Plot
#' 
#' @param plot `ggplot2` plot.
#' @param forecast Dataframe containing a forecast with the following variables: `bottom`,
#' `top`, `lower`, and `upper`.
#'
#' @return A `ggplot2` plot
#' @export
#' @importFrom ggplot2 geom_ribbon aes geom_line
plot_forecast <- function(plot = NULL, forecast = NULL) {
  
  if (nrow(forecast) > 0) {
    plot <- plot + 
      ggplot2::geom_ribbon(data = forecast,
                           ggplot2::aes(ymin = bottom, ymax = top,
                                        fill = "Forecast"),
                           alpha = 0.075) +
      ggplot2::geom_ribbon(data = forecast,
                           ggplot2::aes(ymin = lower, ymax = upper,
                                        fill = "Forecast"),
                           alpha = 0.125) + 
      ggplot2::geom_line(data = forecast, ggplot2::aes(y = bottom, alpha = 0.5)) +
      ggplot2::geom_line(data = forecast, ggplot2::aes(y = top, alpha =  0.5))
    
  }
  
  return(plot)
}


#' Plot Pipeline Results
#'
#' @param target_folder Character string, name of the folder in which to save the results.
#' @param target_date Character string, in the form "2020-01-01". Date to cast.
#' @param min_plot_date Character string, in the form "2020-01-01". Minimum date at which to start
#' plotting estimates.
#' @param report_forecast Logical, defaults to `FALSE`. Should the forecast be reported.
#' @importFrom ggplot2 ggsave theme labs coord_cartesian scale_x_date geom_hline geom_vline
#' @importFrom cowplot theme_cowplot
#' @importFrom patchwork plot_layout
#' @importFrom R.devices suppressGraphics
#' @return A `ggplot2` object
#' @export
plot_pipeline <- function(target_date = NULL, target_folder = NULL, 
                          min_plot_date = NULL, report_forecast = FALSE) {
  
  
  
  # Read in summary data ----------------------------------------------------
  
  summarised_nowcast <- readRDS(paste0(target_folder, "/summarised_nowcast.rds"))
  cases_by_report <- readRDS(paste0(target_folder, "/cases_by_report.rds"))
  case_forecast <- readRDS(paste0(target_folder, "/case_forecast.rds"))
  reff_nowcast <- readRDS(paste0(target_folder, "/bigr_estimates.rds"))[type %in% "nowcast"]
  reff_forecast <- readRDS(paste0(target_folder, "/summarised_reff.rds"))[rt_type %in% "forecast"]
  time_varying_littler <- readRDS(paste0(target_folder, "/time_varying_littler.rds"))
  
  # Detect NULL arguments ---------------------------------------------------
  
  if (is.null(case_forecast)) {
    report_forecast = FALSE
  }
  
  if (report_forecast) {
    horizon <- nrow(case_forecast)
  }else{
    horizon <- 0
  }
  
  # Plots -------------------------------------------------------------------
  
  ## Plot comparison of cases
  ## Make median NA if type nowcast
  plot_cases <-  summarised_nowcast[date >= min_plot_date][type == "nowcast", median := NA]
  
  plot_cases <- EpiNow::plot_confidence(plot_cases,
                                        legend = ifelse(report_forecast,
                                                        "bottom", "none")) +
    ggplot2::labs(y = "Daily cases", x = "Date") +
    ggplot2::geom_vline(xintercept = as.Date(target_date), linetype = 2) 
  
  if (report_forecast) {
    
    plot_cases <- 
      EpiNow::plot_forecast(plot = plot_cases, 
                            forecast = case_forecast)
  }
  
  
  suppressWarnings(
    suppressMessages(
      R.devices::suppressGraphics({
        ggplot2::ggsave(paste0(target_folder, "/cases_plot.png"),
                        plot_cases,
                        width = 12,
                        height = 3,
                        dpi = 320)
      })
    ))
  
  saveRDS(plot_cases,  paste0(target_folder, "/plot_cases.rds"))  
  
  ## Plot R estimates
  plot_bigr <- 
    EpiNow::plot_confidence(reff_nowcast[date >= min_plot_date],
                            plot_median = FALSE, 
                            legend = ifelse(report_forecast, "bottom", "none")) +
    ggplot2::labs(y = "Effective Reproduction no.", x = "Date") +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::geom_vline(xintercept = as.Date(target_date), linetype = 2) 
  
  if (report_forecast) {
    plot_bigr <- 
      EpiNow::plot_forecast(plot =  plot_bigr, 
                            forecast = reff_forecast)
  }
  
  ## Save plot
  suppressWarnings(
    suppressMessages(
      R.devices::suppressGraphics({
        ggplot2::ggsave(paste0(target_folder, "/bigr_eff_plot.png"),
                        plot_bigr,
                        width = 12,
                        height = 6,
                        dpi = 320)})
    ))
   
  
  saveRDS(plot_bigr,
          paste0(target_folder, "/bigr_eff_plot.rds"))
  
  # Pull out and plot little R ----------------------------------------------
  
  time_varying_littler <- time_varying_littler[date >= min_plot_date]
  
  ## Define generic plotting function
  plot_littler_fn <- function(littler_df, plot_var = "Rate of growth") {
    plot_littler <- 
      EpiNow::plot_confidence(littler_df[var %in% plot_var], 
                              plot_median = FALSE) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(y = "", x = "Date")
    
    return(plot_littler)
  }
  
  ## Plot each measure
  plot_littler <-  
    plot_littler_fn(time_varying_littler, 
                    plot_var = "Rate of growth") +
    ggplot2::coord_cartesian(ylim = c(-0.5, 0.5)) +
    ggplot2::labs(tag = "A")
  
  plot_doublingtime <-
    plot_littler_fn(time_varying_littler, 
                    plot_var = "Doubling/halving time (days)") +
    ggplot2::coord_cartesian(ylim = c(-40, 40)) +
    ggplot2::labs(tag = "B")
  
  plot_fit <-  
    plot_littler_fn(time_varying_littler, 
                    plot_var = "Adjusted R-squared") +
    ggplot2::labs(tag = "C")
  
  ## Combine plots
  plot_littler_summary <- suppressMessages(
    plot_littler +
      plot_doublingtime +
      plot_fit +
      patchwork::plot_layout(nrow = 3)
  )
  
  ## Save plot
  suppressWarnings(
    suppressMessages(
      R.devices::suppressGraphics({
        ggplot2::ggsave(paste0(target_folder, "/rate_spread_plot.png"),
                        plot_littler_summary,
                        width = 12,
                        height = 14,
                        dpi = 320)})
    ))
  
  saveRDS(plot_littler_summary,
          paste0(target_folder, "/rate_spread_plot.rds"))
  
  ## Summary plots
  cases <- plot_cases +
    ggplot2::labs("A") + 
    ggplot2::theme(legend.position = "none")
  
  bigr <- plot_bigr +
    ggplot2::labs("B")
  
  rt_cases_plot <- suppressWarnings(
    suppressMessages(
      cases +
        bigr +
        patchwork::plot_layout(ncol = 1) &
        ggplot2::scale_x_date(date_breaks = "1 week",
                              date_labels = "%b %d",
                              limits = c(min_plot_date,
                                         ifelse(!report_forecast, max(cases$data$date) + 1, 
                                                NA)))
    ))
  
  suppressWarnings(
    suppressMessages(
      R.devices::suppressGraphics({
        ggplot2::ggsave(paste0(target_folder, "/rt_cases_plot.png"),
                        rt_cases_plot,
                        width = 12,
                        height = 8,
                        dpi = 320)})
    ))
  
  
  saveRDS(rt_cases_plot,
          paste0(target_folder, "/rt_cases_plot.rds"))
  
  return(invisible(NULL))
}



#' Plot a Grid of Plots
#'
#' @param plot_object A character string indicating the plot object to use as the 
#' base for the grid.
#' @param ...  Additional arguments to pass to `patchwork::plot_layout`
#' @inheritParams summarise_results
#' @return A `ggplot2` object combining multiple plots
#' @export
#' @importFrom purrr map
#' @importFrom ggplot2 labs scale_x_date coord_cartesian guides
#' @importFrom stringr str_replace str_to_title
#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom purrr safely
plot_grid <- function(regions = NULL, plot_object = "bigr_eff_plot.rds", 
                      results_dir = "results", target_date = NULL, ...) {
  
  
  ## Define fn for plot loading
  load_plot <- function(region) {
    plot <- EpiNow::load_nowcast_result(plot_object, region, 
                                        date = target_date, results_dir) +
      ggplot2::labs(title = stringr::str_to_title(
        stringr::str_replace(region, "-", " ")
      )) +
      ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
    
    return(plot)
  }
  
  ## Make safe
  safe_load_plot <- purrr::safely(load_plot)
  
  plots <- suppressMessages(
    purrr::map(regions, ~ safe_load_plot(.)[[1]]))
  
  plots[-1] <- 
    purrr::map(plots[-1], function(plot){
      plot <- plot +
        ggplot2::guides(fill = FALSE)
      
      return(plot)
    })
  
  plot <- 
    patchwork::wrap_plots( plots) +
    patchwork::plot_layout(..., guides = "collect")
  
  return(suppressMessages(plot))
}


#' Plot a Summary of the Latest Results
#'
#' @param summary_results A datatable as returned by `summarise_results` (the `data` object).
#' @param x_lab A character string giving the label for the x axis, defaults to region.
#' @param log_cases Logical, should cases be shown on a logged scale. Defaults to `FALSE`
#' @return A `ggplot2` object
#' @export
#' @importFrom ggplot2 ggplot aes geom_linerange geom_hline facet_wrap theme guides labs expand_limits guide_legend element_blank scale_color_manual .data coord_cartesian
#' @importFrom cowplot theme_cowplot panel_border
#' @importFrom patchwork plot_layout
plot_summary <- function(summary_results, x_lab = "Region", log_cases = FALSE) {
  
  
  ## generic plotting function
  inner_plot <- function(df) {
    ggplot2::ggplot(df, ggplot2::aes(x = region, 
                                     col = `Expected change in daily cases`)) +
      ggplot2::geom_linerange(aes(ymin = lower, ymax = upper), size = 4, alpha = 0.7) +
      ggplot2::geom_linerange(aes(ymin = mid_lower, ymax = mid_upper), size = 4, alpha = 1) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      cowplot::theme_cowplot() +
      cowplot::panel_border() +
      ggplot2::scale_color_manual(   values = c(
        "Increasing" = "#e75f00",
        "Likely increasing" = "#fd9e49",
        "Likely decreasing" = "#5fa2ce",
        "Decreasing" = "#1170aa",
        "Unsure" = "#7b848f"), drop = FALSE) 
  }
  
  ## cases plot
  cases_plot <-  
    inner_plot(summary_results[metric %in% "New confirmed cases by infection date"]) +
    ggplot2::labs(x = x_lab, y = "") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "none")
  
  if (log_cases) {
    cases_plot <- cases_plot +
      ggplot2::scale_y_log10()
  }
  
  ## rt plot
  rt_data <- summary_results[metric %in% "Effective reproduction no."] 
  rt_plot <- 
    inner_plot(rt_data) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::labs(x = x_lab, y = "") +
    ggplot2::expand_limits(y = c(0, min(max(rt_data$upper), 4))) +
    ggplot2::coord_cartesian(ylim = c(0, min(max(rt_data$upper), 4)))
  
  
  ##join plots together
  plot <- cases_plot + rt_plot + patchwork::plot_layout(ncol = 1)
  
  return(plot)
}