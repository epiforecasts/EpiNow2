#' Plot Estimates
#'
#' @param estimate A data.table of estimates containing the following variables: date, type
#' (must contain "estimate", "estimate based on partial data" and optionally "forecast"), 
#' @param reported A data.table of reported cases with the following variables: date, confirm.
#' @param ylab Character string, defaulting to "Cases". Title for the plot y axis.
#' @param hline Numeric, if supplied gives the horizontal intercept for a indicator line.
#' @param obs_as_col Logical, defaults to `TRUE`. Should observed data, if supplied, be plotted using columns or 
#' as points (linked using a line).
#'
#' @return A `ggplot2` object
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point geom_vline geom_hline geom_ribbon scale_y_continuous
#' @importFrom scales comma
#' @importFrom stringr str_to_sentence
#' @importFrom cowplot theme_cowplot
#' @examples
#' \donttest{
#' ## Define example cases
#' cases <- EpiNow2::example_confirmed[1:40]
#' 
#'  
#' ## Set up example generation time
#' generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
#'                         mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
#'                         sd = EpiNow2::covid_generation_times[1, ]$sd,
#'                         sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
#'                         max = 30)
#' ## Set                   
#' incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
#'                           mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
#'                           sd = EpiNow2::covid_incubation_period[1, ]$sd,
#'                           sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
#'                           max = 30)
#'                    
#' reporting_delay <- list(mean = log(5),
#'                         mean_sd = log(2),
#'                         sd = log(2),
#'                         sd_sd = log(1.5),
#'                         max = 30)
#'                         
#' ## Run model
#' out <- EpiNow2::estimate_infections(cases, family = "negbin",
#'                                     generation_time = generation_time,
#'                                     delays = list(incubation_period, reporting_delay),
#'                                     samples = 1000, warmup = 200, cores = ifelse(interactive(), 4, 1),
#'                                     chains = 4, horizon = 7, estimate_rt = TRUE, verbose = TRUE)
#' ## Plot infections
#' plot_estimates(
#'   estimate = out$summarised[variable == "infections"],
#'   reported = cases, 
#'   ylab = "Cases")
#' 
#' ## Plot reported cases estimated via Rt
#' plot_estimates(estimate = out$summarised[variable == "reported_cases"],
#'                reported = cases, 
#'                ylab = "Cases")
#'                
#'## Plot Rt estimates
#'plot_estimates(estimate = out$summarised[variable == "R"],
#'                ylab = "Effective Reproduction No.",
#'                hline = 1)
#' 
#' }
plot_estimates <- function(estimate, reported, ylab = "Cases", hline,
                           obs_as_col = TRUE) {
  
  ## Map type to presentation form
  estimate <- estimate[, type := stringr::str_to_sentence(type)]
  
  ## Initialise plot
  plot <- ggplot2::ggplot(estimate, ggplot2::aes(x = date, col = type, fill = type))
  
  ## Add in reported data if present (either as column or as a line)
  if (!missing(reported)) {
    if (obs_as_col) {
      plot <- plot +
        ggplot2::geom_col(data = reported[date >= min(estimate$date, na.rm = TRUE) &
                                            date <= max(estimate$date, na.rm = TRUE)],
                          ggplot2::aes(y = confirm), fill = "grey", col = "white",
                          show.legend = FALSE)
    }else{
      plot <- plot +
        ggplot2::geom_line(data = reported, 
                           ggplot2::aes(y = confirm, fill = NULL),
                           size = 1.1, alpha = 0.5, col = "black") +
        ggplot2::geom_point(data = reported,
                            ggplot2::aes(y = confirm, fill = NULL),
                            size = 1.1, alpha = 1, col = "black",
                            show.legend = FALSE)
    }
  }
  
  ## Plot estimates
  plot <- plot +
    ggplot2::geom_vline(xintercept = estimate[type == "Estimate based on partial data"][date == max(date)]$date,
                        linetype = 2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = bottom, ymax = top), 
                         alpha = 0.25, size = 0.2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, col = NULL), 
                         alpha = 0.5) +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::labs(y = ylab, x = "Date", col = "Type", fill = "Type") +
    ggplot2::expand_limits(y = 0) + 
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  
  ## Add in a horiontal line if required
  if (!missing(hline)) {
    plot <- plot + 
      ggplot2::geom_hline(yintercept = hline, linetype = 2)
  }
  
  
  return(plot)
}


#' Plot a Summary of the Latest Results
#'
#' @param summary_results A data.able as returned by `summarise_results` (the `data` object).
#' @param x_lab A character string giving the label for the x axis, defaults to region.
#' @param log_cases Logical, should cases be shown on a logged scale. Defaults to `FALSE`
#' @return A `ggplot2` object
#' @export
#' @importFrom ggplot2 ggplot aes geom_linerange geom_hline facet_wrap theme guides labs expand_limits guide_legend element_blank scale_color_manual .data coord_cartesian scale_y_continuous
#' @importFrom scales comma
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
      ggplot2::scale_y_log10(labels = scales::comma)
  }else{
    cases_plot <- cases_plot +
      ggplot2::scale_y_continuous(labels = scales::comma)
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