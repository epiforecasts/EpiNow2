#' Plot Estimates
#'
#' @param estimate A data.table of estimates containing the following variables: date, type
#' (must contain "estimate", "estimate based on partial data" and optionally "forecast"), 
#' @param reported A data.table of reported cases with the following variables: date, confirm.
#' @param ylab Character string, defaulting to "Cases". Title for the plot y axis.
#' @param hline Numeric, if supplied gives the horizontal intercept for a indicator line.
#' @param obs_as_col Logical, defaults to `TRUE`. Should observed data, if supplied, be plotted using columns or 
#' as points (linked using a line).
#' @param max_plot Numeric, defaults to 10. A multiplicative upper bound on the number of cases shown on the plot. Based
#' on the maximum number of reported cases. 
#' @return A `ggplot2` object
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point geom_vline geom_hline geom_ribbon scale_y_continuous
#' @importFrom scales comma 
#' @importFrom cowplot theme_cowplot
#' @importFrom data.table setDT fifelse
#' @importFrom purrr map
#' @examples
#' \donttest{
#' # define example cases
#' cases <- EpiNow2::example_confirmed[1:40]
#' 
#' # set up example delays
#' generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#' incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#' reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1), max_value = 30)
#' 
#'                         
#' # run model
#' out <- EpiNow2::estimate_infections(cases, generation_time = generation_time,
#'                                     delays = list(incubation_period, reporting_delay),
#'                                     stan_args = list(cores = ifelse(interactive(), 4, 1)))
#' # plot infections
#' plot_estimates(
#'   estimate = out$summarised[variable == "infections"],
#'   reported = cases,
#'   ylab = "Cases", max_plot = 2) + ggplot2::facet_wrap(~type, scales = "free_y")
#' 
#' # plot reported cases estimated via Rt
#' plot_estimates(estimate = out$summarised[variable == "reported_cases"],
#'                reported = cases,
#'                ylab = "Cases")
#'                
#' # plot Rt estimates
#' plot_estimates(estimate = out$summarised[variable == "R"],
#'                ylab = "Effective Reproduction No.",
#'                hline = 1)
#' }
plot_estimates <- function(estimate, reported, ylab = "Cases", hline,
                           obs_as_col = TRUE, max_plot = 10) {
  
  # convert input to data.table
  estimate <- data.table::setDT(estimate)
  if (!missing(reported)) {
    reported <- data.table::setDT(reported)
  }

  # map type to presentation form
  to_sentence <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  estimate <- estimate[, type := to_sentence(type)]
  
  # scale plot values based on reported cases
  if (!missing(reported) & !is.na(max_plot)) {
    sd_cols <- c(grep("lower_", colnames(estimate), value = TRUE),
                 grep("upper_", colnames(estimate), value = TRUE))
    cols <- setdiff(colnames(reported), c("date", "confirm", "breakpoint"))
    
    if (length(cols > 1)) {
      max_cases_to_plot <- data.table::copy(reported)[,
          .(max = round(max(confirm, na.rm = TRUE) * max_plot, 0)), by = cols]
      estimate <- estimate[max_cases_to_plot, on = cols]
    }else{
      max_cases_to_plot <- round(max(reported$confirm, na.rm = TRUE) * max_plot, 0)
      estimate <- estimate[, max := max_cases_to_plot]
    }
    estimate <- estimate[, lapply(.SD, function(var){data.table::fifelse(var > max, 
                                                            max, var)}),
                         by = setdiff(colnames(estimate), sd_cols), .SDcols = sd_cols] 
  }
  
  # initialise plot
  plot <- ggplot2::ggplot(estimate, ggplot2::aes(x = date, col = type, fill = type))
  
  # add in reported data if present (either as column or as a line)
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
  
  # plot estimates
  plot <- plot +
    ggplot2::geom_vline(
      xintercept = estimate[type == "Estimate based on partial data"][date == max(date)]$date,
      linetype = 2)
  
  # plot CrIs
  CrIs <- extract_CrIs(estimate)
  index <- 1
  alpha_per_CrI <- 0.6 / (length(CrIs) - 1)
  for (CrI in CrIs) {
    bottom <- paste0("lower_", CrI)
    top <-  paste0("upper_", CrI)
    if (index == 1) {
      plot <- plot +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]), 
                             alpha = 0.2, size = 0.05)
    }else{
      plot <- plot +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]],
                                          col = NULL), 
                             alpha = alpha_per_CrI)
    }
    index <- index + 1
  }
  
# add plot theming
plot <- plot +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::labs(y = ylab, x = "Date", col = "Type", fill = "Type") +
    ggplot2::expand_limits(y = 0) + 
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  
  # add in a horizontal line if required
  if (!missing(hline)) {
    plot <- plot + 
      ggplot2::geom_hline(yintercept = hline, linetype = 2)
  }
  return(plot)
}


#' Plot a Summary of the Latest Results
#'
#' @param summary_results A data.table as returned by `summarise_results` (the `data` object).
#' @param x_lab A character string giving the label for the x axis, defaults to region.
#' @param log_cases Logical, should cases be shown on a logged scale. Defaults to `FALSE`
#' @param max_cases Numeric, no default. The maximum number of cases to plot. 
#' @return A `ggplot2` object
#' @export
#' @importFrom ggplot2 ggplot aes geom_linerange geom_hline facet_wrap theme guides labs expand_limits guide_legend element_blank scale_color_manual .data coord_cartesian scale_y_continuous
#' @importFrom scales comma
#' @importFrom cowplot theme_cowplot panel_border
#' @importFrom patchwork plot_layout
#' @importFrom data.table setDT
plot_summary <- function(summary_results,
                         x_lab = "Region",
                         log_cases = FALSE,
                         max_cases) {
  
  # set input to data.table
  summary_results <- data.table::setDT(summary_results)
  
  # generic plotting function
  inner_plot <- function(df) {
    plot <- ggplot2::ggplot(df, ggplot2::aes(x = region, 
                                     col = `Expected change in daily cases`))
    # plot CrIs
    CrIs <- extract_CrIs(df)
    max_CrI <- max(CrIs)
    index <- 1
    alpha_per_CrI <- 0.8 / (length(CrIs) - 1)
    for (CrI in CrIs) {
      bottom <- paste0("lower_", CrI)
      top <-  paste0("upper_", CrI)
      plot <- plot +
        ggplot2::geom_linerange(ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]), 
                                alpha = ifelse(index == 1, 0.4, alpha_per_CrI),
                                size = 4)
      index <- index + 1
    }
    
    plot <- plot + 
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      cowplot::theme_cowplot() +
      cowplot::panel_border() +
      ggplot2::scale_color_manual(values = c(
        "Increasing" = "#e75f00",
        "Likely increasing" = "#fd9e49",
        "Likely decreasing" = "#5fa2ce",
        "Decreasing" = "#1170aa",
        "Unsure" = "#7b848f"), drop = FALSE) 
  }
   
  # check max_cases
  upper_CrI <- paste0("upper_", max_CrI)
  max_upper <- max(summary_results[metric %in% "New confirmed cases by infection date"][, ..upper_CrI], 
                   na.rm = TRUE)
  max_cases <- min(c(max_cases, 
                     max_upper + 1),
                   na.rm = TRUE)
  # cases plot
  cases_plot <-  
    inner_plot(summary_results[metric %in% "New confirmed cases by infection date"]) +
    ggplot2::labs(x = x_lab, y = "") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "none")
  
  if (log_cases) {
    cases_plot <- cases_plot +
      ggplot2::scale_y_log10(labels = scales::comma,
                             limits = c(NA, ifelse(!missing(max_cases), max_cases, NA)),
                             oob = scales::squish)
  }else{
    cases_plot <- cases_plot +
      ggplot2::scale_y_continuous(labels = scales::comma,
                                  limits = c(NA, ifelse(!missing(max_cases), max_cases, NA)),
                                  oob = scales::squish)
  }
  
  # rt plot
  rt_data <- summary_results[metric %in% "Effective reproduction no."] 
  rt_plot <- 
    inner_plot(rt_data) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::labs(x = x_lab, y = "") +
    ggplot2::expand_limits(y = c(0, min(max(rt_data$upper), 4))) +
    ggplot2::coord_cartesian(ylim = c(0, min(max(rt_data$upper), 4)))
  
  # join plots together
  plot <- cases_plot + rt_plot + patchwork::plot_layout(ncol = 1)
  return(plot)
}
