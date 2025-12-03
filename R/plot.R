#' Plot EpiNow2 Credible Intervals
#'
#' @description `r lifecycle::badge("stable")`
#' Adds lineranges for user specified credible intervals
#' @param plot A `{ggplot2}` plot
#'
#' @param CrIs Numeric list of credible intervals present in the data. As
#' produced by [extract_CrIs()].
#'
#' @param alpha Numeric, overall alpha of the target line range
#'
#' @param linewidth Numeric, line width of the default line range.
#'
#' @return A `{ggplot2}` plot.
plot_CrIs <- function(plot, CrIs, alpha, linewidth) {
  index <- 1
  alpha_per_CrI <- alpha / (length(CrIs) - 1)
  p <- plot
  for (CrI in CrIs) {
    bottom <- paste0("lower_", CrI)
    top <- paste0("upper_", CrI)
    if (index == 1) {
      p <- p +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]),
          alpha = 0.2, linewidth = linewidth
        )
    } else {
      p <- p +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = .data[[bottom]], ymax = .data[[top]],
            col = NULL
          ),
          alpha = alpha_per_CrI
        )
    }
    index <- index + 1
  }
  p
}

#' Plot Estimates
#'
#' @description `r lifecycle::badge("questioning")`
#' Allows users to plot the output from [estimate_infections()] easily.
#' In future releases it may be depreciated in favour of increasing the
#' functionality of the S3 plot methods.
#'
#' @param estimate A `<data.table>` of estimates containing the following
#' variables: date, type (must contain "estimate", "estimate based on partial
#' data" and optionally "forecast").
#'
#' @param reported A `<data.table>` of reported cases with the following
#' variables: date, confirm.
#'
#' @param ylab Character string. Title for the plot y
#' axis.
#'
#' @param hline Numeric, if supplied gives the horizontal intercept for a
#' indicator line.
#'
#' @param obs_as_col Logical, defaults to `TRUE`. Should observed data, if
#' supplied, be plotted using columns or as points (linked using a line).
#'
#' @param max_plot Numeric, defaults to 10. A multiplicative upper bound on the\
#' number of cases shown on the plot. Based on the maximum number of reported
#' cases.
#'
#' @param estimate_type Character vector indicating the type of data to plot.
#' Default to all types with supported options being: "Estimate", "Estimate
#' based on partial data", and "Forecast".
#'
#' @return A `ggplot2` object
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point geom_vline
#' @importFrom ggplot2 geom_hline geom_ribbon scale_y_continuous theme_bw
#' @importFrom scales comma
#' @importFrom data.table setDT fifelse copy as.data.table
#' @importFrom purrr map
#' @importFrom rlang arg_match
#' @examples
#' # get example model results
#' out <- readRDS(system.file(
#'   package = "EpiNow2", "extdata", "example_estimate_infections.rds"
#' ))
#'
#' # plot infections
#' plot_estimates(
#'   estimate = summary(out, type = "parameters", param = "infections"),
#'   reported = out$observations,
#'   ylab = "Cases", max_plot = 2
#' ) + ggplot2::facet_wrap(~type, scales = "free_y")
#'
#' # plot reported cases estimated via Rt
#' plot_estimates(
#'   estimate = summary(out, type = "parameters", param = "reported_cases"),
#'   reported = out$observations,
#'   ylab = "Cases"
#' )
#'
#' # plot Rt estimates
#' plot_estimates(
#'   estimate = summary(out, type = "parameters", param = "R"),
#'   ylab = "Effective Reproduction No.",
#'   hline = 1
#' )
#'
#' #' # plot Rt estimates without forecasts
#' plot_estimates(
#'   estimate = summary(out, type = "parameters", param = "R"),
#'   ylab = "Effective Reproduction No.",
#'   hline = 1, estimate_type = "Estimate"
#' )
plot_estimates <- function(estimate, reported, ylab, hline,
                           obs_as_col = TRUE, max_plot = 10,
                           estimate_type = c(
                             "Estimate", "Estimate based on partial data",
                             "Forecast"
                           )) {
  # convert input to data.table
  estimate <- data.table::as.data.table(estimate)
  if (!missing(reported)) {
    reported <- data.table::as.data.table(reported)
  }

  # map type to presentation form
  to_sentence <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  estimate <- estimate[, type := to_sentence(type)]

  orig_estimate <- copy(estimate)
  estimate_type <- arg_match(estimate_type, multiple = TRUE)
  # scale plot values based on reported cases
  if (!missing(reported) && !is.na(max_plot)) {
    sd_cols <- c(
      grep("lower_", colnames(estimate), value = TRUE, fixed = TRUE),
      grep("upper_", colnames(estimate), value = TRUE, fixed = TRUE)
    )
    cols <- setdiff(colnames(reported), c("date", "confirm", "breakpoint"))

    if (!all(is.na(reported$confirm))) {
      if (length(cols) > 1) {
        max_cases_to_plot <- data.table::copy(reported)[,
          .(max = round(max(confirm, na.rm = TRUE) * max_plot, 0)),
          by = cols
        ]
        estimate <- estimate[max_cases_to_plot, on = cols]
      } else {
        max_cases_to_plot <- round(
          max(reported$confirm, na.rm = TRUE) * max_plot, 0
        )
        estimate <- estimate[, max := max_cases_to_plot]
      }
      estimate <- estimate[, lapply(.SD, pmin, max),
        by = setdiff(colnames(estimate), sd_cols), .SDcols = sd_cols
      ]
    }
  }

  # initialise plot
  p <- ggplot2::ggplot(
    estimate, ggplot2::aes(x = date, col = type, fill = type)
  )

  # add in reported data if present (either as column or as a line)
  if (!missing(reported)) {
    if (obs_as_col) {
      p <- p +
        ggplot2::geom_col(
          data = reported[
            date >= min(estimate$date, na.rm = TRUE) &
              date <= max(estimate$date, na.rm = TRUE)
          ],
          ggplot2::aes(y = confirm), fill = "grey", col = "white",
          show.legend = FALSE, na.rm = TRUE
        )
    } else {
      p <- p +
        ggplot2::geom_line(
          data = reported,
          ggplot2::aes(y = confirm, fill = NULL),
          linewidth = 1.1, alpha = 0.5, col = "black", na.rm = TRUE
        ) +
        ggplot2::geom_point(
          data = reported,
          ggplot2::aes(y = confirm, fill = NULL),
          linewidth = 1.1, alpha = 1, col = "black",
          show.legend = FALSE, na.rm = TRUE
        )
    }
  }

  # plot estimates
  p <- p +
    ggplot2::geom_vline(
      xintercept = orig_estimate[
        type == "Estimate based on partial data"
      ][date == max(date)]$date,
      linetype = 2
    )

  # plot CrIs
  p <- plot_CrIs(p, extract_CrIs(estimate),
    alpha = 0.6, linewidth = 0.05
  )

  # add plot theming
  p <- p +
    ggplot2::theme_bw() +
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
    p <- p +
      ggplot2::geom_hline(yintercept = hline, linetype = 2)
  }
  p
}


#' Plot a Summary of the Latest Results
#'
#' @description `r lifecycle::badge("questioning")`
#' Used to return a summary plot across regions (using results generated by
#' [summarise_results()]).
#'
#' May be depreciated in later releases in favour of enhanced S3 methods.
#'
#' @param summary_results A data.table as returned by [summarise_results()] (the
#' `data` object).
#'
#' @param x_lab A character string giving the label for the x axis, defaults to
#' region.
#'
#' @param log_cases Logical, should cases be shown on a logged scale. Defaults
#' to `FALSE`.
#'
#' @param max_cases Numeric, no default. The maximum number of cases to plot.
#'
#' @return A `{ggplot2}` object
#' @export
#' @importFrom ggplot2 ggplot aes geom_linerange geom_hline facet_wrap
#' @importFrom ggplot2 theme guides labs expand_limits guide_legend
#' @importFrom ggplot2 scale_color_manual .data coord_cartesian
#' @importFrom ggplot2 theme_bw element_blank scale_y_continuous
#' @importFrom scales comma
#' @importFrom patchwork plot_layout
#' @importFrom data.table setDT
plot_summary <- function(summary_results,
                         x_lab = "Region",
                         log_cases = FALSE,
                         max_cases) {
  # set input to data.table
  summary_results <- data.table::setDT(summary_results)

  # extract CrIs
  CrIs <- extract_CrIs(summary_results)
  max_CrI <- max(CrIs)

  # generic plotting function
  inner_plot <- function(df) {
    p <- ggplot2::ggplot(df, ggplot2::aes(
      x = region,
      col = `Expected change in reports`
    ))
    # plot CrIs
    index <- 1
    alpha_per_CrI <- 0.8 / (length(CrIs) - 1)
    for (CrI in CrIs) {
      bottom <- paste0("lower_", CrI)
      top <- paste0("upper_", CrI)
      p <- p +
        ggplot2::geom_linerange(
          ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]),
          alpha = ifelse(index == 1, 0.4, alpha_per_CrI),
          linewidth = 4
        )
      index <- index + 1
    }

    p <- p +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::facet_wrap(~metric, ncol = 1, scales = "free_y") +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values = c(
        Increasing = "#e75f00",
        "Likely increasing" = "#fd9e49",
        "Likely decreasing" = "#5fa2ce",
        Decreasing = "#1170aa",
        Stable = "#7b848f"
      ), drop = FALSE)
  }

  # check max_cases
  upper_CrI <- paste0("upper_", max_CrI) # nolint
  max_upper <- max(
    summary_results[
      metric == "New infections per day"
    ][, ..upper_CrI],
    na.rm = TRUE
  )
  max_cases <- min(
    c(
      max_cases,
      max_upper + 1
    ),
    na.rm = TRUE
  )
  # cases plot
  cases_plot <-
    inner_plot(
      summary_results[metric == "New infections per day"]
    ) +
    ggplot2::labs(x = x_lab, y = "") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
    ) +
    ggplot2::theme(legend.position = "none")

  if (log_cases) {
    cases_plot <- cases_plot +
      ggplot2::scale_y_log10(
        labels = scales::comma,
        limits = c(NA, ifelse(!missing(max_cases), max_cases, NA)),
        oob = scales::squish
      )
  } else {
    cases_plot <- cases_plot +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        limits = c(NA, ifelse(!missing(max_cases), max_cases, NA)),
        oob = scales::squish
      )
  }

  # rt plot
  rt_data <- summary_results[metric == "Effective reproduction no."]
  uppers <- grepl("upper_", colnames(rt_data), fixed = TRUE) # nolint
  max_rt <- max(data.table::copy(rt_data)[, ..uppers], na.rm = TRUE)
  rt_plot <-
    inner_plot(rt_data) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(col = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::labs(x = x_lab, y = "") +
    ggplot2::expand_limits(y = c(0, min(max_rt, 4))) +
    ggplot2::coord_cartesian(ylim = c(0, min(max_rt, 4)))

  # join plots together
  cases_plot + rt_plot + patchwork::plot_layout(ncol = 1)
}

#' Plot method for estimate_infections
#'
#' @description `r lifecycle::badge("maturing")`
#' `plot` method for class `<estimate_infections>`.
#'
#' @param x A list of output as produced by `estimate_infections`
#'
#' @param ... Pass additional arguments to report_plots
#' @importFrom rlang arg_match
#' @inheritParams select_plots
#' @inheritParams calc_summary_measures
#'
#' @seealso [report_plots()]
#' @method plot estimate_infections
#' @return List of plots as produced by [report_plots()]
#' @export
plot.estimate_infections <- function(x,
                                     type = "summary",
                                     CrIs = c(0.2, 0.5, 0.9),
                                     ...) {
  out <- report_plots(
    summarised_estimates = summary(x, type = "parameters", CrIs = CrIs),
    reported = x$observations, ...
  )
  select_plots(out, type)
}

#' Plot method for forecast_infections
#'
#' @description `r lifecycle::badge("maturing")`
#' `plot` method for class `<forecast_infections>`.
#'
#' @param x A list of output as produced by `forecast_infections`
#'
#' @param ... Pass additional arguments to report_plots
#' @importFrom rlang arg_match
#' @inheritParams select_plots
#' @inheritParams calc_summary_measures
#'
#' @seealso [report_plots()] [forecast_infections()]
#' @method plot forecast_infections
#' @return List of plots as produced by [report_plots()]
#' @export
plot.forecast_infections <- function(x,
                                     type = "summary",
                                     CrIs = c(0.2, 0.5, 0.9),
                                     ...) {
  plot.estimate_infections(x, type = type, CrIs = CrIs, ...)
}

#' Plot method for epinow
#'
#' @description `r lifecycle::badge("maturing")`
#' `plot` method for class `<epinow>`.
#' @param x A list of output as produced by [epinow()].
#' @inheritParams plot.estimate_infections
#' @seealso [plot.estimate_infections()] [report_plots()]
#' @method plot epinow
#' @return List of plots as produced by [report_plots()]
#' @export
plot.epinow <- function(x, type = "summary", ...) {
  out <- report_plots(
    summarised_estimates = x$estimates$summarised,
    reported = x$estimates$observations, ...
  )
  select_plots(out, type)
}

#' Internal helper function to select plots from those created by
#' [report_plots()]
#'
#' @param plots A list of plots as produced by [report_plots()].
#' @param type A character vector indicating the name of the plot to return.
#' Defaults to  "summary" with supported options being "infections", "reports",
#' "R", "growth_rate", "summary", "all". If "all" is supplied all plots are
#' generated.
#' @return Selected plots by type
#' @keywords internal
select_plots <- function(plots, type = c(
  "summary", "infections", "reports", "R",
  "growth_rate", "all"
)) {
  type <- arg_match(type)
  if (type == "all") {
    type <- c("summary", "infections", "reports", "R", "growth_rate")
  }

  if (!is.null(plots)) {
    plots <- plots[type]
    if (length(type) == 1) {
      plots <- plots[[1]]
    }
    plots
  } else {
    invisible(NULL)
  }
}
