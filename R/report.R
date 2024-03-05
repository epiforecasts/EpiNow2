#' Report case counts by date of report
#'
#' @description `r lifecycle::badge("deprecated")`
#' Convolves latent infections to reported cases via an observation model.
#' Likely to be removed/replaced in later releases by functionality drawing on
#' the `stan` implementation.
#'
#' @param case_estimates A data.table of case estimates with the following
#' variables: date, sample, cases
#'
#' @param case_forecast A data.table of case forecasts with the following
#' variables: date, sample, cases. If not supplied the default is not to
#' incorporate forecasts.
#'
#' @param reporting_effect A `data.table` giving the weekly reporting effect
#'  with the following variables: `sample` (must be the same as in `nowcast`),
#'  `effect` (numeric scaling factor for each weekday),`day` (numeric 1 - 7
#'  (1 = Monday and 7 = Sunday)). If not supplied then no weekly reporting
#'  effect is assumed.
#'
#' @return A list of `data.table`s. The first entry contains the following
#' variables `sample`, `date` and `cases` with the second being summarised
#' across samples.
#'
#' @export
#' @inheritParams estimate_infections
#' @inheritParams adjust_infection_to_report
#' @importFrom data.table data.table rbindlist
#' @importFrom future.apply future_lapply
#' @importFrom lifecycle deprecate_warn
#' @examples
#' \donttest{
#' # define example cases
#' cases <- example_confirmed[1:40]
#'
#' # get example delays
#' # Instead of running them model we use example
#' # data for speed in this example.
#' cases <- cases[, cases := as.integer(confirm)]
#' cases <- cases[, confirm := NULL][, sample := 1]
#'
#' reported_cases <- report_cases(
#'   case_estimates = cases,
#'   delays = delay_opts(example_incubation_period + example_reporting_delay),
#'   type = "sample"
#' )
#' print(reported_cases)
#' }
report_cases <- function(case_estimates,
                         case_forecast = NULL,
                         delays,
                         type = "sample",
                         reporting_effect,
                         CrIs = c(0.2, 0.5, 0.9)) {
  deprecate_warn(
    "1.5.0",
    "report_cases()"
  )
  samples <- length(unique(case_estimates$sample))

  # define delay distributions
  delay_defs <- purrr::map(
    seq_along(delays$mean_mean),
    ~ EpiNow2::lognorm_dist_def(
      mean = delays$mean_mean[.],
      mean_sd = delays$mean_sd[.],
      sd = delays$mean_mean[.],
      sd_sd = delays$mean_sd[.],
      max_value = delays$max[.],
      samples = samples
    )
  )
  # add a null reporting effect if missing
  if (missing(reporting_effect)) {
    reporting_effect <- data.table::data.table(
      sample = list(1:samples),
      effect = rep(1, 7),
      day = 1:7
    )
    reporting_effect <- reporting_effect[,
      .(sample = unlist(sample)), by = .(effect, day)
    ]
  }
  # filter and sum nowcast to use only upscaled cases by date of infection
  infections <- data.table::copy(case_estimates)

  # add in case forecast if present
  if (!is.null(case_forecast)) {
    infections <- data.table::rbindlist(list(
      infections,
      case_forecast[, .(date, sample, cases = as.integer(cases))]
    ), use.names = TRUE)
  }

  ## For each sample map to report date
  report <- future.apply::future_lapply(1:max(infections$sample),
    function(id) {
      EpiNow2::adjust_infection_to_report(infections[sample == id],
        delay_defs = purrr::map(delay_defs, ~ .[id, ]),
        type = type,
        reporting_effect = reporting_effect[sample == id, ]$effect
      )
    },
    future.seed = TRUE
  )

  report <- data.table::rbindlist(report, idcol = "sample")

  out <- list()
  # bind all samples together
  out$samples <- report
  # summarise samples
  out$summarised <- calc_summary_measures(
    report[, value := cases][, cases := NULL],
    summarise_by = "date",
    order_by = "date",
    CrIs = CrIs
  )
  return(out)
}

#' Provide Summary Statistics for Estimated Infections and Rt
#' @description `r lifecycle::badge("questioning")`
#' Creates a snapshot summary of estimates. May be removed in later releases as
#' S3 methods are enhanced.
#'
#' @param summarised_estimates A data.table of summarised estimates containing
#' the following variables: variable, median, bottom, and top. It should
#' contain the following estimates: R, infections, and r (rate of growth).
#'
#' @param rt_samples A data.table containing Rt samples with the following
#' variables: sample and value.
#'
#' @param return_numeric Should numeric summary information be returned.
#'
#' @inheritParams setup_target_folder
#'
#' @return A data.table containing formatted and numeric summary measures
#' @export
#' @importFrom data.table data.table setDT
#' @importFrom purrr map
report_summary <- function(summarised_estimates,
                           rt_samples, target_folder = NULL,
                           return_numeric = FALSE) {
  # set input to data.table
  summarised_estimates <- data.table::setDT(summarised_estimates)
  rt_samples <- data.table::setDT(rt_samples)

  CrIs <- extract_CrIs(summarised_estimates)
  max_CrI <- max(CrIs)

  # extract values of interest
  summarised_estimates <- summarised_estimates[, setdiff(
    colnames(summarised_estimates),
    c("strat", "type", "date")
  ),
  with = FALSE
  ]

  # extract latest R estimate
  R_latest <- summarised_estimates[variable == "R"][,
    variable := NULL][, purrr::map(.SD, signif, digits = 2)]

  # estimate probability of control
  prob_control <- rt_samples[,
   .(prob_control = sum(value <= 1) / .N)
  ]$prob_control
  prob_control <- signif(prob_control, 2)

  # extract current cases
  current_cases <- summarised_estimates[variable == "infections"][,
    variable := NULL][,
    purrr::map(.SD, ~ signif(as.integer(.)), 2)
  ]

  # get individual estimates
  r_latest <- summarised_estimates[variable == "growth_rate"][,
    variable := NULL][, purrr::map(.SD, signif, digits = 2)
  ]

  doubling_time <- function(r) {
    signif(log(2) * 1 / r, 2)
  }
  doubling_time_latest <- summarised_estimates[variable == "growth_rate"][
    ,
    variable := NULL
  ][
    ,
    purrr::map(.SD, doubling_time)
  ]

  # regional summary
  summary <- data.table::data.table(
    measure = c(
      "New confirmed cases by infection date",
      "Expected change in daily cases",
      "Effective reproduction no.",
      "Rate of growth",
      "Doubling/halving time (days)" # nolint
    ),
    estimate = c(
      make_conf(current_cases, max_CrI),
      as.character(EpiNow2::map_prob_change(prob_control)),
      make_conf(R_latest, max_CrI),
      make_conf(r_latest, max_CrI),
      make_conf(doubling_time_latest, max_CrI, reverse = TRUE)
    )
  )

  if (return_numeric) {
    summary$numeric_estimate <- list(
      current_cases,
      prob_control,
      R_latest,
      r_latest,
      doubling_time_latest
    )
  }

  if (!is.null(target_folder)) {
    saveRDS(summary, file.path(target_folder, "summary.rds"))
  }
  return(summary)
}



#' Report plots
#'
#' @description `r lifecycle::badge("questioning")`
#' Returns key summary plots for estimates. May be depreciated in later
#' releases as current S3 methods are enhanced.
#'
#' @param summarised_estimates A data.table of summarised estimates containing
#' the following variables: variable, median, bottom, and top.
#'
#'  It should also contain the following estimates: R, infections,
#'  reported_cases_rt, and r (rate of growth).
#'
#' @param ... Additional arguments passed to `plot_estimates()`.
#'
#' @importFrom ggplot2 ggsave theme labs scale_x_date theme_bw
#' @importFrom patchwork plot_layout
#' @importFrom data.table setDT
#' @inheritParams setup_target_folder
#' @inheritParams epinow
#' @inheritParams plot_estimates
#' @return A named list of `ggplot2` objects, `list(infections, reports, R,
#' growth_rate, summary)`, which correspond to a summary combination (last
#' item) and for the leading items.
#'
#' @seealso [plot_estimates()] of
#' `summarised_estimates[variable == "infections"]`,
#' `summarised_estimates[variable == "reported_cases"]`,
#' `summarised_estimates[variable == "R"]`, and
#' `summarised_estimates[variable == "growth_rate"]`, respectively.
#' @export
#' @examples
#' # get example output form estimate_infections
#' out <- readRDS(system.file(
#'     package = "EpiNow2", "extdata", "example_estimate_infections.rds"
#' ))
#'
#' # plot infections
#' plots <- report_plots(
#'   summarised_estimates = out$summarised,
#'   reported = out$observations
#' )
#' plots
report_plots <- function(summarised_estimates, reported,
                         target_folder = NULL, ...) {
  # set input to data.table
  summarised_estimates <- data.table::setDT(summarised_estimates)
  reported <- data.table::setDT(reported)

  # infections plot
  infections <- plot_estimates(
    estimate = summarised_estimates[variable == "infections"],
    reported = reported,
    ylab = "Cases by \n date of infection",
    ...
  )

  # cases by report ---------------------------------------------------------
  reports <- plot_estimates(
    estimate = summarised_estimates[variable == "reported_cases"],
    reported = reported, ylab = "Cases by \n date of report",
    ...
  )

  # Rt plot ------------------------------------------------------------------
  R <- plot_estimates(
    estimate = summarised_estimates[variable == "R"],
    ylab = "Effective \n reproduction no.", hline = 1,
    ...
  )

  # r plot ------------------------------------------------------------------
  growth_rate <- plot_estimates(
    estimate = summarised_estimates[variable == "growth_rate"],
    ylab = "Growth rate", hline = 0, ...
  )

  # summary plot ------------------------------------------------------------
  summary <- suppressWarnings(
    suppressMessages(
      reports +
        ggplot2::theme(legend.position = "none") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        ) +
        ggplot2::labs(tag = "A") +
        infections +
        ggplot2::theme(legend.position = "none") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        ) +
        ggplot2::labs(tag = "B") +
        R +
        ggplot2::labs(tag = "C") +
        patchwork::plot_layout(ncol = 1) &
        ggplot2::scale_x_date(
          date_breaks = "1 week",
          date_labels = "%b %d",
          limits = c(
            min(summarised_estimates[variable == "R"]$date),
            max(summarised_estimates[variable == "R"]$date)
          )
        )
    )
  )

  # Organise output
  plots <- list(
    infections = infections,
    reports = reports,
    R = R,
    growth_rate = growth_rate,
    summary = summary
  )

  if (!is.null(target_folder)) {
    suppressWarnings(suppressMessages({
      wd <- 12
      ht <- rep(3, length(plots))
      # summary plot is stack of panels
      ht[length(plots)] <- ht[length(plots)] * 4
      dpi <- 320
      pths <- file.path(target_folder, c(
        infection = "infections_plot.png", reports = "reported_plot.png",
        R = "reff_plot.png", growth_rate = "growth_rate_plot.png",
        summary = "summary_plot.png"
      ))
      mapply(
        ggplot2::ggsave, filename = pths, plot = plots,
        width = wd, height = ht, dpi = dpi
      )
    }))
  }
  return(plots)
}
