#' Summarise Real-time Results
#'
#' @description `r lifecycle::badge("questioning")`
#' Used internally by `regional_summary` to produce a summary table of results.
#' May be streamlined in later releases.
#'
#' @param regions An character string containing the list of regions to extract
#' results for (must all have results for the same target date).
#'
#' @param summaries A list of summary `<data.frame>`s as output by `epinow`
#'
#' @param results_dir An optional character string indicating the location of
#' the results directory to extract results from.
#'
#' @param target_date A character string indicating the target date to extract
#' results for. All regions must have results
#' for this date.
#'
#' @param region_scale A character string indicating the name to give the
#' regions being summarised.
#'
#' @importFrom purrr safely map_chr map_dbl map_chr
#' @importFrom data.table setorderv melt merge.data.table dcast
#' @importFrom cli cli_abort
#' @return A list of summary data
#' @keywords internal
summarise_results <- function(regions,
                              summaries = NULL,
                              results_dir = NULL,
                              target_date = "latest",
                              region_scale = "Region") {
  if (is.null(results_dir)) {
    if (is.null(summaries)) {
      cli_abort(
        c(
          "!" = "One of a {.var results_dir} or {.var summary}
          must be supplied."
        )
      )
    }
  } else if (!is.null(summaries)) {
    cli_abort(
      c(
        "!" = "Cannot supply both {.var results_dir} and {.var summary}.",
        "i" = "Only one of {.var results_dir} or {.var summary} should be
        supplied."
      )
    )
  }

  if (is.null(summaries)) {
    ## Utility functions
    get_result <- function(file, region) {
      get_raw_result(
        file = file,
        region = region,
        date = target_date,
        result_dir = results_dir
      )
    }

    load_data <- purrr::safely(get_result) # nolint

    estimates <- purrr::map(
      regions, ~ load_data(file = "summary.rds", region = .)[[1]]
    )
    names(estimates) <- regions
  } else {
    estimates <- summaries
  }

  estimates <- data.table::rbindlist(estimates, idcol = "region", fill = TRUE)
  numeric_estimates <-
    data.table::copy(estimates)[measure %in% c(
      "New infections per day",
      "Effective reproduction no."
    )][
      ,
      .(
        data.table::data.table(region, measure, estimate),
        data.table::rbindlist(numeric_estimate, fill = TRUE)
      )
    ][
      ,
      metric := factor(measure, levels = c(
        "New infections per day",
        "Effective reproduction no."
      ))
    ][, measure := NULL]


  numeric_estimates <- data.table::merge.data.table(numeric_estimates,
    estimates[measure == "Expected change in reports"][
      ,
      .(region,
        `Expected change in reports` = estimate,
        prob_control = numeric_estimate
      )
    ],
    by = "region", all.x = TRUE
  )
  # rank countries by incidence countries
  high_inc_regions <- unique(
    data.table::setorderv(numeric_estimates, cols = "median", order = -1)$region
  )

  numeric_estimates <- numeric_estimates[
    ,
    region := factor(region, levels = high_inc_regions)
  ]

  # clean up joined estimate table and munge into a presentation format
  estimates <- estimates[, numeric_estimate := NULL][
    ,
    measure := factor(measure, levels = c(
      "New infections per day",
      "Expected change in reports",
      "Effective reproduction no.",
      "Rate of growth",
      "Doubling/halving time (days)" # nolint
    ))
  ]

  estimates <- data.table::dcast(
    estimates, region ~ ...,
    value.var = "estimate"
  )
  estimates <- estimates[, (region_scale) := region][, region := NULL]
  estimates <- estimates[,
    c(region_scale, colnames(estimates)[-ncol(estimates)]),
    with = FALSE
  ]

  out <- list(estimates, numeric_estimates, high_inc_regions)
  names(out) <- c("table", "data", "regions_by_inc")
  return(out)
}


#' Regional Summary Output
#'
#' @description `r lifecycle::badge("maturing")`
#' Used to produce summary output either internally in `regional_epinow` or
#'  externally.
#' @param summary_dir A character string giving the directory
#'  in which to store summary of results.
#' @param target_date A character string giving the target date for which to
#'  extract results
#' (in the format "yyyy-mm-dd"). Defaults to latest available estimates.
#' @param all_regions Logical, defaults to `TRUE`. Should summary plots for all
#'  regions be returned rather than just regions of interest.
#' @param plot Logical, defaults to `TRUE`. Should regional summary plots be
#' produced.
#' @param ... Additional arguments passed to `report_plots`.
#' @return A list of summary measures and plots
#' @export
#' @seealso regional_epinow
#' @inheritParams summarise_results
#' @inheritParams plot_summary
#' @inheritParams plot_estimates
#' @inheritParams summarise_key_measures
#' @inheritParams regional_epinow
#' @inheritParams get_regional_results
#' @inheritParams report_plots
#' @inheritParams epinow
#' @importFrom purrr map_chr compact
#' @importFrom ggplot2 coord_cartesian guides guide_legend ggsave ggplot_build
#' @importFrom data.table setDT fcase
#' @importFrom futile.logger flog.info
#' @importFrom cli cli_abort
#' @examples
#' # get example output from regional_epinow model
#' regional_out <- readRDS(system.file(
#'   package = "EpiNow2", "extdata", "example_regional_epinow.rds"
#' ))
#'
#' regional_summary(
#'   regional_output = regional_out$regional,
#'   data = regional_out$summary$reported_cases
#' )
regional_summary <- function(regional_output = NULL,
                             data,
                             results_dir = NULL,
                             summary_dir = NULL,
                             target_date = NULL,
                             region_scale = "Region",
                             all_regions = TRUE,
                             return_output = is.null(summary_dir),
                             plot = TRUE,
                             max_plot = 10,
                             ...) {
  reported_cases <- data.table::setDT(data)
  if (is.null(summary_dir)) {
    futile.logger::flog.info(
      "No summary directory specified so returning summary output"
    )
  } else {
    futile.logger::flog.info("Saving summary to : %s", summary_dir)
  }

  if (!is.null(results_dir) && !is.null(regional_output)) {
    cli_abort(
      c(
        "!" = "Both {.var results_dir} and {.var regional_output} cannot be
        specified.",
        "i" = "Only supply one of {.var results_dir} or
        {.var regional_output}."
      )
    )
  }

  if (is.null(regional_output)) {
    if (!is.null(results_dir)) {
      futile.logger::flog.info("Extracting results from: %s", results_dir)
      regions <- get_regions(results_dir)
      if (is.null(target_date)) {
        target_date <- "latest"
      }
    }
  } else {
    regions <- names(regional_output)
    regional_output <- purrr::compact(regional_output)
  }

  futile.logger::flog.trace("Getting regional results")
  # get estimates
  results <- get_regional_results(regional_output,
    results_dir = results_dir,
    date = target_date,
    samples = FALSE,
    forecast = FALSE
  )

  # get latest date
  latest_date <- unique(reported_cases[confirm > 0][date == max(date)]$date)

  if (!is.null(summary_dir)) {
    # make summary directory
    if (!dir.exists(summary_dir)) {
      dir.create(summary_dir, recursive = TRUE)
    }
    saveRDS(latest_date, file.path(summary_dir, "latest_date.rds"))
    data.table::fwrite(
      reported_cases, file.path(summary_dir, "reported_cases.csv")
    )
  }

  if (!is.null(regional_output)) {
    regional_summaries <- purrr::map(regional_output, ~ .$summary)
  } else {
    regional_summaries <- NULL
  }
  futile.logger::flog.trace("Summarising results")

  # summarise results to csv
  sum_key_measures <- summarise_key_measures(
    regional_results = results,
    results_dir = results_dir,
    summary_dir = summary_dir,
    type = tolower(region_scale),
    date = target_date
  )

  # summarise results as a table
  summarised_results <- summarise_results(regions,
    summaries = regional_summaries,
    results_dir = results_dir,
    target_date = target_date,
    region_scale = region_scale
  )

  force_factor <- function(df) {
    df[, `Expected change in reports` :=
      factor(`Expected change in reports`,
        levels = c(
          "Increasing", "Likely increasing", "Stable",
          "Likely decreasing", "Decreasing"
        )
      )]
  }
  summarised_results$table <- force_factor(summarised_results$table)
  summarised_results$data <- force_factor(summarised_results$data)

  if (!is.null(summary_dir)) {
    data.table::fwrite(
      summarised_results$table, file.path(summary_dir, "summary_table.csv")
    )
    data.table::fwrite(
      summarised_results$data, file.path(summary_dir, "summary_data.csv")
    )
  }

  # adaptive add a logscale to the summary plot based on range of observed cases
  current_inf <- summarised_results$data[
    metric == "New infections per day"
  ]
  uppers <- grepl("upper_", colnames(current_inf), fixed = TRUE) # nolint
  lowers <- grepl("lower_", colnames(current_inf), fixed = TRUE) # nolint
  log_cases <- (max(current_inf[, ..uppers], na.rm = TRUE) /
                  (min(current_inf[, ..lowers], na.rm = TRUE) + 1)) > 1000

  max_reported_cases <- round(
    max(reported_cases$confirm, na.rm = TRUE) * max_plot, 0
  )

  if (plot) {
    # summarise cases and Rts
    summary_plot <- plot_summary(summarised_results$data,
      x_lab = region_scale,
      log_cases = log_cases,
      max_cases = max_reported_cases
    )

    if (!is.null(summary_dir)) {
      save_ggplot <- function(plot, name, height = 12, width = 12, ...) {
        suppressWarnings(
          suppressMessages(
            ggplot2::ggsave(file.path(summary_dir, name),
              plot,
              dpi = 300, width = width,
              height = height, ...
            )
          )
        )
      }
      save_ggplot(summary_plot, "summary_plot.png",
        width = data.table::fcase(
          length(regions) > 60 & length(regions) > 120, 36,
          length(regions) > 60 & length(regions) <= 120, 24,
          default = 12
        )
      )
    }
    # extract regions with highest number of reported cases in the last week
    most_reports <- get_regions_with_most_reports(reported_cases,
      time_window = 7,
      no_regions = 6
    )

    high_plots <- report_plots(
      summarised_estimates = results$estimates$summarised[
        region %in% most_reports
      ],
      reported = reported_cases[region %in% most_reports],
      max_plot = max_plot, ...
    )

    high_plots$summary <- NULL
    high_plots <-
      purrr::map(
        high_plots,
        ~ . + ggplot2::facet_wrap(~region, scales = "free_y", ncol = 2)
      )

    if (!is.null(summary_dir)) {
      save_ggplot(high_plots$R, "high_rt_plot.png")
      save_ggplot(high_plots$infections, "high_infections_plot.png")
      save_ggplot(high_plots$reports, "high_reported_cases_plot.png")
    }

    if (all_regions) {
      plots_per_row <- data.table::fcase(
        length(regions) > 60 & length(regions) > 120, 8,
        length(regions) > 60 & length(regions) <= 120, 5,
        default = 3
      )

      plots <- report_plots(
        summarised_estimates = results$estimates$summarised,
        reported = reported_cases,
        max_plot = max_plot, ...
      )

      plots$summary <- NULL
      plots <- purrr::map(
        plots,
        ~ . + ggplot2::facet_wrap(~region,
          scales = "free_y",
          ncol = plots_per_row
        )
      )

      if (!is.null(summary_dir)) {
        save_big_ggplot <- function(plot, name) {
          save_ggplot(plot, name,
            height = 3 * round(length(regions) / plots_per_row, 0),
            width = 24,
            limitsize = FALSE
          )
        }
        save_big_ggplot(plots$R, "rt_plot.png")
        save_big_ggplot(plots$infections, "infections_plot.png")
        save_big_ggplot(plots$reports, "reported_cases_plot.png")
      }
    }
  } else {
    summary_plot <- NULL
    high_plots <- NULL
  }

  if (return_output) {
    out <- list()
    out$latest_date <- latest_date
    out$results <- results
    out$summarised_results <- summarised_results
    out$summary_plot <- summary_plot
    out$summarised_measures <- sum_key_measures
    out$reported_cases <- reported_cases
    out$high_plots <- high_plots

    if (all_regions && plot) {
      out$plots <- plots
    }
    return(out)
  } else {
    return(invisible(NULL))
  }
}

#' Summarise rt and cases
#'
#' @description `r lifecycle::badge("maturing")`
#' Produces summarised `<data.frame>`s of output across regions.
#' Used internally by `regional_summary`.
#'
#' @param regional_results A list of dataframes as produced by
#' `get_regional_results`
#'
#' @param results_dir Character string indicating the directory from which to
#' extract results.
#'
#' @param summary_dir Character string the directory into which to save results
#' as a csv.
#'
#' @param type Character string, the region identifier to apply (defaults to
#' region).
#'
#' @inheritParams get_regional_results
#' @importFrom cli cli_abort
#' @seealso regional_summary
#' @return A list of summarised Rt, cases by date of infection and cases by
#' date of report
#' @keywords internal
#' @importFrom data.table setnames fwrite setorderv
summarise_key_measures <- function(regional_results = NULL,
                                   results_dir = NULL, summary_dir = NULL,
                                   type = "region", date = "latest") {
  if (is.null(regional_results)) {
    if (is.null(results_dir)) {
      cli_abort(
        c(
          "!" = "{.var results_dir} must be specified."
        )
      )
    }
    timeseries <- EpiNow2::get_regional_results(
      results_dir = results_dir,
      date = date, forecast = FALSE,
      samples = FALSE
    )
  } else {
    timeseries <- regional_results
  }
  summarise_variable <- function(df, dof = Inf) {
    cols <- setdiff(names(df), c("region", "date", "type", "strat"))
    if (!is.null(dof)) {
      df[, (cols) := round(.SD, dof), .SDcols = cols]
    }
    data.table::setorderv(df, cols = c("region", "date", "type", "strat"))
    data.table::setnames(df, "region", type)
    return(df)
  }

  save_variable <- function(df, name) {
    if (!is.null(summary_dir)) {
      data.table::fwrite(df, paste0(summary_dir, "/", name, ".csv"))
    }
  }
  out <- list()
  sum_est <- timeseries$estimates$summarised
  # clean and save Rt estimates
  out$rt <- summarise_variable(sum_est[variable == "R"][, variable := NULL])
  save_variable(out$rt, "rt")

  # clean and save growth rate estimates
  out$growth_rate <- summarise_variable(sum_est[variable == "growth_rate"][
    ,
    variable := NULL
  ])
  save_variable(out$growth_rate, "growth_rate")

  # clean and save case estimates
  out$cases_by_infection <- summarise_variable(
    sum_est[variable == "infections"][
      ,
      variable := NULL
    ], 1
  )
  save_variable(out$cases_by_infection, "cases_by_infection")

  # clean and save case estimates
  out$cases_by_report <- summarise_variable(
    sum_est[variable == "reported_cases"][
      ,
      variable := NULL
    ], 1
  )
  save_variable(out$cases_by_report, "cases_by_report")
  return(out)
}

#' Summarise Regional Runtimes
#'
#' @description `r lifecycle::badge("maturing")`
#' Used internally by `regional_epinow` to summarise region run times.
#'
#' @seealso regional_summary regional_epinow
#' @inheritParams regional_summary
#' @inheritParams epinow
#' @return A data.table of region run times
#' @export
#' @importFrom data.table data.table fwrite
#' @importFrom purrr map safely map_vec
#' @importFrom cli cli_abort
#' @keywords internal
#' @examples
#' regional_out <- readRDS(system.file(
#'   package = "EpiNow2", "extdata", "example_regional_epinow.rds"
#' ))
#' regional_runtimes(regional_output = regional_out$regional)
regional_runtimes <- function(regional_output = NULL,
                              target_folder = NULL,
                              target_date = NULL,
                              return_output = FALSE) {
  if (is.null(target_folder) && is.null(regional_output)) {
    cli_abort(
      c(
        "i" = "Either an output should be passed in or a target folder
        specified."
      )
    )
  }
  if (is.null(target_folder)) {
    futile.logger::flog.info(
      "No target directory specified so returning timings"
    )
    return_output <- TRUE
  } else {
    futile.logger::flog.info(
      "Saving timings information to : %s", target_folder
    )
  }
  if (!is.null(regional_output)) {
    timings <- data.table::data.table(
      region = names(regional_output),
      # purrr::map_vec will preserve the difftime class
      time = unlist(purrr::map_vec(regional_output, ~ .$timing))
    )
  } else {
    if (is.null(target_date)) {
      target_date <- "latest"
    }
    safe_read <- purrr::safely(readRDS) # nolint
    regions <- get_regions(target_folder)
    timings <- data.table::data.table(
      region = regions,
      time = unlist(purrr::map(regions, ~ safe_read(file.path(
        target_folder, ., target_date,
        "runtime.rds"
      )))[[1]])
    )
  }
  if (!is.null(target_folder)) {
    data.table::fwrite(timings, file.path(target_folder, "runtimes.csv"))
  }
  if (return_output) {
    return(timings)
  } else {
    return(invisible(NULL))
  }
}

#' Calculate Credible Interval
#'
#' @description `r lifecycle::badge("stable")`
#' Adds symmetric a credible interval based on quantiles.
#' @param samples A data.table containing at least a value variable
#' @param summarise_by A character vector of variables to group by.
#' @param CrI Numeric between 0 and 1. The credible interval for which to
#' return values. Defaults to 0.9.
#' @return A data.table containing the upper and lower bounds for the specified
#' credible interval.
#' @export
#' @importFrom data.table copy setDT
#' @importFrom stats quantile
#' @examples
#' samples <- data.frame(value = 1:10, type = "car")
#' # add 90% credible interval
#' calc_CrI(samples)
#' # add 90% credible interval grouped by type
#' calc_CrI(samples, summarise_by = "type")
calc_CrI <- function(samples, summarise_by = NULL, CrI = 0.9) {
  samples <- data.table::setDT(samples)
  CrI_half <- CrI / 2
  lower_CrI <- 0.5 - CrI_half
  upper_CrI <- 0.5 + CrI_half
  CrI_scale <- round(100 * CrI, 1)
  with_CrI <-
    data.table::copy(samples)[, .(
      value = quantile(value, c(lower_CrI, upper_CrI), na.rm = TRUE),
      CrI = c(paste0("lower_", CrI_scale), paste0("upper_", CrI_scale))
    ),
    by = summarise_by
    ]
  return(with_CrI)
}


#' Calculate Credible Intervals
#'
#' @description `r lifecycle::badge("stable")`
#' Adds symmetric credible intervals based on quantiles.
#'
#' @param CrIs Numeric vector of credible intervals to calculate.
#'
#' @inheritParams calc_CrI
#'
#' @return A data.table containing the `summarise_by` variables and the
#' specified lower and upper credible intervals.
#'
#' @importFrom purrr map
#' @importFrom data.table rbindlist dcast
#' @export
#' @examples
#' samples <- data.frame(value = 1:10, type = "car")
#' # add credible intervals
#' calc_CrIs(samples)
#' # add 90% credible interval grouped by type
#' calc_CrIs(samples, summarise_by = "type")
calc_CrIs <- function(samples, summarise_by = NULL, CrIs = c(0.2, 0.5, 0.9)) {
  CrIs <- sort(CrIs)
  with_CrIs <- purrr::map(CrIs, ~ calc_CrI(
    samples = samples,
    summarise_by = summarise_by,
    CrI = .
  ))

  with_CrIs <- data.table::rbindlist(with_CrIs)
  scale_CrIs <- round(CrIs * 100, 1)
  # nolint start
  order_CrIs <- c(
    paste0("lower_", rev(scale_CrIs)), paste0("upper_", scale_CrIs)
  )
  # nolint end
  with_CrIs <- data.table::dcast(
    with_CrIs, ... ~ factor(CrI, levels = order_CrIs),
    value.var = "value"
  )
  return(with_CrIs)
}

#' Extract Credible Intervals Present
#'
#' @description `r lifecycle::badge("stable")`
#' Helper function to extract the credible intervals present in a
#' `<data.frame>`.
#' @param summarised A `<data.frame>` as processed by `calc_CrIs`
#' @return A numeric vector of credible intervals detected in
#' the `<data.frame>`.
#' @export
#' @examples
#' samples <- data.frame(value = 1:10, type = "car")
#' summarised <- calc_CrIs(samples,
#'   summarise_by = "type",
#'   CrIs = c(seq(0.05, 0.95, 0.05))
#' )
#' extract_CrIs(summarised)
extract_CrIs <- function(summarised) {
  CrIs <- grep("lower_", colnames(summarised), value = TRUE, fixed = TRUE)
  CrIs <- gsub("lower_", "", CrIs, fixed = TRUE)
  as.numeric(CrIs)
}

#' Calculate Summary Statistics
#'
#' @description `r lifecycle::badge("stable")`
#' Calculate summary statistics from a `<data.frame>` by group.
#' Currently supports the mean, median and standard deviation.
#' @return A data.table containing the upper and lower bounds for the specified
#' credible interval
#' @export
#' @inheritParams calc_CrI
#' @importFrom data.table copy setDT
#' @examples
#' samples <- data.frame(value = 1:10, type = "car")
#' # default
#' calc_summary_stats(samples)
#' #  by type
#' calc_summary_stats(samples, summarise_by = "type")
calc_summary_stats <- function(samples, summarise_by = NULL) {
  samples <- data.table::setDT(samples)
  sum_stats <-
    data.table::copy(samples)[, .(
      median = median(value, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE)
    ),
    by = summarise_by
    ]
  return(sum_stats)
}

#' Calculate All Summary Measures
#'
#' @description `r lifecycle::badge("stable")`
#' Calculate summary statistics and credible intervals from a `<data.frame>` by
#' group.
#'
#' @param order_by A character vector of parameters to order by, defaults to
#' all `summarise_by` variables.
#'
#' @return A data.table containing summary statistics by group.
#'
#' @export
#' @inheritParams calc_CrIs
#' @importFrom data.table setorderv
#' @examples
#' samples <- data.frame(value = 1:10, type = "car")
#' # default
#' calc_summary_measures(samples)
#' #  by type
#' calc_summary_measures(samples, summarise_by = "type")
calc_summary_measures <- function(samples,
                                  summarise_by = NULL,
                                  order_by = NULL,
                                  CrIs = c(0.2, 0.5, 0.9)) {
  if (is.null(summarise_by)) {
    summarise_by <- setdiff(names(samples), "value")
  }
  if (is.null(order_by)) {
    order_by <- summarise_by
  }

  CrIs <- calc_CrIs(
    samples = samples,
    summarise_by = summarise_by,
    CrIs = CrIs
  )
  sum_stats <- calc_summary_stats(
    samples = samples,
    summarise_by = summarise_by
  )

  summarised <- sum_stats[CrIs, on = summarise_by]
  data.table::setorderv(summarised, cols = order_by)
  return(summarised)
}


#' Summary output from epinow
#'
#' @description `r lifecycle::badge("stable")`
#'  \code{summary} method for class "epinow".
#' @param object A list of output as produced by "epinow".
#'
#' @param output A character string of output to summarise. Defaults to
#' "estimates" but also supports "forecast", and "estimated_reported_cases".
#'
#' @inheritParams summary.estimate_infections
#'
#' @importFrom rlang arg_match
#'
#' @param ... Pass additional summary arguments to lower level methods
#'
#' @seealso summary.estimate_infections epinow
#' @aliases summary
#' @method summary epinow
#' @return Returns a `<data.frame>` of summary output
#' @export
summary.epinow <- function(object,
                           output = c(
                             "estimates", "forecast", "estimated_reported_cases"
                           ),
                           date = NULL, params = NULL,
                           ...) {
  output <- arg_match(output)
  if (output == "estimates") {
    out <- summary(object$estimates,
      date = date,
      params = params, ...
    )
  } else {
    out <- object[[output]]$summarised
    if (!is.null(date)) {
      target_date <- as.Date(date)
      out <- out[date == target_date]
    }
    if (!is.null(params)) {
      out <- out[variable == params]
    }
  }
  return(out)
}

#' Summary output from estimate_infections
#'
#' @description `r lifecycle::badge("stable")`
#' \code{summary} method for class "estimate_infections".
#'
#' @param object A list of output as produced by "estimate_infections".
#'
#' @param type A character vector of data types to return. Defaults to
#' "snapshot" but also supports "parameters", and "samples". "snapshot" return
#' a summary at a given date (by default the latest date informed by data).
#' "parameters" returns summarised parameter estimates that can be further
#' filtered using `params` to show just the parameters of interest and date.
#' "samples" similarly returns posterior
#' samples.
#'
#' @param date A date in the form "yyyy-mm-dd" to inspect estimates for.
#'
#' @param params A character vector of parameters to filter for.
#'
#' @param ... Pass additional arguments to `report_summary`
#' @importFrom rlang arg_match
#' @seealso summary estimate_infections report_summary
#' @method summary estimate_infections
#' @return Returns a `<data.frame>` of summary output
#' @export
summary.estimate_infections <- function(object,
                                        type = c(
                                          "snapshot", "parameters", "samples"
                                        ),
                                        date = NULL, params = NULL, ...) {
  type <- arg_match(type)
  if (is.null(date)) {
    target_date <- unique(
      object$summarised[type != "forecast"][date == max(date)]$date
    )
  } else {
    target_date <- as.Date(date)
  }

  if (type == "snapshot") {
    out <- report_summary(
      summarised_estimates = object$summarised[date == target_date],
      rt_samples = object$samples[variable == "R"][
        date == target_date, .(sample, value)
      ],
      ...
    )
  } else if (type %in% c("parameters", "samples")) {
    if (type == "parameters") {
      type <- "summarised"
    }
    out <- object[[type]]
    if (!is.null(date)) {
      out <- out[date == target_date]
    }
    if (!is.null(params)) {
      out <- out[variable %in% params]
    }
  }
  return(out)
}
