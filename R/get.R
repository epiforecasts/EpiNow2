#' Get Folders with Results
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param results_dir A character string giving the directory in which results
#'  are stored (as produced by `regional_rt_pipeline`).
#'
#' @return A named character vector containing the results to plot.
#' @author Sam Abbott
#' @export
get_regions <- function(results_dir) {
  # regions to include - based on folder names
  regions <- list.dirs(results_dir,
    recursive = FALSE,
    full.names = FALSE
  )

  # put into alphabetical order
  regions <- regions[!(regions %in% "runtimes.csv")]
  regions <- sort(regions)
  names(regions) <- regions
  return(regions)
}
#' Get a Single Raw Result
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param file Character string giving the result files name.
#'
#' @param region Character string giving the region of interest.
#'
#' @param date Target date (in the format `"yyyy-mm-dd`).
#'
#' @param result_dir Character string giving the location of the target
#' directory.
#'
#' @return An R object read in from the targeted .rds file
#' @author Sam Abbott
#' @export
get_raw_result <- function(file, region, date,
                           result_dir) {
  file_path <- file.path(result_dir, region, date, file)
  object <- readRDS(file_path)
  return(object)
}
#' Get Combined Regional Results
#'
#' @description `r lifecycle::badge("stable")`
#' Summarises results across regions either from input or from disk. See the
#' examples for details.
#'
#' @param regional_output A list of output as produced by `regional_epinow` and
#' stored in the `regional` list.
#'
#' @param results_dir A character string indicating the folder containing the
#' `EpiNow2` results to extract.
#'
#' @param date A Character string (in the format "yyyy-mm-dd") indicating the
#' date to extract data for. Defaults to "latest" which finds the latest
#' results available.
#'
#' @param samples Logical, defaults to `TRUE`. Should samples be returned.
#'
#' @param forecast Logical, defaults to `FALSE`. Should forecast results be
#' returned.
#'
#' @return A list of estimates, forecasts and estimated cases by date of report.
#' @author Sam Abbott
#' @export
#' @importFrom purrr map safely
#' @importFrom data.table rbindlist
#' @examples
#' # get example multiregion estimates
#' regional_out <- example_regional_epinow
#' # from output
#' results <- get_regional_results(regional_out$regional, samples = FALSE)
get_regional_results <- function(regional_output,
                                 results_dir, date,
                                 samples = TRUE,
                                 forecast = FALSE) {
  if (missing(regional_output)) {
    regional_output <- NULL
  }

  if (is.null(regional_output)) {
    # assign to latest likely date if not given
    if (missing(date)) {
      date <- "latest"
    }
    # find all regions
    regions <- get_regions(results_dir)

    load_data <- purrr::safely(EpiNow2::get_raw_result) # nolint

    # get estimates
    get_estimates_file <- function(samples_path, summarised_path) {
      out <- list()

      if (samples) {
        samples <- purrr::map(regions, ~ load_data(samples_path, .,
          result_dir = results_dir,
          date = date
        )[[1]])
        samples <- data.table::rbindlist(samples, idcol = "region", fill = TRUE)
        out$samples <- samples
      }
      # get incidence values and combine
      summarised <- purrr::map(regions, ~ load_data(summarised_path, .,
        result_dir = results_dir,
        date = date
      )[[1]])
      summarised <- data.table::rbindlist(
        summarised, idcol = "region", fill = TRUE
      )
      out$summarised <- summarised
      return(out)
    }
    out <- list()
    out$estimates <- get_estimates_file(
      samples_path = "estimate_samples.rds",
      summarised_path = "summarised_estimates.rds"
    )

    if (forecast) {
      out$estimated_reported_cases <- get_estimates_file(
        samples_path = "estimated_reported_cases_samples.rds",
        summarised_path = "summarised_estimated_reported_cases.rds"
      )
    }
  } else {
    get_estimates_data <- function(data) {
      out <- list()
      if (samples) {
        samples <- purrr::map(regional_output, ~ .[[data]]$samples)
        samples <- data.table::rbindlist(samples, idcol = "region", fill = TRUE)
        out$samples <- samples
      }
      # get incidence values and combine
      summarised <- purrr::map(regional_output, ~ .[[data]]$summarised)
      summarised <- data.table::rbindlist(
        summarised, idcol = "region", fill = TRUE
      )
      out$summarised <- summarised
      return(out)
    }
    out <- list()
    out$estimates <- get_estimates_data("estimates")
    if (forecast) {
      out$estimated_reported_cases <-
        get_estimates_data("estimated_reported_cases")
    }
  }
  return(out)
}
#' Get a Literature Distribution
#'
#'
#' @description `r lifecycle::badge("stable")`
#' Search a data frame for a distribution and return it in the format expected
#' by `delay_opts()` and the `generation_time` argument of `epinow` and
#' `estimate_infections`.
#'
#' @param data A `data.table` in the format of `generation_times`.
#'
#' @param disease A character string indicating the disease of interest.
#'
#' @param source A character string indicating the source of interest.
#'
#' @param max_value Numeric, the maximum value to allow. Defaults to 15 days.
#'
#' @param fixed Logical, defaults to `FALSE`. Should distributions be supplied
#' as fixed values (vs with uncertainty)?
#'
#' @return A list defining a distribution
#'
#' @author Sam Abbott
#' @export
#' @examples
#' get_dist(
#'  EpiNow2::generation_times, disease = "SARS-CoV-2", source = "ganyani"
#' )
get_dist <- function(data, disease, source, max_value = 15, fixed = FALSE) {
  target_disease <- disease
  target_source <- source
  data <- data[disease == target_disease][source == target_source]
  dist <- as.list(data[, .(mean, mean_sd, sd, sd_sd, max = max_value, dist)])
  if (fixed) {
    dist$mean_sd <- 0
    dist$sd_sd <- 0
  }
  return(do.call(dist_spec, dist))
}
#'  Get a Literature Distribution for the Generation Time
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts a literature distribution from `generation_times`.
#'
#' @inheritParams get_dist
#' @inherit get_dist
#' @export
#' @author Sam Abbott
#' @examples
#' get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
get_generation_time <- function(disease, source, max_value = 15,
                                fixed = FALSE) {
  dist <- get_dist(EpiNow2::generation_times,
    disease = disease, source = source,
    max_value = max_value, fixed = fixed
  )

  return(dist)
}
#'  Get a Literature Distribution for the Incubation Period
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts a literature distribution from `incubation_periods`.
#'
#' @inheritParams get_dist
#' @inherit get_dist
#' @author Sam Abbott
#' @export
#' @examples
#' get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
get_incubation_period <- function(disease, source, max_value = 15,
                                  fixed = FALSE) {
  dist <- get_dist(EpiNow2::incubation_periods,
    disease = disease, source = source,
    max_value = max_value, fixed = fixed
  )

  return(dist)
}
#' Get Regions with Most Reported Cases
#'
#' @description `r lifecycle::badge("stable")`
#' Extract a vector of regions with the most reported cases in a set time
#' window.
#'
#' @param time_window Numeric, number of days to include from latest date in
#' data. Defaults to 7 days.
#'
#' @param no_regions Numeric, number of regions to return. Defaults to 6.
#'
#' @inheritParams regional_epinow
#'
#' @return A character vector of regions with the highest reported cases
#'
#' @author Sam Abbott
#' @importFrom data.table copy setorderv
#' @importFrom lubridate days
#' @export
get_regions_with_most_reports <- function(reported_cases,
                                          time_window = 7,
                                          no_regions = 6) {
  most_reports <- data.table::copy(reported_cases)
  most_reports <-
    most_reports[,
      .SD[date >= (max(date, na.rm = TRUE) - lubridate::days(time_window))
    ],
      by = "region"
    ]
  most_reports <- most_reports[,
   .(confirm = sum(confirm, na.rm = TRUE)), by = "region"
  ]
  most_reports <- data.table::setorderv(
    most_reports, cols = "confirm", order = -1
  )
  most_reports <- most_reports[1:no_regions][!is.na(region)]$region
  return(most_reports)
}

##' Estimate seeding time from delays and generation time
##'
##' The seeding time is set to the mean of the specified delays, constrained
##' to be at least the maximum generation time
##' @param delays Delays as specified using `dist_spec`
##' @param generation_time Generation time as specified using `dist_spec`
##' @return An integer seeding time
##' @author Sebastian Funk
get_seeding_time <- function(delays, generation_time) {
  # Estimate the mean delay -----------------------------------------------
  seeding_time <- sum(mean(delays))
  if (seeding_time < 1) {
    seeding_time <- 1
  } else {
    seeding_time <- as.integer(seeding_time)
  }
  ## make sure we have at least (length of total gt pmf - 1) seeding time
  seeding_time <- max(
    seeding_time,
    sum(generation_time$max) + sum(generation_time$np_pmf_max) -
      length(generation_time$max) - length(generation_time$np_pmf_max)
  )
  return(seeding_time)
}
