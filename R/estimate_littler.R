#' Estimate r
#'
#' @param sample A datatable containing a numeric cases variable.
#' @param min_time Numeric, minimum time to use to fit the model.
#' @param max_time Numeric, maximum time to use to fit the model.
#'
#' @return A datatable containing an estimate of r, its standard deviation and a
#' measure of the goodness of fit.
#' @importFrom data.table data.table
#' @export
#' @examples
#'
#' cases <- data.table::setDT(EpiSoon::example_obs_cases)[, 
#'                           cases := as.integer(cases)]
#'
#' estimate_little_r(cases)
estimate_little_r <- function(sample, min_time = NULL, max_time = NULL) {
  ## Add time var
  sample <- data.table::copy(sample)[, time := 1:length(cases)]
  
  ## Add all data if time windows are not given
  if (is.null(min_time)) {
    min_time <- min(sample$time, na.rm = TRUE)
  }
  
  if (is.null(max_time)) {
    max_time <- max(sample$time, na.rm = TRUE)
  }
  
  ## Limit data based on time window supplied
  sample <- sample[time >= min_time][time <= max_time]
  
  ## Fit log model - adapted from the R0 package
  model <- glm(cases ~ time, family = quasipoisson(), data = sample)
  
  model_sum <- summary(model)
  
  ## Extract little r and summary measures
  result <- data.table::data.table(r = model_sum$coefficients[2, 1],
                                   sd = model_sum$coefficients[2, 2],
                                   fit_meas = (model$null.deviance - model$deviance) /
                                     (model$null.deviance))
  return(result)
}


#' Estimate the doubling time
#'
#' @param r An estimate of the rate of change (r)
#'
#' @return A vector of numeric values
#' @export
#'
#' @examples
#' 
#' estimate_doubling_time(0.2)
estimate_doubling_time <- function(r) {
  log(2) * 1 / r
}


#' Estimate r in a set time window
#'
#' @param onsets A list of samples datasets nested within the dataset sampled from.
#' @param min_time Numeric, the minimum time to fit the model to.
#' @param max_time Numeric, the maximum time to fit the model to.
#' @param bootstrap_samples Numeric, defaults to 1000. The number of samples to take when
#' bootstrapping little r to account for model uncertainty.
#'
#' @return A list of 3 dataframes containing estimates for little r, doubling time and
#' model goodness of fit.
#' @export
#' @importFrom purrr map map_dbl
#' @importFrom HDInterval hdi
#' @importFrom data.table setDTthreads rbindlist copy
estimate_r_in_window <- function(onsets = NULL,
                                 min_time = NULL,
                                 max_time = NULL,
                                 bootstrap_samples = 1000) {
  
  data.table::setDTthreads(1)
  
  r <- 
    purrr::map(
      onsets, 
      ~ EpiNow::estimate_little_r(.,
                                  min_time = min_time,
                                  max_time = max_time)[,
                                                       sampled_r := list(stats::rnorm(bootstrap_samples, r, sd))]
    )
  
  r <- data.table::rbindlist(r, idcol = "sample")
  
  ## Summarise r
  r <- r[, .(sampled_r = unlist(sampled_r)), 
         by = c("sample", "r", "sd", "fit_meas")]
  
  
  summarise_r <- data.table::copy(r)[,.(
    bottom  = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.9)), ~ .[[1]]),
    top = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.9)), ~ .[[2]]),
    lower  = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.5)), ~ .[[1]]),
    upper = purrr::map_dbl(list(HDInterval::hdi(sampled_r, credMass = 0.5)), ~ .[[2]]),
    mean = mean(sampled_r, na.rm = TRUE),
    median = median(sampled_r, na.rm = TRUE))
    ]
  
  
  ## Summarise doubling time
  cols <- colnames(summarise_r)
  summarise_doubling <- data.table::copy(summarise_r)[, 
                                                      (cols) := lapply(.SD,EpiNow::estimate_doubling_time),
                                                      .SDcols = cols] 
  
  ## Flip credible intervals
  summarise_doubling <- summarise_doubling[,
                                           .(bottom = top, top = bottom, lower = upper,
                                             upper = lower, mean, median)]
  
  ## Sumamrise goodness of fit
  summarise_fit <- data.table::copy(r)[,.(
    bottom = purrr::map_dbl(list(HDInterval::hdi(fit_meas, 
                                                 credMass = 0.9)), ~ .[[1]]),
    top = purrr::map_dbl(list(HDInterval::hdi(fit_meas, 
                                              credMass = 0.9)), ~ .[[2]]),
    lower = purrr::map_dbl(list(HDInterval::hdi(fit_meas, 
                                                credMass = 0.5)), ~ .[[1]]),
    upper = purrr::map_dbl(list(HDInterval::hdi(fit_meas, 
                                                credMass = 0.5)), ~ .[[2]]),
    mean = mean(fit_meas, na.rm = TRUE),
    median = median(fit_meas, na.rm = TRUE))
    ]
  
  out <- list(summarise_r, summarise_doubling, summarise_fit)
  
  names(out) <- c("little_r", "doubling_time", "goodness_of_fit")
  
  return(out)
}


#' Estimate time varying r
#'
#' @param window integer value for window size in days (default = 7)
#'
#' @inheritParams estimate_r_in_window
#' @return A dataframe of r estimates over time summarisd across samples.
#' @export
#' @importFrom purrr safely map_lgl
#' @importFrom future.apply future_lapply
estimate_time_varying_r <- function(onsets, window = 7) {
  
  safe_estimate_r_window <- purrr::safely(
    EpiNow::estimate_r_in_window
  )
  
  ## Set up results data.table
  windowed_r <- data.table::data.table(
    max_time = window:nrow(onsets[[1]]),
    date = onsets[[1]]$date[window:nrow(onsets[[1]])]
  )
  
  ## Add minimium window
  windowed_r <- windowed_r[, min_time := max_time - window]
  
  ## Estimate little r
  windowed_r <- windowed_r[, 
                           .(date, min_time, max_time,
                             estimates = future.apply::future_lapply(1:length(min_time),
                                                                     function(index){suppressMessages(safe_estimate_r_window(onsets, 
                                                                                                                             min_time = min_time[index],
                                                                                                                             max_time = max_time[index])[[1]])},
                                                                     future.scheduling = 10, future.packages = c("EpiNow", "purrr")))][,
                                                                           var := list(names(estimates[[1]]))]
  
  ## Remove first nesting layer
  windowed_r <- windowed_r[!purrr::map_lgl(estimates, is.null)][,
                           .(estimates = purrr::flatten(estimates), var = unlist(var)),
                           by = c("date", "min_time", "max_time")]
  
  windowed_r <- windowed_r[, .(windowed_r[, .(date, var)],
                               data.table::rbindlist(estimates))]
  
  
  return(windowed_r)
}


