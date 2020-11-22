#' Estimate a Secondary Observation from a Primary Observation
#'
#' @description `r lifecycle::badge("experimental")`
#' Estimates the relationship between a primary and secondary observation, for 
#' example hospital admissions and deaths or hospital admissions and bed 
#' occupancy. 
#' @param reports A data frame containing the `date` of report and both `primary` 
#' and `secondary` reports.
#' @param model A compiled stan model to override the default model. May be
#' useful for package developers or those developing extensions.
#' @param verbose Logical, should model fitting progress be returned.
#' @param ... Additional parameters to pass to `rstan::sampling`.
#' @return 
#' @export
#' @inheritParams estimate_infections
#' @inheritParams calc_CrIs
#' @importFrom rstan sampling
#' @importFrom lubridate wday
#' @importFrom data.table as.data.table
#' @examples
#' #set number of cores to use
#' options(mc.cores = ifelse(interactive(), 4, 1))
#' # load data.table for manipulation
#' library(data.table)
#' # make some example data
#' cases <- example_confirmed
#' cases <- as.data.table(cases)
#' cases <- cases[, .(date, primary = confirm, secondary = shift(confirm, n = 5, type = "lag"))]
#' cases <- cases[, secondary := frollmean(secondary, 3, align = "center")]
#' cases <- cases[!is.na(secondary)][, secondary := as.integer(secondary)]
#' 
#' # dev model compile
#' model <- rstan::stan_model("inst/stan/estimate_secondary.stan")
#'
#' # fit model to example data
#' est <- estimate_secondary(cases, verbose = interactive(), model = model,
#'                           obs = obs_opts(week_effect = FALSE, family = "poisson"),
#'                           chains = 2)
estimate_secondary <- function(reports, 
                               delays = delay_opts(
                                  list(mean = 2.5, mean_sd = 1, 
                                       sd = 0.47, sd_sd = 1, max = 30)),
                                truncation = trunc_opts(),
                                obs = obs_opts(),
                                CrIs = c(0.2, 0.5, 0.9),
                                model = NULL, 
                                verbose = TRUE,
                                ...) { 
  reports <- data.table::as.data.table(reports)
  # observation and control data
  data <- list(
    t = nrow(reports), 
    obs = reports$secondary,
    primary = reports$primary,
    day_of_week = lubridate::wday(reports$date, week_start = 1),
    cumulative = 0,               
    historic = 1,               
    primary_hist_additive = 1,   
    current = 0,               
    primary_current_additive = 0
  )
  # delay data
  data <- c(data, delays)
  data$seeding_time <- 0
  # truncation data
  data <- c(data, truncation)
  # observation model data
  data <- c(data, create_obs_model(obs))
  
  # fit
  if (is.null(model)) {
    model <- stanmodels$estimate_truncation
  }
  fit <- rstan::sampling(model, 
                         data = data, 
                         init = "random",
                         refresh = ifelse(verbose, 50, 0),
                         ...)
  
  out <- list()
  out$predictions <- extract_rstan(fit, "secondary", CrIs = CrIs)
  out$predictions <- out$predictions[, lapply(.SD, round, 1)]
  out$predictions <- cbind(reports, out$predictions)
  out$data <- data
  out$fit <- fit
  class(out) <- c("estimate_secondary", class(out))
  return(out)
}


