#' Estimate Truncation of Observed Data
#'
#' @description \lifecycle{experimental}
#' Estimates a truncation distribution from multiple snapshots of the same 
#' data source over time. This distribution can then be used in `regional_epinow`,
#' `epinow`, and `estimate_infections` to adjust for truncated data. 
#' The model of truncation is as follows:
#' 
#' 1. The truncation distribution is assumed to be log normal with a mean and 
#' standard deviation that is informed by the data.
#' 2. The data set with the latest observations is adjusted for truncation using 
#' the truncation distribution.
#' 3. Earlier data sets are recreated by applying the truncation distribution to
#' the adjusted latest observations in the time period of the earlier data set. These 
#' data sets are then compared to the earlier observations assuming a negative binomial
#' observation model.
#' 
#' This model is then fit using `stan` with standard normal, or half normal,
#' prior for the mean, standard deviation and 1 over the square root of the over dispersion.
#' 
#' This approach assumes that: 
#'  - Current truncation is related to past truncation.
#'  - Truncation is a multiplicative scaling of underlying reported cases.
#'  - Truncation is log normally distributed. 
#' @param obs A list of data frames each containing a date variable 
#' and a confirm (integer) variable. Each data set should be a snapshot 
#' of the reported data over time. All data sets must contain a complete vector 
#' of dates. 
#' @param max_truncation Integer, defaults to 10. Maximum number of 
#' days to include in the truncation distribution.
#' @param model A compiled stan model to override the default model. May be
#' useful for package developers or those developing extensions.
#' @param verbose Logical, should model fitting progress be returned.
#' @param ... Additional parameters to pass to `rstan::sampling`.
#' @return A list containing: the summary parameters of the truncation distribution
#'  (`dist`), a data frame containing the observed truncated data, latest observed data
#'  and the adjusted for truncation observations (`obs`), the data used for fitting 
#'  (`data`) and the fit object (`fit`).
#' @export
#' @inheritParams calc_CrIs
#' @importFrom purrr map reduce map_dbl
#' @importFrom rstan sampling
#' @importFrom data.table copy .N as.data.table merge.data.table
#' @examples
#' #set number of cores to use
#' options(mc.cores = ifelse(interactive(), 4, 1))
#' # get example case counts
#' reported_cases <- example_confirmed[1:60]
#' 
#' # define example truncation distribution (note not integer adjusted)
#' trunc_dist <- list(mean = convert_to_logmean(3, 2),
#'                    mean_sd = 0.1,
#'                    sd = convert_to_logsd(3, 2),
#'                    sd_sd = 0.1,
#'                    max = 10)
#'
#' # apply truncation to example data
#' construct_truncation <- function(index, cases, dist) {
#' set.seed(index)
#'   cmf <- cumsum(
#'      dlnorm(1:(dist$max + 1), 
#'             rnorm(1, dist$mean, dist$mean_sd),
#'             rnorm(1, dist$sd, dist$sd_sd)))
#'   cmf <- cmf / cmf[dist$max + 1]
#'   cmf <- rev(cmf)[-1]
#'   trunc_cases <- data.table::copy(cases)[1:(.N - index)]
#'   trunc_cases[(.N - length(cmf) + 1):.N, confirm := as.integer(confirm * cmf)]
#'   return(trunc_cases)
#'  }
#' example_data <- purrr::map(10:0, 
#'                            construct_truncation,
#'                            cases = reported_cases,
#'                            dist = trunc_dist)
#'
#' # fit model to example data
#' est <- estimate_truncation(example_data, verbose = interactive())
#'                            
#' # summary of the distribution
#' est$dist
#' # observations linked to truncation adjusted estimates
#' est$obs      
estimate_truncation <- function(obs, max_truncation = 10, 
                                model = NULL, 
                                CrIs = c(0.2, 0.5, 0.9),
                                verbose = TRUE,
                                ...) {
  # combine into ordered matrix
  dirty_obs <- data.table::copy(obs)
  nrow_obs <- order(purrr::map_dbl(dirty_obs, nrow))
  dirty_obs <- dirty_obs[nrow_obs]
  obs <- data.table::copy(dirty_obs)
  obs <- purrr::map(1:length(obs), ~ obs[[.]][, (as.character(.)) := confirm][, 
                                                  confirm := NULL])
  obs <- purrr::reduce(obs, merge, all = TRUE)
  obs_start <- nrow(obs) - max_truncation - sum(is.na(obs$`1`)) + 1
  obs_dist <- purrr::map_dbl(2:(ncol(obs)), ~ sum(is.na(obs[[.]])))
  obs_data <- obs[, -1][, purrr::map(.SD, ~ ifelse(is.na(.), 0, .))]
  obs_data <- obs_data[obs_start:.N]
  
  # convert to stan list
  data <- list(
    obs = obs_data,
    obs_dist = obs_dist,
    t = nrow(obs_data),
    obs_sets = ncol(obs_data),
    trunc_max = array(max_truncation)
  )
  
  # initial conditions
  init_fn <- function() {
    data <- list(
      logmean = array(rnorm(1, 0, 1)),
      logsd = array(abs(rnorm(1, 0, 1)))
    )
    return(data)
  }
  
  # fit
  if (is.null(model)) {
    model <- stanmodels$estimate_truncation
  }
  fit <- rstan::sampling(model, 
                         data = data, 
                         init = init_fn,
                         refresh = ifelse(verbose, 50, 0),
                         ...)

  out <- list()
  # Summarise fit truncation distribution for downstream usage
  out$dist <- list(
    mean = round(rstan::summary(fit, pars = "logmean")$summary[1], 3),
    mean_sd = round(rstan::summary(fit, pars = "logmean")$summary[3], 3),
    sd = round(rstan::summary(fit, pars = "logsd")$summary[1], 3),
    sd_sd = round(rstan::summary(fit, pars = "logsd")$summary[3], 3),
    max = max_truncation
  )
  
  # summarise reconstructed observations
  CrIs <- c(0.5, 0.5 - CrIs / 2, 0.5 + CrIs / 2)
  CrIs <- CrIs[order(CrIs)]
  recon_obs <- rstan::summary(fit, pars = "recon_obs", probs = CrIs)$summary
  recon_obs <- data.table::as.data.table(recon_obs, 
                                         keep.rownames = "id")
  recon_obs <- recon_obs[, dataset := 1:.N][, 
                           dataset := dataset %% data$obs_sets][
                           dataset == 0, dataset := data$obs_sets]
  # link reconstructed observations to observed
  last_obs <- 
    data.table::copy(dirty_obs[[length(dirty_obs)]])[, last_confirm := confirm][, 
                                     confirm := NULL]
  link_obs <- function(index) {
    target_obs <- dirty_obs[[index]][, index := .N - 0:(.N-1)]
    target_obs <- target_obs[index < max_truncation]
    estimates <- recon_obs[dataset == index][, c("id", "dataset") := NULL]
    estimates <- estimates[, lapply(.SD, as.integer)]
    estimates <- estimates[, index := .N - 0:(.N-1)]
    estimates[, c("n_eff", "Rhat") := NULL]
    target_obs <- data.table::merge.data.table(target_obs, estimates,
                                               by = "index", all.x = TRUE)
    target_obs <- target_obs[order(date)][, index := NULL]
    target_obs <- 
      data.table::merge.data.table(
        target_obs, last_obs, by = "date")
    target_obs[,report_date := max(date)]
    return(target_obs)
  }
  out$obs <- purrr::map(1:(data$obs_sets), link_obs)
  out$obs <- data.table::rbindlist(out$obs)
  out$data <- data
  out$fit <- fit

  class(out) <- c("estimate_truncation", class(out))
  return(out)
}
