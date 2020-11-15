#' Estimate Truncation of Observed Data
#'
#' @description \lifecycle{experimental}
#' @param obs A list of data frames each containing a date variable 
#' and a confirm (integer) variable. Each dataset should be a snapshot 
#' of the reported data over time.
#' @param max_truncation Integer, defaults to 10. Maximum number of 
#' days to include in the truncation distribution.
#' @param model A compiled stan model to override the default model. May be
#' useful for package developers or those developing extensions.
#' @param ... Additional parameters to pass to `rstan::sampling`.
#' @return
#' @export
#' @examples
#' library(EpiNow2)
#' #set number of cores to use
#' options(mc.cores = ifelse(interactive(), 4, 1))
#' # get example case counts
#' reported_cases <- EpiNow2::example_confirmed[1:60]
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
#' # compile dev model                      
#' model <- rstan::stan_model("inst/stan/estimate_truncation.stan")
#'
#' # fit model to example data
#' est <- estimate_truncation(example_data, model = model)
estimate_truncation <- function(obs, max_truncation = 10, model = NULL, 
                                CrIs = c(0.2, 0.5, 0.9), ...) {
  # combine into ordered matrix
  obs <- data.table::copy(obs)
  nrow_obs <- order(purrr::map_dbl(obs, nrow))
  obs <- obs[nrow_obs]
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
    t = nrow(obs),
    obs_sets = ncol(obs) - 1,
    trunc_max = array(max_truncation)
  )
  
  # initial conditions for stan model
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
                         ...)

  out <- list()
  # Summarise fit truncation distribution for downstream usage
  out$dist <- list(
    mean = round(summary(fit, pars = "logmean")$summary[1], 3),
    mean_sd = round(summary(fit, pars = "logmean")$summary[3], 3),
    sd = round(summary(fit, pars = "logsd")$summary[1], 3),
    sd_sd = round(summary(fit, pars = "logsd")$summary[3], 3),
    max = max_truncation
  )
  
  # summarise reconstructed obs
  CrIs <- c(0.5, 0.5 - CrIs / 2, 0.5 + CrIs / 2)
  CrIs <- CrIs[order(CrIs)]
  recon_obs <- summary(fit, pars = "recon_obs", probs = CrIs)$summary
  recon_obs <- data.table::as.data.table(recon_obs, 
                                         keep.rownames = "id")
  recon_obs <- recon_obs[, dataset := 1:.N][, 
                           dataset := dataset %% data$obs_sets][
                        dataset == 0, dataset := data$obs_sets]
  
  example_data
  out$data <- data
  out$fit <- fit
  return(out)
}


