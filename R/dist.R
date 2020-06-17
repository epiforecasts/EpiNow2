#' Distribution Skeleton
#'
#' @description This function acts as a skeleton for a truncated distribution defined by 
#' model type, maximum value and model parameters. It is designed to be used with the
#' output from `get_dist`.
#' @param n Numeric vector, number of samples to take (or days for the probability density).
#' @param dist Logical, defaults to `FALSE`. Should the probability density be returned rather
#' than a number of samples. 
#' @param cum Logical, defaults to `TRUE`. If `dist = TRUE` should the returned distribution be 
#' cumulative.
#' @param model Character string, defining the model to be used. Supported options are exponential 
#' ("exp"), gamma ("gamma"), and log normal ("lognorm")
#' @param params A list of parameters values (by name) required for each model. For the exponential model
#' this is a rate parameter and for the gamma model this is alpha and beta.
#' @param max_value Numeric, the maximum value to allow. Defaults to 120. Samples outside 
#' of this range are resampled.
#'
#' @return A vector of samples or a probability distribution.
#' @export
#'
#' @examples
#' 
#' ## Exponential model
#' 
#' ## Sample
#' dist_skel(10, model = "exp", params = list(rate = 1))
#' 
#' ## Cumulative prob density
#' dist_skel(1:10, model = "exp", dist = TRUE, params = list(rate = 1))
#' 
#' ## Probability density
#' dist_skel(1:10, model = "exp", dist = TRUE, 
#'           cum = FALSE, params = list(rate = 1))
#' 
#' ## Gamma model
#' 
#' dist_skel(10, model = "gamma", params = list(alpha = 1, beta = 2))
#' 
#' ## Cumulative prob density
#' dist_skel(0:10, model = "gamma", dist = TRUE,
#'           params = list(alpha = 1, beta = 2))
#' 
#' ## Probability density
#' dist_skel(0:10, model = "gamma", dist = TRUE, 
#'           cum = FALSE, params = list(alpha = 2, beta = 2))
#' 
#' ## Log normal model
#' 
#' dist_skel(10, model = "lognorm", params = list(mean = log(5), sd = log(2)))
#' 
#' ## Cumulative prob density
#' dist_skel(0:10, model = "lognorm", dist = TRUE,
#'           params = list(mean = log(5), sd = log(2)))
#' 
#' ## Probability density
#' dist_skel(0:10, model = "lognorm", dist = TRUE, cum = FALSE,
#'           params = list(mean = log(5), sd = log(2)))
#'         

dist_skel <- function(n, dist = FALSE, cum = TRUE, model,
                      params, max_value = 120) {
  
  if (model %in% "exp") {
    ## Define support functions for exponential dist
    rdist <- function(n) {rexp(n, params$rate)}
    pdist <- function(n) {pexp(n, params$rate) / pexp(max_value, params$rate)}
    ddist <- function(n) {(pexp(n + 1, params$rate) -
        pexp(n, params$rate)) / 
        pexp(max_value, params$rate)}
  }else if (model %in% "gamma") {
    rdist <- function(n) {rgamma(n, params$alpha, params$beta)}
    pdist <- function(n) {pgamma(n, params$alpha, params$beta)  /
        pgamma(max_value, params$alpha, params$beta)}
    ddist <- function(n) {
      (pgamma(n + 1, params$alpha, params$beta) -
        pgamma(n, params$alpha, params$beta)) / 
        pgamma(max_value, params$alpha, params$beta)}
  }else if (model %in% "lognorm") {
    rdist <- function(n) {rlnorm(n, params$mean, params$sd)}
    pdist <- function(n) {plnorm(n, params$mean, params$sd) / 
        plnorm(max_value, params$mean, params$sd)}
    ddist <- function(n) {
      (plnorm(n + 1, params$mean, params$sd) -
        plnorm(n, params$mean, params$sd)) / 
        plnorm(max_value, params$mean, params$sd)}
  }
  
  ## Define internal sampling function
  inner_skel <- function(n, dist = FALSE, cum = TRUE, max_value = NULL){
    if(!dist) {
      rdist(n)
    }else{
      if (length(n) > max_value) {
        n <- 1:max_value
      }
      if (cum) {
        pdist(n)
      }else{
        ddist(n)
      }
    }
  }
  
  ## Define truncation wrapper
  truncated_skel <- function(n, dist, cum, max_value) {
    n <- inner_skel(n, dist, cum, max_value)
    
    if (!dist) {
      while(any(!is.na(n) & n >= max_value)) {
        n <- ifelse(n >= max_value, inner_skel(n), n)
      }
      
      n <- as.integer(n)
    }
    
    return(n)
  }
  
  ## Call function
  sample <- truncated_skel(n, dist = dist, cum = cum, max_value = max_value)
  
  return(sample)
}




#' Fit an integer adjusted exponential or gamma distribution
#'
#'
#' @param values Numeric vector of values
#' @param samples Numeric, number of samples to take
#' @param dist Character string, which distribution to fit. Defaults to exponential (`"exp"`) but
#' gamma is also supported (`"gamma"`).
#' @return A `stan` fit of an interval censored distribution
#' @export
#' @import Rcpp
#' @import methods
#' @importFrom rstan sampling extract
#' @useDynLib EpiNow, .registration=TRUE
#' @examples
#' 
#' dist_fit(rexp(1:100, 2), samples = 1000, dist = "exp")
#' 
dist_fit <- function(values = NULL, samples = NULL, dist = "exp") {
  
  if (is.null(samples)) {
    samples <- 1000
  }
  
  if (samples < 1000) {
    samples <- 1000
  }
  
  ## Model parameters
  lows <- values - 1
  lows <- ifelse(lows <=0, 1e-6, lows)
  ups <- values + 1
  
  data <- list(N = length(values),
               low = lows,
               up = ups,
               iter = samples + 1000,
               warmup = 1000)
  
  if (dist %in% "exp") {
    model <- stanmodels$exp_fit
    data <- c(data, lam_mean = mean(values))
    
  }else if (dist %in% "gamma") {
    model <- stanmodels$gamma_fit
  }
  
  ## Set adapt delta based on the sample size
  if (length(values) <= 30) {
    adapt_delta <- 0.999
  } else {
    adapt_delta <- 0.9
  }
  
  ## Fit model
  fit <- rstan::sampling(
    model,
    data = data,
    control = list(adapt_delta = adapt_delta),
    chains = 2,
    cores = 1,
    refresh = 0)
  
  
  return(fit)
}


#' Generate a Gamma Distribution Definition Based on Parameter Estimates
#'
#' @description Generates a distribution definition when only parameter estimates 
#' are available for gamma distributed parameters. See `rgamma` for distribution information.
#' @param shape Numeric, shape parameter of the gamma distribution.
#' @param shape_sd Numeric, standard deviation of the shape parameter.
#' @param scale Numeric, scale parameter of the gamma distribution.
#' @param scale_sd  Numeric, standard deviation of the scale parameter.
#' @param samples Numeric, number of sample distributions to generate.
#' @importFrom truncnorm rtruncnorm
#' @return A data.table definining the distribution as used by `dist_skel`
#' @export
#' @inheritParams dist_skel
#' @examples
#' 
#' ## Using estimated shape and scale
#' def <- gamma_dist_def(shape = 5.807, shape_sd = 0.2,
#'                scale = 0.9, scale_sd = 0.05,
#'                max_value = 20, samples = 10)
#'                
#'print(def)
#'
#'def$params[[1]]
#'
#'## Using mean and sd
#'def <- gamma_dist_def(mean = 3, mean_sd = 0.5,
#'                sd = 3, sd_sd = 0.1,
#'                max_value = 20, samples = 10)
#'                
#'print(def)
#'
#'def$params[[1]]
gamma_dist_def <- function(shape, shape_sd,
                           scale, scale_sd, 
                           mean, mean_sd,
                           sd, sd_sd,
                           max_value, samples) {
  
  if (missing(shape) & missing(scale) & !missing(mean) & !missing(sd)) {
    mean <- truncnorm::rtruncnorm(samples, a = 0, mean = mean, sd = mean_sd)
    sd <- truncnorm::rtruncnorm(samples, a = 0, mean = sd, sd = sd_sd)
    beta <- sd^2/mean
    alpha <- mean/beta
    beta <- 1 / beta
  }else{
    alpha <- truncnorm::rtruncnorm(samples, a = 0, mean = shape, sd = shape_sd)
    beta <- 1 / truncnorm::rtruncnorm(samples, a = 0, mean = scale, sd = scale_sd)
  }
  
  dist <- data.table::data.table(
    model = rep("gamma", samples),
    params = purrr::transpose(
      list(alpha = alpha,
           beta = beta)),
    max_value = rep(max_value, samples)
  )
  
  return(dist)
}

#' Generate a Log Normal Distribution Definition Based on Parameter Estimates
#'
#' @description Generates a distribution definition when only parameter estimates 
#' are available for log normal distributed parameters. See `rlnorm` for distribution information.
#' @param mean Numeric, log mean parameter of the gamma distribution.
#' @param mean_sd Numeric, standard deviation of the log mean parameter.
#' @param sd Numeric, log sd parameter of the gamma distribution.
#' @param sd_sd  Numeric, standard deviation of the log sd parameter.
#' @param samples Numeric, number of sample distributions to generate.
#' @param to_log Logical, should parameters be logged before use.
#'
#' @return A data.table definining the distribution as used by `dist_skel`
#' @importFrom truncnorm rtruncnorm
#' @export
#' @inheritParams dist_skel
#' @examples
#' 
#' 
#' def <- lognorm_dist_def(mean = 1.621, mean_sd = 0.0640,
#'                         sd = 0.418, sd_sd = 0.0691,
#'                         max_value = 20, samples = 10)
#'                
#'print(def)
#'
#'def$params[[1]]
#'
#'def <- lognorm_dist_def(mean = 5, mean_sd = 1,
#'                         sd = 3, sd_sd = 1,
#'                         max_value = 20, samples = 10,
#'                         to_log = TRUE)
#'                
#'print(def)
#'
#'def$params[[1]]
lognorm_dist_def <- function(mean, mean_sd,
                             sd, sd_sd, 
                             max_value, samples,
                             to_log = FALSE) {
  
  if (to_log) {
    mean <- log(mean)
    mean_sd <- log(mean_sd)
    sd <- log(sd)
    sd_sd <- log(sd_sd)
  }
  dist <- data.table::data.table(
    model = rep("lognorm", samples),
    params = purrr::transpose(
      list(mean = truncnorm::rtruncnorm(samples, a = 0, mean = mean, sd = mean_sd),
           sd = truncnorm::rtruncnorm(samples, a = 0, mean = sd, sd = sd_sd))),
    max_value = rep(max_value, samples)
  )
  
  return(dist)
}


  
#' Get a Parameters that Define a Discrete Distribution
#'
#' @param values Numeric vector of integer values.
#' @param verbose Logical, defaults to `FALSE`. Should progress messages be printed
#' @param samples Numeric, number of samples to take overall from the bootstrapped posteriors.
#' @param bootstraps Numeric, defaults to 1. The number of bootstrap samples (with replacement)
#'  of the delay distribution to take.
#' @param bootstrap_samples Numeric, defaults to 100. The number of samples to take in each boostrap.
#' When the sample size of the supplied delay distribution is less than 100 this is used instead.
#' @return A data.table of distributions and the parameters that define them.
#' @importFrom purrr map map2 flatten
#' @importFrom future.apply future_lapply
#' @importFrom rstan extract
#' @importFrom data.table data.table rbindlist setDTthreads
#' @importFrom loo loo relative_eff extract_log_lik
#' @author Sebastian Funk <sebastian.funk@lshtm.ac.uk>
#'
#' @export
#'
#' @examples
#'
#' ## Example with exponential and a small smaple
#' delays <- rexp(20, 1)
#'
#' get_dist_def(delays, samples = 10, verbose = TRUE)
#'
#'
#' \dontrun{
#' ## Example with gamma and a larger sample
#' delays <- rgamma(50, 4, 1)
#'
#' out <- get_dist_def(delays, samples = 2, bootstraps = 2)
#'
#' ## Inspect
#' out
#' 
#' ## Inspect one parameter
#' out$params[[1]]
#' 
#' 
#' ## Load into skeleton and sample with truncation
#' EpiNow::dist_skel(10, model = out$model[[1]],
#'                   params = out$params[[1]],
#'                   max_value = out$max_value[[1]])
#'}
get_dist_def <- function(values, verbose = FALSE, samples = 1,
                         bootstraps = 1, bootstrap_samples = 250) {
  
  if (samples < bootstraps) {
    samples <- bootstraps
  }
  ## Make values integer if not
  values <- as.integer(values)
  ## Remove NA values
  values <- values[!is.na(values)]
  ## Filter out negative values
  values <- values[values >= 0]
  
  get_single_dist <- function(values = NULL, samples = 1) {
    
    data.table::setDTthreads(1)
    
    ## Structure
    out <- data.table::data.table(
      max_value = max(values) + 1
    )
    
    # Fit gamma and exponential models
    fit_exp <- EpiNow::dist_fit(values, samples = samples, dist = "exp")
    
    # If there is enough data, try fitting a gamma  
    if(length(values) >= 30) {
      
      fit_gam <- EpiNow::dist_fit(values, samples = samples, dist = "gamma")
      
      # Extract log likelihoods
      log_lik_exp <- loo::extract_log_lik(fit_exp, merge_chains = FALSE)
      log_lik_gam <- loo::extract_log_lik(fit_gam, merge_chains = FALSE)
      # Calculate relative efficiencies
      rel_exp <- loo::relative_eff(exp(log_lik_exp))
      rel_gam <- loo::relative_eff(exp(log_lik_gam))
      # Estimate looic
      loo_exp <- loo::loo(log_lik_exp, r_eff = rel_exp)
      loo_gam <- loo::loo(log_lik_gam, r_eff = rel_gam)
      # Choose best model
      best_model <- ifelse(loo_exp$estimates[3,1] < loo_gam$estimates[3,1], "exp", "gamma")
      
    }else{
      best_model <- "exp"
    }
    
    ## Add model definition to output
    out <- out[, model := best_model]
    
    if(best_model == "exp"){
      if (verbose) {
        message("Exponential selected as the best fit for the distribution")
      }
      rate <- sample(rstan::extract(fit_exp)$lambda, samples)
      
      ## Add parameters to output
      out <- out[, params := list(purrr::map(rate, ~ list(rate = .)))]
      
    }else if(best_model == "gamma"){
      if (verbose) {
        message("Gamma selected as the best fit for the distribution")
      }
      
      alpha <- sample(rstan::extract(fit_gam)$alpha, samples)
      beta <- sample(rstan::extract(fit_gam)$beta, samples)
      
      out <- out[, params := list(purrr::map2(alpha, beta,
                                              ~ list(alpha = .x, beta = .y)))]
    } 
    
    ## Unnest parameter lists
    if (samples != 1) {
      out <- out[, .(params = purrr::flatten(params)),
                 by = c("model", "max_value")]
    }
    
    return(out)
  }
  
  
  if (bootstraps == 1) {
    dist_defs <- get_single_dist(values, samples = samples)
  }else{
    ## Fit each sub sample
    dist_defs <- future.apply::future_lapply(1:bootstraps,
                                             function(boot){get_single_dist(sample(values, 
                                                                                   min(length(values), bootstrap_samples),
                                                                                   replace = TRUE),
                                                                            samples = ceiling(samples / bootstraps))},
                                             future.scheduling = 10,
                                             future.globals = c("values", "bootstraps", "samples",
                                                                "bootstrap_samples", "get_single_dist"),
                                             future.packages = "data.table")
    
    ## Bind distributions together               
    dist_defs <- data.table::rbindlist(dist_defs)
    
    ## Resample without replacement to force the correct number of samples.
    indexes <- sample(1:nrow(dist_defs), samples, replace = FALSE)
    
    dist_defs <- dist_defs[indexes]
  }
  
  return(dist_defs)
}


#' Approximate Sampling a Distribution using Counts
#'
#' @param cases A dataframe of cases (in date order) with the following variables:
#' `date` and `cases`. 
#' @param max_value Numeric, maximum value to allow. Defaults to 120 days
#' @param direction Character string, defato "backwards". Direction in which to map cases. Supports
#' either "backwards" or "forwards".
#' @param dist_fn Function that takes two arguments with the first being numeric and the second being logical (and 
#' defined as `dist`). Should return the probability density or a sample from the defined distribution. See
#' the examples for more.
#' @param earliest_allowed_mapped A character string representing a date ("2020-01-01"). Indicates 
#' the earlies allowed mapped value.
#' @param type Character string indicating the method to use to transfrom counts. Supports either "sample"
#' which approximates sampling or "median" would shift by the median of the distribution.
#' @param truncate_future Logical, should cases be truncted if they occur after the first date reported in the data. 
#' Defaults to `TRUE`.
#' @return A `data.table` of cases by date of onset
#' @export
#' @importFrom purrr map_dfc
#' @importFrom data.table data.table setorder
#' @importFrom lubridate days
#' @examples
#' 
#' cases <- data.table::as.data.table(EpiSoon::example_obs_cases) 
#' 
#' cases <- cases[, cases := as.integer(cases)] 
#' 
#' ## Reported case distribution
#' print(cases)
#' 
#' ## Total cases
#' sum(cases$cases)
#' 
#' delay_fn <- function(n, dist, cum) {
#'               if(dist) {
#'                 pgamma(n + 0.9999, 2, 1) - pgamma(n - 1e-5, 2, 1)
#'                }else{
#'                 as.integer(rgamma(n, 2, 1))
#'                }
#'              }
#' 
#' onsets <- sample_approx_dist(cases = cases,
#'                              dist_fn = delay_fn)
#'    
#' ## Estimated onset distribution
#' print(onsets)
#'   
#' ## Check that sum is equal to reported cases
#' total_onsets <- median(
#'    purrr::map_dbl(1:1000, 
#'                   ~ sum(sample_approx_dist(cases = cases,
#'                   dist_fn = delay_fn)$cases))) 
#'                    
#' total_onsets
#'  
#'                    
#' ## Map from onset cases to reported                  
#' reports <- sample_approx_dist(cases = cases,
#'                               dist_fn = delay_fn,
#'                               direction = "forwards")
#'                               
#'                               
#' ## Map from onset cases to reported using a mean shift               
#' reports <- sample_approx_dist(cases = cases,
#'                               dist_fn = delay_fn,
#'                               direction = "forwards",
#'                               type = "median")
#' 
sample_approx_dist <- function(cases = NULL, 
                               dist_fn = NULL,
                               max_value = 120, 
                               earliest_allowed_mapped = NULL,
                               direction = "backwards",
                               type = "sample",
                               truncate_future = TRUE) {
  
  if (type %in% "sample") {
    
    if (direction %in% "backwards") {
      direction_fn <- rev
    }else if (direction %in% "forwards") {
      direction_fn <- function(x){x}
    }
    ## Reverse cases so starts with current first
    reversed_cases <- direction_fn(cases$cases)
    reversed_cases[is.na(reversed_cases)] <- 0 
    ## Draw from the density fn of the dist
    draw <- dist_fn(0:max_value, dist = TRUE, cum = FALSE)
    
    ## Approximate cases
    mapped_cases <- suppressMessages(purrr::map_dfc(1:length(reversed_cases), 
                                   ~ c(rep(0, . - 1), 
                                       stats::rbinom(length(draw),
                                                     rep(reversed_cases[.], length(draw)),
                                                     draw),
                                       rep(0, length(reversed_cases) - .))))
    
    
    ## Set dates order based on direction mapping
    if (direction %in% "backwards") {
      dates <- seq(min(cases$date) - lubridate::days(length(draw) - 1),
                   max(cases$date), by = "days")
    }else if (direction %in% "forwards") {
      dates <- seq(min(cases$date),
                   max(cases$date)  + lubridate::days(length(draw) - 1),
                   by = "days")
    }
    
    ## Summarises movements and sample for placement of non-integer cases
    case_sum <- direction_fn(rowSums(mapped_cases))
    floor_case_sum <- floor(case_sum)
    sample_cases <- floor_case_sum + 
      data.table::fifelse((runif(1:length(case_sum)) < (case_sum - floor_case_sum)),
                          1, 0)
    
    ## Summarise imputed onsets and build output data.table
    mapped_cases <- data.table::data.table(
      date = dates,
      cases = sample_cases
    )
    
    ## Filter out all zero cases until first recorded case
    mapped_cases <- data.table::setorder(mapped_cases, date)
    mapped_cases <- mapped_cases[,cum_cases := cumsum(cases)][cum_cases != 0][,cum_cases := NULL]
    
  }else if (type %in% "median") {
    shift <- as.integer(median(as.integer(dist_fn(1000, dist = FALSE)), na.rm = TRUE))
    
    if (direction %in% "backwards") {
      mapped_cases <- data.table::copy(cases)[, date := date - lubridate::days(shift)]
    }else if (direction %in% "forwards") {
      mapped_cases <- data.table::copy(cases)[, date := date + lubridate::days(shift)]
    }
  }

  
  if (!is.null(earliest_allowed_mapped)) {
    mapped_cases <- mapped_cases[date >= as.Date(earliest_allowed_mapped)]
  }
  
  ## Filter out future cases
  if (direction %in% "forwards" & truncate_future) {
    max_date <- max(cases$date)
    mapped_cases <- mapped_cases[date <= max_date]
  }
  
  
  return(mapped_cases)
}
