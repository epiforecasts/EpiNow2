#' Extract Samples for a Parameter from a Stan model
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts a single from a list of `stan` output and returns it as a `data.table`.
#' @param param Character string indicating the parameter to extract
#' @param samples Extracted stan model (using `rstan::extract`)
#' @param dates A vector identifying the dimensionality of the parameter to extract. Generally this will be 
#' a date
#' @importFrom data.table melt as.data.table
#' @return A data frame containing the parameter name, date, sample id and sample value
extract_parameter <- function(param, samples, dates) {
  param_df <- data.table::as.data.table(
    t(
      data.table::as.data.table(
        samples[[param]]
      )
    ))
  param_df <- param_df[, time := 1:.N]
  param_df <- data.table::melt(param_df, id.vars = "time",
                               variable.name = "var")
  
  param_df <- param_df[, var := NULL][, sample := 1:.N, by = .(time)]
  param_df <- param_df[, date := dates, by = .(sample)]
  param_df <- param_df[, .(parameter = param, time, date, 
                           sample, value)]
  return(param_df)
}


#' Extract Samples from a Parameter with a Single Dimension
#'
#' @inheritParams extract_parameter
#' @return A data frame containing the parameter name, sample id and sample value
extract_static_parameter <- function(param, samples) {
  data.table::data.table(
    parameter = param,
    sample = 1:length(samples[[param]]),
    value = samples[[param]])
}


#' Extract Parameter Samples from a Stan Model
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts a custom set of parameters from a stan object and adds stratification and 
#' dates where appropriate.
#' @param stan_fit A fit Stan model as returned by `rstan:sampling`
#' @param data A list of the data supplied to the `rstan::sampling` call.
#' @param reported_dates A vector of dates to report estimates for.
#' @param reported_inf_dates A vector of dates to report infection estimates for.
#' @param drop_length_1 Logical; whether the first dimension should be dropped if it
#' is if length 1; this is necessary when processing simulation results
#' @param merge if TRUE, merge samples and data so that parameters can be
#' extracted from data
#' @importFrom rstan extract
#' @importFrom data.table data.table
#' @return A list of dataframes each containing the posterior of a parameter
extract_parameter_samples <- function(stan_fit, data, reported_dates, reported_inf_dates, 
                                      drop_length_1 = FALSE, merge = FALSE) {
  
  # extract sample from stan object
  samples <- rstan::extract(stan_fit)

  ## drop initial length 1 dimensions if requested
  if (drop_length_1) {
    samples <- lapply(samples, function(x) {
      if (length(dim(x)) > 1 && dim(x)[1] == 1) dim(x) <- dim(x)[-1]
      return(x)
    })
  }
  
  for (data_name in names(data)) {
    if (!(data_name %in% names(samples))) {
      samples[[data_name]] <- data[[data_name]]
    }
  }
  
  # construct reporting list
  out <- list()
  # report infections, and R
  out$infections <- extract_parameter("infections", 
                                      samples,
                                      reported_inf_dates)
  
  out$reported_cases <- extract_parameter("imputed_reports", 
                                          samples, 
                                          reported_dates)
  if (data$estimate_r == 1) {
    out$R <- extract_parameter("R", 
                               samples,
                               reported_dates)
    if (data$bp_n > 0) {
      out$breakpoints <- extract_parameter("bp_effects", 
                                           samples, 
                                           1:data$bp_n)
      out$breakpoints <- out$breakpoints[, strat := date][, c("time", "date") := NULL]
    }
  }else{
    out$R <- extract_parameter("gen_R", 
                               samples,
                               reported_dates)
  }
  out$growth_rate <- extract_parameter("r", 
                                       samples,
                                       reported_dates)
  if (data$week_effect  == 1) {
    out$day_of_week <- extract_parameter("day_of_week_simplex", 
                                         samples,
                                         1:7)
    
    char_day_of_week <- data.table::data.table(wday = c("Monday", "Tuesday", "Wednesday",
                                                        "Thursday", "Friday", "Saturday",
                                                        "Sunday"),
                                               time = 1:7)
    out$day_of_week <- out$day_of_week[char_day_of_week, on = "time"][, 
                                       strat := as.character(wday)][,
                                      `:=`(time = NULL, date = NULL, wday = NULL)][,
                                       value := value * 7]
  }
  if (data$delays > 0) {
    out$delay_mean <- extract_parameter("delay_mean", samples, 1:data$delays)
    out$delay_mean <- 
      out$delay_mean[, strat := as.character(time)][, time := NULL][, date := NULL]
    out$delay_sd <- extract_parameter("delay_sd", samples, 1:data$delays)
    out$delay_sd <- 
      out$delay_sd[, strat :=  as.character(time)][, time := NULL][, date := NULL]
  }
  if (data$truncation > 0) {
    out$truncation_mean <- extract_parameter("truncation_mean", samples, 1)
    out$truncation_mean <- 
      out$truncation_mean[, strat := as.character(time)][, time := NULL][, date := NULL]
    out$truncation_sd <- extract_parameter("truncation_sd", samples, 1)
    out$truncation_sd <- 
      out$truncation_sd[, strat :=  as.character(time)][, time := NULL][, date := NULL]
  }
  if (data$estimate_r == 1) {
    out$gt_mean <- extract_static_parameter("gt_mean", samples)
    out$gt_mean <- out$gt_mean[, value := value.V1][, value.V1 := NULL]
    out$gt_sd <- extract_static_parameter("gt_sd", samples)
    out$gt_sd <- out$gt_sd[, value := value.V1][, value.V1 := NULL]
  }
  if (data$model_type == 1) {
    out$reporting_overdispersion <- extract_static_parameter("rep_phi", samples)
    out$reporting_overdispersion <- out$reporting_overdispersion[, value := value.V1][, 
                                                                   value.V1 := NULL]
  }
  if (data$obs_scale == 1) {
    out$fraction_observed <- extract_static_parameter("frac_obs", samples)
    out$fraction_observed <- out$fraction_observed[, value := value.V1][, 
                                                     value.V1 := NULL]
  }
  return(out)
}

#' Generate initial conditions from a Stan fit
#'
#' @description `r lifecycle::badge("experimental")`
#' Extracts posterior samples to use to initialise a full model fit. This may be useful
#' for certain data sets where the sampler gets stuck or cannot easily be initialised. 
#' In `estimate_infections()`, `epinow()` and `regional_epinow()` this option can be
#' engaged by setting `stan_opts(init_fit = <stanfit>)`.
#' 
#' This implementation is based on the approach taken in [epidemia](https://github.com/ImperialCollegeLondon/epidemia/)
#' authored by James Scott.
#' @param fit A stanfit object
#' @param samples Numeric, defaults to 50. Number of posterior samples.
#' @importFrom purrr map
#' @importFrom rstan extract
#' @export
#' @return A function that when called returns a set of initial conditions as a named list.
extract_inits <- function(fit, samples = 50) {
  # extract and generate samples as function
  init_fun <- function(i) {
    res <- lapply(
      rstan::extract(fit),
      function(x) {
        if (length(dim(x)) == 1) {
          as.array(x[i])
        }
        else if (length(dim(x)) == 2) {
          x[i, ]
        } else {
          x[i, , ]
        }
      }
    )
    for (j in names(res)) {
      if (length(res[j]) == 1) {
        res[[j]] <- as.array(res[[j]])
      }
    }
    res$r <- NULL
    res$log_lik <- NULL
    res$lp__ <- NULL
    return(res)
  }
  # extract samples
  inits <- purrr::map(1:samples, init_fun)
  # set up sampling function
  inits_sample <- function(inits_list = inits) {
    i <- sample(1:length(inits_list), 1)
    return(inits_list[[i]])
  }
}