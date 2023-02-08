#' Extract Samples for a Parameter from a Stan model
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts a single from a list of `stan` output and returns it as a
#' `data.table`.
#
#' @param param Character string indicating the parameter to extract
#'
#' @param samples Extracted stan model (using `rstan::extract`)
#'
#' @param dates A vector identifying the dimensionality of the parameter to
#' extract. Generally this will be a date.
#'
#' @return A data frame containing the parameter name, date, sample id and
#' sample value.
#' @author Sam Abbott
#' @importFrom data.table melt as.data.table
extract_parameter <- function(param, samples, dates) {
  param_df <- data.table::as.data.table(
    t(
      data.table::as.data.table(
        samples[[param]]
      )
    )
  )
  param_df <- param_df[, time := seq_len(.N)]
  param_df <- data.table::melt(param_df,
    id.vars = "time",
    variable.name = "var"
  )

  param_df <- param_df[, var := NULL][, sample := seq_len(.N), by = .(time)]
  param_df <- param_df[, date := dates, by = .(sample)]
  param_df <- param_df[, .(
    parameter = param, time, date,
    sample, value
  )]
  return(param_df)
}


#' Extract Samples from a Parameter with a Single Dimension
#'
#' @inheritParams extract_parameter
#' @return A data frame containing the parameter name, sample id and sample
#' value
#' @author Sam Abbott
extract_static_parameter <- function(param, samples) {
  data.table::data.table(
    parameter = param,
    sample = seq_along(samples[[param]]),
    value = samples[[param]]
  )
}


#' Extract Parameter Samples from a Stan Model
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts a custom set of parameters from a stan object and adds
#' stratification and dates where appropriate.
#'
#' @param stan_fit A fit Stan model as returned by `rstan:sampling`.
#'
#' @param data A list of the data supplied to the `rstan::sampling` call.
#'
#' @param reported_dates A vector of dates to report estimates for.
#'
#' @param reported_inf_dates A vector of dates to report infection estimates
#' for.
#'
#' @param drop_length_1 Logical; whether the first dimension should be dropped
#' if it is off length 1; this is necessary when processing simulation results.
#'
#' @param merge if TRUE, merge samples and data so that parameters can be
#' extracted from data.
#'
#' @return A list of dataframes each containing the posterior of a parameter
#' @author Sam Abbott
#' @importFrom rstan extract
#' @importFrom data.table data.table
extract_parameter_samples <- function(stan_fit, data, reported_dates,
                                      reported_inf_dates,
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
  out$infections <- extract_parameter(
    "infections",
    samples,
    reported_inf_dates
  )
  out$infections <- out$infections[date >= min(reported_dates)]
  out$reported_cases <- extract_parameter(
    "imputed_reports",
    samples,
    reported_dates
  )
  if (data$estimate_r == 1) {
    out$R <- extract_parameter(
      "R",
      samples,
      reported_dates
    )
    if (data$bp_n > 0) {
      out$breakpoints <- extract_parameter(
        "bp_effects",
        samples,
        1:data$bp_n
      )
      out$breakpoints <- out$breakpoints[,
        strat := date][, c("time", "date") := NULL
      ]
    }
  } else {
    out$R <- extract_parameter(
      "gen_R",
      samples,
      reported_dates
    )
  }
  out$growth_rate <- extract_parameter(
    "r",
    samples,
    reported_dates
  )
  if (data$week_effect > 1) {
    out$day_of_week <- extract_parameter(
      "day_of_week_simplex",
      samples,
      1:data$week_effect
    )
    out$day_of_week <- out$day_of_week[, value := value * data$week_effect]
    out$day_of_week <- out$day_of_week[, strat := date][,
     c("time", "date") := NULL
    ]
  }
  if (data$delay_n_p > 0) {
    out$delay_mean <- extract_parameter(
      "delay_mean", samples, seq_len(data$delay_n_p)
    )
    out$delay_mean <-
      out$delay_mean[, strat := as.character(time)][, time := NULL][,
        date := NULL
      ]
    out$delay_sd <- extract_parameter(
      "delay_sd", samples, seq_len(data$delay_n_p)
    )
    out$delay_sd <-
      out$delay_sd[, strat := as.character(time)][, time := NULL][,
       date := NULL
      ]
  }
  if (data$trunc_n_p > 0) {
    out$truncation_mean <- extract_parameter(
      "trunc_mean", samples, seq_len(data$trunc_n_p)
    )
    out$truncation_mean <- out$truncation_mean[,
        strat := as.character(time)][, time := NULL][, date := NULL
    ]
    out$truncation_sd <- extract_parameter(
      "trunc_sd", samples, seq_len(trunc_n_p)
    )
    out$truncation_sd <- out$truncation_sd[,
      strat := as.character(time)][, time := NULL][, date := NULL
    ]
  }
  if (data$estimate_r && data$gt_n_p > 0) {
    out$gt_mean <- extract_parameter("gt_mean", samples, seq_len(data$gt_n_p))
    out$gt_mean[, strat := as.character(time)][, time := NULL][, date := NULL]
    out$gt_sd <- extract_parameter("gt_sd", samples, seq_len(data$gt_n_p))
    out$gt_sd[, strat := as.character(time)][, time := NULL][, date := NULL]
  }
  if (data$model_type == 1) {
    out$reporting_overdispersion <- extract_static_parameter("rep_phi", samples)
    out$reporting_overdispersion <- out$reporting_overdispersion[,
     value := value.V1][,
      value.V1 := NULL
    ]
  }
  if (data$obs_scale == 1) {
    out$fraction_observed <- extract_static_parameter("frac_obs", samples)
    out$fraction_observed <- out$fraction_observed[, value := value.V1][,
      value.V1 := NULL
    ]
  }
  return(out)
}

#' Extract a Parameter Summary from a Stan Object
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts summarised parameter posteriors from a `stanfit` object using
#' `rstan::summary` in a format consistent with other summary functions in
#' `EpiNow2`.
#'
#' @param fit A `stanfit` objec.
#
#' @param params A character vector of parameters to extract. Defaults to all
#' parameters.
#'
#' @param var_names Logical defaults to `FALSE`. Should variables be named.
#' Automatically set to TRUE if multiple parameters are to be extracted.
#'
#' @return A `data.table` summarising parameter posteriors. Contains a
#' following variables: `variable`, `mean`, `mean_se`, `sd`, `median`, and
#' `lower_`, `upper_` followed by credible interval labels indicating the
#' credible intervals present.
#'
#' @author Sam Abbott
#' @inheritParams calc_summary_measures
#' @export
#' @importFrom data.table as.data.table :=
#' @importFrom rstan summary
extract_stan_param <- function(fit, params = NULL,
                               CrIs = c(0.2, 0.5, 0.9), var_names = FALSE) {
  # generate symmetric CrIs
  CrIs <- CrIs[order(CrIs)]
  sym_CrIs <- c(0.5, 0.5 - CrIs / 2, 0.5 + CrIs / 2)
  sym_CrIs <- sym_CrIs[order(sym_CrIs)]
  CrIs <- round(100 * CrIs, 0)
  CrIs <- c(paste0("lower_", rev(CrIs)), "median", paste0("upper_", CrIs))
  args <- list(object = fit, probs = sym_CrIs)
  if (!is.null(params)) {
    if (length(params) > 1) {
      var_names <- TRUE
    }
    args <- c(args, pars = params)
  } else {
    var_names <- TRUE
  }
  summary <- do.call(rstan::summary, args)
  summary <- data.table::as.data.table(summary$summary,
    keep.rownames = ifelse(var_names,
      "variable",
      FALSE
    )
  )
  cols <- c("mean", "se_mean", "sd", CrIs, "n_eff", "Rhat")
  if (var_names) {
    cols <- c("variable", cols)
  }
  colnames(summary) <- cols
  summary <- summary[, c("n_eff", "Rhat") := NULL]
  return(summary)
}

#' Generate initial conditions from a Stan fit
#'
#' @description `r lifecycle::badge("experimental")`
#' Extracts posterior samples to use to initialise a full model fit. This may
#' be useful for certain data sets where the sampler gets stuck or cannot
#' easily be initialised. In `estimate_infections()`, `epinow()` and\
#' `regional_epinow()` this option can be engaged by setting
#' `stan_opts(init_fit = <stanfit>)`.
#'
#' This implementation is based on the approach taken in
#' [epidemia](https://github.com/ImperialCollegeLondon/epidemia/) authored by
#' James Scott.
#'
#' @param fit A stanfit object.
#'
#' @param current_inits A function that returns a list of initial conditions
#' (such as `create_initial_conditions()`). Only used in `exclude_list` is
#' specified.
#'
#' @param exclude_list A character vector of parameters to not initialise from
#' the fit object, defaulting to `NULL`.
#'
#' @param samples Numeric, defaults to 50. Number of posterior samples.
#'
#' @return A function that when called returns a set of initial conditions as a
#' named list.
#'
#' @author Sam Abbott
#' @importFrom purrr map
#' @importFrom rstan extract
#' @export

extract_inits <- function(fit, current_inits,
                          exclude_list = NULL,
                          samples = 50) {
  # extract and generate samples as function
  init_fun <- function(i) {
    res <- lapply(
      rstan::extract(fit),
      function(x) {
        if (length(dim(x)) == 1) {
          as.array(x[i])
        } else if (length(dim(x)) == 2) {
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
    res$infections <- NULL
    res$reports <- NULL
    res$obs_reports <- NULL
    res$imputed_reports <- NULL
    return(res)
  }
  # extract samples
  fit_inits <- purrr::map(1:samples, init_fun) # nolint
  # set up sampling function
  exclude_vars <- exclude_list
  old_init_fn <- current_inits
  inits_sample <- function(inits_list = fit_inits,
                           old_inits = old_init_fn,
                           exclude = exclude_vars) {
    i <- sample(seq_along(inits_list), 1)
    fit_inits <- inits_list[[i]]
    if (!is.null(exclude_list)) {
      old_inits_sample <- old_inits()
      old_inits_sample <- old_inits_sample[exclude]
      new_inits <- update_list(fit_inits, old_inits_sample)
    } else {
      new_inits <- fit_inits
    }
    return(new_inits)
  }
  return(inits_sample)
}
