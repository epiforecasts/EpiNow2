#' Extract Samples for a Parameter from a Stan model
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts a single from a list of stan output and returns it as a
#' `<data.table>`.
#
#' @param param Character string indicating the parameter to extract
#'
#' @param samples Extracted stan model (using [rstan::extract()])
#'
#' @param dates A vector identifying the dimensionality of the parameter to
#' extract. Generally this will be a date.
#'
#' @return A `<data.frame>` containing the parameter name, date, sample id and
#' sample value.
#' @importFrom data.table melt as.data.table
#' @keywords internal
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
  param_df[, .(
    parameter = param, time, date,
    sample, value
  )]
}


#' Extract Samples from a Parameter with a Single Dimension
#'
#' @inheritParams extract_parameter
#' @return A `<data.frame>` containing the parameter name, sample id and sample
#' value
#' @keywords internal
extract_static_parameter <- function(param, samples) {
  id <- samples[[paste(param, "id", sep = "_")]]
  lookup <- samples[["params_variable_lookup"]][id]
  data.table::data.table(
    parameter = param,
    sample = seq_along(samples[["params"]][, lookup]),
    value = samples[["params"]][, lookup]
  )
}

#' Extract all samples from a stan fit
#'
#' If the `object` argument is a `<stanfit>` object, it simply returns the
#' result of [rstan::extract()]. If it is a `<CmdStanMCMC>` it returns samples
#' in the same format as [rstan::extract()] does for `<stanfit>` objects.
#' @param stan_fit A `<stanfit>` or `<CmdStanMCMC>` object as returned by
#'   [fit_model()].
#' @param pars Any selection of parameters to extract
#' @param include whether the parameters specified in `pars` should be included
#' (`TRUE`, the default) or excluded (`FALSE`)
#' @importFrom cli cli_abort
#' @return List of data.tables with samples
#' @export
#'
#' @importFrom data.table data.table melt setkey
#' @importFrom rstan extract
extract_samples <- function(stan_fit, pars = NULL, include = TRUE) {
  if (inherits(stan_fit, "stanfit")) {
    extract_args <- list(object = stan_fit, include = include)
    if (!is.null(pars)) extract_args <- c(extract_args, list(pars = pars))
    return(do.call(rstan::extract, extract_args))
  }
  if (!inherits(stan_fit, "CmdStanMCMC") &&
        !inherits(stan_fit, "CmdStanFit")) {
    cli_abort(
      "{.var stan_fit} must be a {.cls stanfit}, {.cls CmdStanMCMC} or
      {.cls CmdStanFit} object."
    )
  }

  # extract sample from stan object
  if (!include) {
    all_pars <- stan_fit$metadata()$stan_variables
    pars <- setdiff(all_pars, pars)
  }
  samples_df <- data.table::data.table(stan_fit$draws(
    variables = pars, format = "df"
  ))
  # convert to rstan format
  samples_df <- suppressWarnings(data.table::melt(
    samples_df,
    id.vars = c(".chain", ".iteration", ".draw")
  ))
  samples_df <- samples_df[
    ,
    index := sub("^.*\\[([0-9,]+)\\]$", "\\1", variable)
  ][
    ,
    variable := sub("\\[.*$", "", variable)
  ]
  samples <- split(samples_df, by = "variable")
  samples <- purrr::map(samples, function(df) {
    permutation <- sample(max(df$.draw), max(df$.draw), replace = FALSE)
    df <- df[, new_draw := permutation[.draw]]
    setkey(df, new_draw)
    max_indices <- strsplit(tail(df$index, 1), split = ",", fixed = TRUE)[[1]]
    if (any(grepl("[^0-9]", max_indices))) {
      max_indices <- 1
    } else {
      max_indices <- as.integer(max_indices)
    }
    ret <- aperm(
      a = array(df$value, dim = c(max_indices, length(permutation))),
      perm = c(length(max_indices) + 1, seq_along(max_indices))
    )
    ## permute
    dimnames(ret) <- c(
      list(iterations = NULL), rep(list(NULL), length(max_indices))
    )
    ret
  })

  return(samples)
}

#' Extract Parameter Samples from a Stan Model
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts a custom set of parameters from a stan object and adds
#' stratification and dates where appropriate.
#'
#' @param data A list of the data supplied to the [fit_model()] call.
#'
#' @param reported_dates A vector of dates to report estimates for.
#'
#' @param imputed_dates A vector of dates to report imputed reports for.
#'
##' @param reported_inf_dates A vector of dates to report infection estimates
#' for.
#'
#' @param drop_length_1 Logical; whether the first dimension should be dropped
#' if it is off length 1; this is necessary when processing simulation results.
#'
#' @param merge if TRUE, merge samples and data so that parameters can be
#' extracted from data.
#'
#' @inheritParams extract_samples
#' @return A list of `<data.frame>`'s each containing the posterior of a
#' parameter
#' @importFrom rstan extract
#' @importFrom data.table data.table
#' @keywords internal
extract_parameter_samples <- function(stan_fit, data, reported_dates,
                                      imputed_dates, reported_inf_dates,
                                      drop_length_1 = FALSE, merge = FALSE) {
  # extract sample from stan object
  samples <- extract_samples(stan_fit)

  ## drop initial length 1 dimensions if requested
  if (drop_length_1) {
    samples <- lapply(samples, function(x) {
      if (length(dim(x)) > 1 && dim(x)[1] == 1) dim(x) <- dim(x)[-1]
      x
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
    imputed_dates
  )
  if ("estimate_r" %in% names(data)) {
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
        out$breakpoints <- out$breakpoints[
          ,
          strat := date
        ][, c("time", "date") := NULL]
      }
    } else {
      out$R <- extract_parameter(
        "gen_R",
        samples,
        reported_dates
      )
    }
  }
  out$growth_rate <- extract_parameter(
    "r",
    samples,
    reported_dates[-1]
  )
  if (data$week_effect > 1) {
    out$day_of_week <- extract_parameter(
      "day_of_week_simplex",
      samples,
      1:data$week_effect
    )
    out$day_of_week <- out$day_of_week[, value := value * data$week_effect]
    out$day_of_week <- out$day_of_week[, strat := date][
      ,
      c("time", "date") := NULL
    ]
  }
  if (data$delay_n_p > 0) {
    out$delay_params <- extract_parameter(
      "delay_params", samples, seq_len(data$delay_params_length)
    )
    out$delay_params <-
      out$delay_params[, strat := as.character(time)][, time := NULL][
        ,
        date := NULL
      ]
  }
  if (data$model_type == 1) {
    out$reporting_overdispersion <- extract_static_parameter(
      "dispersion", samples
    )
  }
  if ("obs_scale_sd" %in% names(data) && data$obs_scale_sd > 0) {
    out$fraction_observed <- extract_static_parameter("frac_obs", samples)
  }
  return(out)
}

#' Extract a Parameter Summary from a Stan Object
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts summarised parameter posteriors from a `stanfit` object using
#' `rstan::summary()` in a format consistent with other summary functions
#' in `{EpiNow2}`.
#'
#' @param fit A `<stanfit>` objec.
#
#' @param params A character vector of parameters to extract. Defaults to all
#' parameters.
#'
#' @param var_names Logical defaults to `FALSE`. Should variables be named.
#' Automatically set to TRUE if multiple parameters are to be extracted.
#'
#' @return A `<data.table>` summarising parameter posteriors. Contains a
#' following variables: `variable`, `mean`, `mean_se`, `sd`, `median`, and
#' `lower_`, `upper_` followed by credible interval labels indicating the
#' credible intervals present.
#'
#' @inheritParams calc_summary_measures
#' @export
#' @importFrom posterior mcse_mean
#' @importFrom data.table as.data.table :=
#' @importFrom rstan summary
extract_stan_param <- function(fit, params = NULL,
                               CrIs = c(0.2, 0.5, 0.9), var_names = FALSE) {
  # generate symmetric CrIs
  CrIs <- sort(CrIs)
  sym_CrIs <- c(0.5, 0.5 - CrIs / 2, 0.5 + CrIs / 2)
  sym_CrIs <- sort(sym_CrIs)
  CrIs <- round(100 * CrIs, 0)
  CrIs <- c(paste0("lower_", rev(CrIs)), "median", paste0("upper_", CrIs))
  if (!is.null(params)) {
    if (length(params) > 1) {
      var_names <- TRUE
    }
  } else {
    var_names <- TRUE
  }
  if (inherits(fit, "stanfit")) { # rstan backend
    summary_args <- list(object = fit, probs = sym_CrIs)
    if (!is.null(params)) summary_args <- c(summary_args, list(pars = params))
    param_summary <- do.call(rstan::summary, summary_args)
    param_summary <- data.table::as.data.table(param_summary$summary,
      keep.rownames = ifelse(var_names,
        "variable",
        FALSE
      )
    )
    param_summary <- param_summary[, c("n_eff", "Rhat") := NULL]
  } else if (inherits(fit, "CmdStanMCMC")) { # cmdstanr backend
    param_summary <- fit$summary(
      variable = params,
      mean, mcse_mean, sd, ~ quantile(.x, probs = sym_CrIs)
    )
    if (!var_names) param_summary$variable <- NULL
    param_summary <- data.table::as.data.table(param_summary)
  }
  cols <- c("mean", "se_mean", "sd", CrIs)
  if (var_names) {
    cols <- c("variable", cols)
  }
  colnames(param_summary) <- cols
  return(param_summary)
}

#' Generate initial conditions from a Stan fit
#'
#' @description `r lifecycle::badge("experimental")`
#' Extracts posterior samples to use to initialise a full model fit. This may
#' be useful for certain data sets where the sampler gets stuck or cannot
#' easily be initialised. In [estimate_infections()], [epinow()] and
#' [regional_epinow()] this option can be engaged by setting
#' `stan_opts(init_fit = <stanfit>)`.
#'
#' This implementation is based on the approach taken in
#' [epidemia](https://github.com/ImperialCollegeLondon/epidemia/) authored by
#' James Scott.
#'
#' @param fit A `<stanfit>` object.
#'
#' @param current_inits A function that returns a list of initial conditions
#' (such as [create_initial_conditions()]). Only used in `exclude_list` is
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
#' @importFrom purrr map
#' @importFrom rstan extract
#' @importFrom utils modifyList
#' @export
extract_inits <- function(fit, current_inits,
                          exclude_list = NULL,
                          samples = 50) {
  # extract and generate samples as function
  init_fun <- function(i) {
    res <- lapply(
      extract_samples(fit),
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
    res
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
      new_inits <- modifyList(fit_inits, old_inits_sample)
    } else {
      new_inits <- fit_inits
    }
    return(new_inits)
  }
  return(inits_sample)
}
