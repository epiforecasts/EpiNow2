#' Estimate a Secondary Observation from a Primary Observation
#'
#' @description `r lifecycle::badge("experimental")`
#' Estimates the relationship between a primary and secondary observation, for 
#' example hospital admissions and deaths or hospital admissions and bed 
#' occupancy. See `secondary_opts()` for model structure options. See parameter 
#' documentation for model defaults and options.
#' @param secondary A call to `secondary_opts()` or a list containing the following 
#' binary variables: cumulative, historic, primary_hist_additive, current, 
#' primary_current_additive. These parameters control the structure of the 
#' secondary model, see `secondary_opts()` for details.
#' @param delays A call to `delay_opts()` defining delay distributions between
#' primary and secondary observations See the documentation of `delay_opts()` for 
#' details. BY default a diffuse prior  is assumed with a mean of 14 days and 
#' standard deviation of 7 days (both with a standard deviation of 1 on the log scale).
#' @param reports A data frame containing the `date` of report and both `primary` 
#' and `secondary` reports.
#' @param model A compiled stan model to override the default model. May be
#' useful for package developers or those developing extensions.
#' @param verbose Logical, should model fitting progress be returned. Defaults to
#' `interactive()`.
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
#' 
#' # make some example secondary incidence data
#' cases <- example_confirmed
#' cases <- as.data.table(cases)
#' 
#' # apply a convolution of a log normal to a vector of observations
#' weight_cmf <- function(x, ...) {
#'    set.seed(x[1])
#'    meanlog <- rnorm(1, 1.6, 0.2)
#'    sdlog <- rnorm(1, 0.8, 0.1)
#'    cmf <- cumsum(dlnorm(1:length(x), meanlog, sdlog)) - 
#'            cumsum(dlnorm(0:(length(x) - 1), meanlog, sdlog))
#'    conv <- sum(x * rev(cmf), na.rm = TRUE)
#'    conv <- round(conv, 0)
#'  return(conv)
#' }
#' # roll over observed cases to produce a convolution
#' cases <- cases[, .(date, primary = confirm, secondary = confirm)]
#' cases <- cases[, secondary := frollapply(secondary, 15, weight_cmf, align = "right")]
#' cases <- cases[!is.na(secondary)][, secondary := as.integer(secondary)]
#' # add a day of the week effect and scale secondary observations at 40% of primary
#' cases <- cases[lubridate::wday(date) == 1, secondary := round(0.5 * secondary, 0)] 
#' cases <- cases[, secondary := round(secondary * rnorm(.N, 0.4, 0.025), 0)]
#' cases <- cases[secondary < 0, secondary := 0]
#' 
#' # fit model to example data assuming only a given fraction of primary observations
#' # become secondary observations
#' inc <- estimate_secondary(cases[1:100], chains = 2, iter = 1000, 
#'                           obs = obs_opts(scale = list(mean = 0.2, sd = 0.2)))
#' plot(inc, primary = TRUE)
#' \donttest{
#' # make some example prevalence data
#' cases <- example_confirmed
#' cases <- as.data.table(cases)
#' cases <- 
#'   cases[, .(date, primary = confirm, 
#'                   scaled_primary = confirm * rnorm(.N, 0.25, 0.05))]
#' cases$secondary <- 0
#' cases$secondary[1] <- as.integer(cases$scaled_primary[1])
#' for (i in 2:nrow(cases)) {
#'   meanlog <- rnorm(1, 1.6, 0.1)
#'   sdlog <- rnorm(1, 0.8, 0.01)
#'   cmf <- cumsum(dlnorm(1:min(i-1,20), meanlog, sdlog)) - 
#'            cumsum(dlnorm(0:min(19,i-2), meanlog, sdlog))
#'   reducing_cases <- sum(cases$scaled_primary[(i-1):max(1,i-20)] * cmf)
#'   reducing_cases <- ifelse(cases$secondary[i - 1] < reducing_cases, 
#'                            cases$secondary[i - 1], reducing_cases) 
#'   cases$secondary[i] <- as.integer(
#'   cases$secondary[i - 1] + cases$scaled_primary[i] - reducing_cases
#'   ) 
#'   cases$secondary[i] <- ifelse(cases$secondary[i] < 0, 0,
#'                                cases$secondary[i])
#' }
#' # fit model to example prevalence data
#' # here we assume no day of the week effect and a Poisson observation model
#' # this is motivated by the expected level of auto-correlation in cumulative
#' prev <- estimate_secondary(cases, secondary = secondary_opts(type = "prevalence"),
#'                           obs = obs_opts(week_effect = FALSE, 
#'                                          scale = list(mean = 0.3, sd = 0.1)))
#' plot(prev, primary = TRUE)
#' }
estimate_secondary <- function(reports, 
                               secondary = secondary_opts(),
                               delays = delay_opts(
                                  list(mean = 2.5, mean_sd = 1, 
                                       sd = 0.47, sd_sd = 1, max = 30)),
                                truncation = trunc_opts(),
                                obs = obs_opts(),
                                CrIs = c(0.2, 0.5, 0.9),
                                model = NULL, 
                                verbose = interactive(),
                                ...) { 
  reports <- data.table::as.data.table(reports)
  # observation and control data
  data <- list( 
    t = nrow(reports), 
    obs = reports$secondary,
    primary = reports$primary,
    day_of_week = lubridate::wday(reports$date, week_start = 1)
  )
  # secondary model options
  data <- c(data, secondary)
  # delay data
  data <- c(data, delays)
  data$seeding_time <- 0
  # truncation data
  data <- c(data, truncation)
  # observation model data
  data <- c(data, create_obs_model(obs))
  
  # initial conditions (from estimate_infections)
  inits <- create_initial_conditions(
    c(data, list(estimate_r = 0, fixed = 1, bp_n = 0))
    )
  # fit
  if (is.null(model)) {
    model <- stanmodels$estimate_secondary
  }
  fit <- rstan::sampling(model, 
                         data = data, 
                         init = inits,
                         refresh = ifelse(verbose, 50, 0),
                         ...)
  
  out <- list()
  out$predictions <- extract_stan_param(fit, "sim_secondary", CrIs = CrIs)
  out$predictions <- out$predictions[, lapply(.SD, round, 1)]
  out$predictions <- cbind(reports, out$predictions)
  out$data <- data
  out$fit <- fit
  class(out) <- c("estimate_secondary", class(out))
  return(out)
}

#' Secondary Reports Options
#' 
#' @description `r lifecycle::badge("experimental")`
#' Returns a list of options defining the secondary model used in `estimate_secondary()`. 
#' This model is a combination of a convolution of previously observed primary reports 
#' combined with current primary reports (either additive or subtractive). This model 
#' can optionally be cumulative. See the documentation of `type` for sensible options 
#' to cover most use cases and the returned values of `secondary_opts()` for all 
#' currently supported options.
#' @param type A character string indicating the type of observation the secondary reports
#' are. Options include: 
#' - "incidence": Assumes that secondary reports equal a convolution of previously
#' observed primary reported cases. An example application is deaths from an infectious
#' disease predicted by reported cases of that disease (or estimated infections).
#' - "prevalence": Assumes that secondary reports are cumulative and are defined by
#' currently observed primary reports minus a convolution of secondary reports. An example
#' application is hospital bed usage predicted by hospital admissions.
#' @param ... Overwrite options defined by type. See the returned values for all 
#' options that can be passed.
#' @seealso estimate_secondary
#' @return A list of binary options summarising secondary model used in `estimate_secondary()`. 
#' Options returned are `cumulative` (should the secondary report be cumulative), `historic`
#' (should a convolution of primary reported cases be used to predict secondary reported 
#' cases), `primary_hist_additive` (should the historic convolution of primary reported cases 
#' be additive or subtractive), `current` (should currently observed primary reported cases 
#' contribute to current secondary reported cases), `primary_current_additive` (should current
#' primary reported cases be additive or subtractive).
#' @export
#' @examples
#' # incidence model
#' secondary_opts("incidence")
#' 
#' # prevalence model
#' secondary_opts("prevalence")
secondary_opts <- function(type = "incidence", ...)  {
  type <- match.arg(type, choices = c("incidence", "prevalence"))
  if (type %in% "incidence") {
    data <- list(
      cumulative = 0,               
      historic = 1,               
      primary_hist_additive = 1,   
      current = 0,               
      primary_current_additive = 0
    )
  }else if (type %in% "prevalence") {
    data <- list(
      cumulative = 1,               
      historic = 1,               
      primary_hist_additive = 0,   
      current = 1,               
      primary_current_additive = 1
    )
  }
  data <- update_list(data, list(...))
  return(data)
}

#' Plot method for estimate_secondary
#'
#' @description `r lifecycle::badge("experimental")`
#' `plot` method for class "estimate_secondary". 
#' @param x A list of output as produced by `estimate_secondary`
#' @param primary Logical, defaults to `FALSE`. Should `primary` reports also
#' be plot? 
#' @param ... Pass additional arguments to plot function. Not currently in use.
#' @seealso plot estimate_secondary
#' @method plot estimate_secondary
#' @return `ggplot2` object
#' @importFrom ggplot2 ggplot aes geom_col geom_point labs scale_x_date scale_y_continuous theme
#' @importFrom cowplot theme_cowplot
#' @export
plot.estimate_secondary <- function(x, primary = FALSE, ...) {
  plot <- ggplot2::ggplot(x$predictions, ggplot2::aes(x = date, y = secondary)) +
    ggplot2::geom_col(fill = "grey", col = "white",
                      show.legend = FALSE, na.rm = TRUE)
  
  if (primary) {
    plot <- plot + 
       ggplot2::geom_point(data = x$predictions,
                          ggplot2::aes(y = primary), 
                          alpha = 0.4, size = 0.8) +
      ggplot2::geom_line(data = x$predictions,
                          ggplot2::aes(y = primary), alpha = 0.4)
      
  }
  
  plot <- plot_CrIs(plot, extract_CrIs(x$predictions),
                    alpha = 0.6, size = 1)
  
  plot <- plot +       
    cowplot::theme_cowplot() +
    ggplot2::labs(y = "Confirmed Cases", x = "Date") +
    ggplot2::scale_x_date(date_breaks = "week", date_labels = "%b %d") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  return(plot)
}

predict.estimate_secondary <- function(object, ...) {
  
  
  return(object)
}

#' Simulate infections using a given trajectory of the time-varying reproduction number
#'
#' @description `r lifecycle::badge("stable")`
#' This function simulates infections using an existing fit to observed cases but with a modified 
#' time-varying reproduction number. This can be used to explore forecast models or past counterfactuals.
#' Simulations can be run in parallel using `future::plan`.
#' @param estimates The \code{estimates} element of an \code{epinow} run that has been done with 
#' output = "fit", or the result of \code{estimate_infections} with \code{return_fit} set to TRUE.
#' @param model A compiled stan model as returned by `rstan::stan_model`.
#' @param R A numeric vector of reproduction numbers; these will overwrite the reproduction numbers
#'  contained in \code{estimates}, except elements set to NA. If it is longer than the time series 
#'  of reproduction numbers contained in \code{estimates}, the values going beyond the length of 
#'  estimated reproduction numbers are taken as forecast.
#' @param samples Numeric, number of posterior samples to simulate from. The default is to use all
#' samples in the `estimates` input.
#' @param batch_size Numeric, defaults to 100. Size of batches in which to simulate. May decrease 
#' run times due to reduced IO costs but this is still being evaluated. If set to NULL then all 
#' simulations are done at once.
#' @param verbose Logical defaults to `interactive()`. Should a progress bar (from `progressr`) be
#' shown.
#' @importFrom rstan extract sampling
#' @importFrom purrr transpose map
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor
#' @importFrom data.table rbindlist
#' @importFrom lubridate days
#' @export
#' @examples
#' \donttest{
#' #set number of cores to use
#' options(mc.cores = ifelse(interactive(), 4, 1))
#' # load data.table for manipulation
#' library(data.table)
#' # make some future data
#' primary <- example_confirmed[111:130]
#' primary <- as.data.table(primary)
#' primary <- primary[, .(date, sample = list(1:100), value = confirm)]
#' primary <- primary[, .(sample = as.numeric(unlist(sample))), by = c("date", "value")]
#' sims <- simulate_secondary(est, primary)
#' plot(sims)
#' }
simulate_secondary <- function(estimates,
                               primary,
                               model = NULL,
                               samples = NULL,
                               batch_size = 10,
                               verbose = interactive()) {
  
  ## check batch size
  if (!is.null(batch_size)) {
    if (batch_size <= 1) {
      stop("batch_size must be greater than 1")
    }
  }
  
  ## extract samples from given stanfit object
  draws <- rstan::extract(estimates$fit,
                          pars = c("sim_secondary", "log_lik",
                                   "lp__", "secondary"),
                          include = FALSE)
  # extract data
  data <- estimates$data
  
  # combined primary from data and input primary
  updated_primary <- primary
  primary_fit <- estimates$predictions[, .(date, value = primary, sample = list(unique(updated_primary$sample)))]
  primary_fit <- primary_fit[, .(sample = as.numeric(unlist(sample))), by = c("date", "value")]
  primary_fit <- data.table::rbindlist(list(primary_fit, updated_primary), use.names = TRUE)
  data.table::setorderv(primary_fit, c("sample", "date"))
  
  # update data with primary samples and day of week
  data$primary <- t(
    matrix(primary_fit$value, ncol = length(unique(primary_fit$sample)))
  )
  data$day_of_week <- lubridate::wday(unique(primary_fit$date), week_start = 1)
  data$n <- nrow(data$primary)
  data$t <- ncol(data$primary)
  data$h <- nrow(primary[sample == min(sample)])
  
  # extract samples for posterior of estimates
  draws <- purrr::map(draws, ~ as.matrix(.[sample(1:nrow(.), data$n, replace = TRUE),]))
  # combine with data
  data <- c(data, draws)
  
  # load model
  if (is.null(model)) {
    model <- stanmodels$simulate_secondary
  }
  
  # allocate empty parameters
  data <- allocate_empty(data, c("frac_obs", "delay_mean", "delay_sd"),
                         n = data$n)
  
  ## simulate
  sims <- rstan::sampling(object = model,
                          data = data, chains = 1, iter = 1,
                          algorithm = "Fixed_param",
                          refresh = 0)
  
  # extract samples and organise
  dates <- unique(primary_fit$date)
  samples <- rstan::extract(sims, "sim_secondary")$sim_secondary
  samples <- as.data.table(samples)
  colnames(samples) <- c("iterations", "sample", "time", "value")
  samples <- samples[, c("iterations", "time") := NULL][, date := rep(dates, data$n)]
  
  # summarise samples
  summarised <- calc_summary_measures(samples, summarise_by = "date")
  
  # construct output
  out <- list()
  out$samples <- samples
  out$predictions <- summarised
  class(out) <- c("estimate_secondary", class(out))
  return(out)
}
