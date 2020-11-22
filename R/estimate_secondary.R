#' Estimate a Secondary Observation from a Primary Observation
#'
#' @description `r lifecycle::badge("experimental")`
#' Estimates the relationship between a primary and secondary observation, for 
#' example hospital admissions and deaths or hospital admissions and bed 
#' occupancy. 
#' @param secondary A call to `secondary_opts()` or a list containing the following 
#' binary variables: cumulative, historic, primary_hist_additive, current, 
#' primary_current_additive. These parameters control the structure of the 
#' secondary model, see `secondary_opts()` for details.
#' @param delays A call to `delay_opts()` defining delay distributions and options
#' for the relationship between the primary and secondary observed data. See the 
#' documentation of `delay_opts()` for generic details. BY default a diffuse prior 
#' is assumed with a mean of 14 days and standard deviation of 7 days (both with a 
#' standard deviation of 1 on the log scale).
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
#' # make some example secondary incidence data
#' cases <- example_confirmed
#' cases <- as.data.table(cases)
#' cases <- cases[, .(date, primary = confirm, secondary = shift(confirm, n = 7, type = "lag"))]
#' cases <- cases[, secondary := frollmean(secondary, 3, align = "right")]
#' cases <- cases[!is.na(secondary)][, secondary := as.integer(secondary)]
#' 
#' # dev model compile
#' model <- rstan::stan_model("inst/stan/estimate_secondary.stan")
#'
#' # fit model to example data
#' inc <- estimate_secondary(cases, verbose = interactive(), model = model,
#'                           chains = 2, iter = 1000)
#' plot(inc, primary = TRUE)
#' \donttest{
#' # make some example prevalence data
#' cases <- example_confirmed
#' cases <- as.data.table(cases)
#' cases <- 
#'   cases[, .(date, primary = confirm, scaled_primary = confirm * 0.1)]
#' cases$secondary <- 0
#' cases$secondary[1:6] <- as.integer(cumsum(cases$scaled_primary[1:6]))
#' for (i in 7:nrow(cases)) {
#'   cases$secondary[i] <- as.integer(
#'   cases$secondary[i -1] + cases$scaled_primary[i] - 
#'           cases$scaled_primary[i - 6] * 0.125-
#'           cases$scaled_primary[i - 5] * 0.75 -
#'           cases$scaled_primary[i - 4] * 0.125)
#'   cases$secondary[i] <- ifelse(cases$secondary[i] < 0, 0,
#'                                cases$secondary[i])
#' }
#' 
#' # fit model to example data
#' # here we assume no day of the week effect
#' # this is motivated by the expected level of auto-correlation in prevalence
#' # variables
#' prev <- estimate_secondary(cases, verbose = interactive(), model = model,
#'                           secondary = secondary_opts(type = "prevalence"),
#'                           obs = obs_opts(week_effect = TRUE, 
#'                                          scale = list(mean = 0.2, sd = 0.1)),
#'                           control = list(max_treedepth = 15))
#' plot(prev)
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
                                verbose = TRUE,
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

