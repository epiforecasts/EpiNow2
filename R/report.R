#' Report case counts by date of report                        
#' @param case_estimates A data.table of case estimates with the following variables: date, sample, cases
#' @param case_forecast A data.table of case forecasts with the following variables: date, sample, cases. If not supplied the
#' default is not to incorporate forecasts.
#' @param reporting_effect A `data.table` giving the weekly reporting effect with the following variables:
#' `sample` (must be the same as in `nowcast`), `effect` (numeric scaling factor for each weekday), `day`
#' (numeric 1 - 7 (1 = Monday and 7 = Sunday)). If not supplied then no weekly reporting effect is assumed.
#' @export
#' @return A list of `data.table`s. The first entry contains the following variables `sample`, `date` and `cases` with the second
#' being summarised across samples.
#' @inheritParams estimate_infections
#' @inheritParams adjust_infection_to_report
#' @importFrom data.table data.table rbindlist setorder
#' @importFrom future.apply future_lapply
#' @importFrom HDInterval hdi
#' @examples 
#' \donttest{
#' ## Define example cases
#' cases <- EpiNow2::example_confirmed[1:40]
#' 
#'  
#' ## Set up example generation time
#' generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
#'                         mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
#'                         sd = EpiNow2::covid_generation_times[1, ]$sd,
#'                         sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
#'                         max = 30)
#' ## Set                   
#' incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
#'                           mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
#'                           sd = EpiNow2::covid_incubation_period[1, ]$sd,
#'                           sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
#'                           max = 30)
#'                    
#' reporting_delay <- list(mean = log(5),
#'                         mean_sd = log(2),
#'                         sd = log(2),
#'                         sd_sd = log(1.5),
#'                         max = 30)
#'                         
#' ## Run model
#' out <- EpiNow2::estimate_infections(cases, family = "negbin",
#'                                     generation_time = generation_time,
#'                                     delays = list(incubation_period, reporting_delay),
#'                                     samples = 1000, warmup = 200, 
#'                                     cores = ifelse(interactive(), 4, 1), chains = 4,
#'                                     estimate_rt =  FALSE, verbose = TRUE)
#'                             
#'                      
#' reported_cases <- report_cases(case_estimates = out$samples[variable == "infections"][, 
#'                                                             cases := value][, value := NULL],
#'                                delays = list(incubation_period, reporting_delay),
#'                                type = "sample")
#'                                
#' print(reported_cases)
#' }
report_cases <- function(case_estimates,
                         case_forecast = NULL, 
                         delays,
                         type = "sample",
                         reporting_effect) {
  
  samples <- length(unique(case_estimates$sample))
  

  ## Define delay distributions
  delay_defs <- purrr::map(delays, 
                           ~ EpiNow2::lognorm_dist_def(mean = .$mean,
                                                       mean_sd = .$mean_sd,
                                                       sd = .$sd,
                                                       sd_sd = .$sd_sd,
                                                       max_value = .$max,
                                                       samples = samples))
  ## Add a null reporting effect if missing
  if (missing(reporting_effect)) {
    reporting_effect <- data.table::data.table(
      sample = list(1:samples),
      effect = rep(1, 7),
      day = 1:7
    )
    
    reporting_effect <- reporting_effect[, .(sample = unlist(sample)), by = .(effect, day)]
  }
  
  ## Filter and sum nowcast to use only upscaled cases by date of infection
  infections <- data.table::copy(case_estimates)
  
  ## Add in case forecast if present
  if (!is.null(case_forecast)) {
    infections <- data.table::rbindlist(list(
      infections,
      case_forecast[, .(date, sample, cases = as.integer(cases))]
    ), use.names = TRUE)
  }
  
  ## For each sample map to report date
  report <- future.apply::future_lapply(1:max(infections$sample), 
                     function(id) {EpiNow2::adjust_infection_to_report(infections[sample == id], 
                                                          delay_defs = purrr::map(delay_defs, ~ .[id, ]),
                                                          type = type,
                                                          reporting_effect = reporting_effect[sample == id, ]$effect)})

  report <- data.table::rbindlist(report, idcol = "sample")
    
  out <- list()
  
  ## Bind all samples together
  out$samples <- report
  
  ## Summarise samples
  out$summarised <- report[, .(
    bottom  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.9)), ~ .[[1]])),
    top = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.9)), ~ .[[2]])),
    lower  = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.5)), ~ .[[1]])),
    upper = as.numeric(purrr::map_dbl(list(HDInterval::hdi(cases, credMass = 0.5)), ~ .[[2]])),
    central_lower = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.2)), ~ .[[1]])), 
    central_upper = as.numeric(purrr::map_dbl(list(HDInterval::hdi(value, credMass = 0.2)), ~ .[[2]])),
    median = as.numeric(median(cases, na.rm = TRUE)),
    mean = as.numeric(mean(cases, na.rm = TRUE)),
    sd = as.numeric(sd(cases, na.rm = TRUE))), by = .(date)]
  
  ## Order summarised samples
  data.table::setorder(out$summarised, date) 
  
  return(out)
}


#' Provide Summary Statistics for Estimated Infections and Rt
#' @param summarised_estimates A data.table of summarised estimates containing the following variables:
#'  variable, median, bottom, and top. It should contain the following estimates: R, infections, and r 
#'  (rate of growth).
#' @param rt_samples A data.table containing Rt samples with the following variables: sample and value.
#' @return A data.table containing formatted and numeric summary measures
#' @export
#' @importFrom data.table data.table setDT
#' @importFrom purrr map
report_summary <- function(summarised_estimates,
                           rt_samples) { 
  
  ## Set input to data.table
  summarised_estimates <- data.table::setDT(summarised_estimates)
  rt_samples <- data.table::setDT(rt_samples)
  
  ## Extract values of interest
  summarised_estimates <- summarised_estimates[, .(variable, point = median,
                                                   lower = bottom, upper = top,
                                                   mid_lower = lower, mid_upper = upper,
                                                   central_lower = central_lower,
                                                   central_upper = central_upper)]
  ## Extract latest R estimate
  R_latest <- summarised_estimates[variable == "R"][, variable := NULL][,
                                   purrr::map(.SD, ~ round(., 2))]
   
  ## Estimate probability of control
  prob_control <- rt_samples[, .(prob_control = sum(value <= 1) / .N)]$prob_control
  prob_control <- signif(prob_control, 2)
  
  ##Extract current cases
  current_cases <- summarised_estimates[variable == "infections"][, variable := NULL][,
                                        purrr::map(.SD, ~ round(., 0))]
  

  ## Get individual estimates
  r_latest <- summarised_estimates[variable == "growth_rate"][, variable := NULL][,
                                  purrr::map(.SD, ~ round(., 2))]
  
  doubling_time <- function(r) {
    round(log(2) * 1 / r, 1)
  }

  doubling_time_latest <- summarised_estimates[variable == "growth_rate"][,
                                .(point = doubling_time(point),
                                  lower = doubling_time(upper),
                                  upper = doubling_time(lower))]
  

  ## Regional summary
 summary <- data.table::data.table(
    measure = c("New confirmed cases by infection date",
                "Expected change in daily cases",
                "Effective reproduction no.",
                "Rate of growth",
                "Doubling/halving time (days)"),
    estimate = c(make_conf(current_cases),
                 as.character(EpiNow2::map_prob_change(prob_control)),
                 make_conf(R_latest, digits = 2),
                 make_conf(r_latest, digits = 2),
                 make_conf(doubling_time_latest, digits = 1)),
    numeric_estimate = list(current_cases,
                         prob_control,
                         R_latest,
                         r_latest,
                         doubling_time_latest)
  )

  return(summary) 
}



#' Report plots
#'
#' @param summarised_estimates A data.table of summarised estimates containing the following variables:
#'  variable, median, bottom, and top. It should contain the following estimates: R, infections, reported_cases_rt,
#'   and r (rate of growth).
#' @importFrom ggplot2 ggsave theme labs scale_x_date 
#' @importFrom cowplot theme_cowplot
#' @importFrom patchwork plot_layout
#' @importFrom data.table setDT
#' @inheritParams epinow
#' @inheritParams plot_estimates
#' @return A `ggplot2` object
#' @export
#' @examples 
#' \donttest{
#' ## Define example cases
#' cases <- EpiNow2::example_confirmed[1:40]
#' 
#'  
#' ## Set up example generation time
#' generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
#'                         mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
#'                         sd = EpiNow2::covid_generation_times[1, ]$sd,
#'                         sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
#'                         max = 30)
#' ## Set                   
#' incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
#'                           mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
#'                           sd = EpiNow2::covid_incubation_period[1, ]$sd,
#'                           sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
#'                           max = 30)
#'                    
#' reporting_delay <- list(mean = log(5),
#'                         mean_sd = log(2),
#'                         sd = log(2),
#'                         sd_sd = log(1.5),
#'                         max = 30)
#'                         
#' ## Run model
#' out <- EpiNow2::estimate_infections(cases, family = "negbin",
#'                                     generation_time = generation_time,
#'                                     delays = list(incubation_period, reporting_delay),
#'                                     samples = 1000, warmup = 200, cores = 4, chains = 4,
#'                                     horizon = 7, estimate_rt = TRUE, verbose = TRUE)
#'                             
#'                      
#' ## Plot infections
#' plots <- report_plots(summarised_estimates = out$summarised,
#'                       reported = cases)
#'                       
#' plots
#' }
report_plots <- function(summarised_estimates, reported,
                         target_folder, max_plot = 10) {
  
  ## set input to data.table
  summarised_estimates <- data.table::setDT(summarised_estimates)
  reported <- data.table::setDT(reported)
  
  if (missing(target_folder)) {
    target_folder <- NULL
  }
  
# Infections plot ---------------------------------------------------------

infections <- plot_estimates(estimate = summarised_estimates[variable == "infections"],
                             reported = reported,
                             ylab = "Cases by \n date of infection",
                             max_plot = max_plot)
  

if (!is.null(target_folder)) {
  suppressWarnings(
    suppressMessages(
      ggplot2::ggsave(paste0(target_folder, "/infections_plot.png"),
                      infections,
                      width = 12,
                      height = 3,
                      dpi = 320)
    ))
}

# Cases by report ---------------------------------------------------------

reports <- plot_estimates(estimate = summarised_estimates[variable == "reported_cases"],
                          reported = reported, ylab = "Cases by \n date of report",
                          max_plot = max_plot)

if (!is.null(target_folder)) {
  suppressWarnings(
    suppressMessages(
      ggplot2::ggsave(paste0(target_folder, "/reported_plot.png"),
                      reports,
                      width = 12,
                      height = 3,
                      dpi = 320)
    ))
}


# R plot ------------------------------------------------------------------

reff <- plot_estimates(estimate = summarised_estimates[variable == "R"],
                       ylab = "Effective \n reproduction no.", hline = 1)


if (!is.null(target_folder)) {
  suppressWarnings(
    suppressMessages(
      ggplot2::ggsave(paste0(target_folder, "/reff_plot.png"),
                      reff,
                      width = 12,
                      height = 3,
                      dpi = 320)
    ))
}

# r plot ------------------------------------------------------------------

growth_rate <- plot_estimates(estimate = summarised_estimates[variable == "growth_rate"],
                              ylab = "Growth rate", hline = 0)


if (!is.null(target_folder)) {
  suppressWarnings(
    suppressMessages(
      ggplot2::ggsave(paste0(target_folder, "/growth_rate_plot.png"),
                      growth_rate,
                      width = 12,
                      height = 3,
                      dpi = 320)
    ))
}


# Summary plot ------------------------------------------------------------

  
  summary <- suppressWarnings(
    suppressMessages(
        reports +
          ggplot2::theme(legend.position = "none") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::labs(tag = "A") +
        infections +
          ggplot2::theme(legend.position = "none") + 
          ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          ) +
          ggplot2::labs(tag = "B") +
        reff +
          ggplot2::labs(tag = "C") +
        patchwork::plot_layout(ncol = 1) &
        ggplot2::scale_x_date(date_breaks = "1 week",
                              date_labels = "%b %d",
                              limits = c(min(summarised_estimates[variable == "R"]$date),
                                         max(summarised_estimates[variable == "R"]$date)))
    ))
  
  if (!is.null(target_folder)) {
    suppressWarnings(
      suppressMessages(
        ggplot2::ggsave(paste0(target_folder, "/summary_plot.png"),
                        summary,
                        width = 12,
                        height = 12,
                        dpi = 320)
      ))
  }
  
  
  ## Organise output
  plots <- list(
    infections = infections,
    reports = reports,
    reff = reff,
    growth_rate = growth_rate,
    summary = summary
  )
  
  return(plots)
}
