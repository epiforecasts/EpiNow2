#' Simulate infections using a given trajectory of the time-varying reproduction number
#'
#' @description This function simulates infections using an existing fit to observed cases but with a modified time-varying reproduction number. This can be used to explore forecast models or past counterfactuals.
#' @param estimates The \code{estimates} element of an \code{epinow} run that has been done with output = "fit", or the result of \code{estimate_infections} with \code{return_fit} set to TRUE.
#' @param R A numeric vector of reproduction numbers; these will overwrite the reproduction numbers contained in \code{estimates}, except elements set to NA. If it is longer than the time series of reproduction numbers contained in \code{estimates}, the values going beyond the length of estimated reproduction numbers are taken as forecast.
#' @importFrom rstan extract sampling
#' @export
#' @examples
#' \donttest{
#' # get example case counts
#' reported_cases <- EpiNow2::example_confirmed[1:50]
#'
#' # set up example generation time
#' generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
#' # set delays between infection and case report
#' incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
#' reporting_delay <- list(mean = log(3), mean_sd = 0.1,
#'                         sd = log(1), sd_sd = 0.1, max = 15)
#'
#' est <- estimate_infections(reported_cases, generation_time = generation_time,
#'                            delays = list(incubation_period, reporting_delay),
#'                            stan_args = list(warmup = 200,
#'                             control = list(adapt_delta = 0.95, max_treedepth = 15),
#'                             cores = ifelse(interactive(), 4, 1)),
#'                            verbose = interactive())
#'
#' R <- c(rep(NA_real_, 40), rep(0.5, 17))
#' sims <- simulate_infections(est, R)
#' plot(sims)
#' }
simulate_infections <- function(estimates,
                                R = NULL,
                                model = NULL,
                                verbose = interactive()) {
  ## extract samples from given stanfit object
  samples <- rstan::extract(estimates$fit)
  
  ## if R is given, update trajectories in stanfit object
  if (!is.null(R)) {
    R_mat <- matrix(rep(R, each = dim(samples$R)[1]),
                    ncol = length(R), byrow = FALSE)
    samples$R[!is.na(R_mat)] <- R_mat[!is.na(R_mat)]
  }
  nsamples <- dim(samples$R)[1]
  
  ## prepare data for stan command
  data <- c(list(n = nsamples), samples, estimates$args)
  
  ## simulate
  if (is.null(model)) {
    model <- stanmodels$simulate_infections
  }
  sims <- rstan::sampling(object = model,
                          data = data, chains = 1, iter = 1,
                          algorithm = "Fixed_param",
                          refresh = ifelse(verbose, 50, 0))
  
  ## extract parameters for extract_parameter_samples from passed stanfit object
  mean_shift <- estimates$args$seeding_time
  reported_inf_dates <- na.omit(unique(estimates$samples$date))

  out <- extract_parameter_samples(sims, data,
                                   reported_inf_dates = reported_inf_dates,
                                   reported_dates = reported_inf_dates[-(1:mean_shift)],
                                   drop_length_1 = TRUE, merge = TRUE)

  ## extract parameters for format_fit from passed stanfit object
  horizon <- estimates$args$horizon
  burn_in <- as.integer(min(reported_inf_dates) + mean_shift - min(estimates$observations$date))
  start_date <- as.integer(min(reported_inf_dates) - mean_shift)
  CrIs <- extract_CrIs(estimates$summarised) / 100

  format_out <- format_fit(posterior_samples = out,
                           horizon = horizon,
                           shift = mean_shift,
                           burn_in = burn_in,
                           start_date = start_date,
                           CrIs = CrIs)

  format_out$observations <- estimates$observations
  class(format_out) <- c("estimate_infections", class(format_out))
  return(format_out)
}
