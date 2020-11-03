#' Simulate infections using a given trajectory of the time-varying reproduction number
#'
#' @description This function simulates infections using an existing fit to observed cases but
#'  with a modified time-varying reproduction number. This can be used to explore forecast models
#'   or past counterfactuals. Simulations can be run in parallel using `future::plan`.
#' @param estimates The \code{estimates} element of an \code{epinow} run that has been done with 
#' output = "fit", or the result of \code{estimate_infections} with \code{return_fit} set to TRUE.
#' @param R A numeric vector of reproduction numbers; these will overwrite the reproduction numbers
#'  contained in \code{estimates}, except elements set to NA. If it is longer than the time series 
#'  of reproduction numbers contained in \code{estimates}, the values going beyond the length of 
#'  estimated reproduction numbers are taken as forecast.
#' @param samples Numeric, number of posterior samples to simulate from. The default is to use all
#' samples in the `estimates` input.
#' @param batch_size Numeric, defaults to 100. Size of batches in which to simulate. May decrease 
#' runtimes due to reduced IO costs. If set to NULL then all simulations are done at once.
#' @importFrom rstan extract sampling
#' @importFrom purrr transpose map
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor
#' @importFrom data.table rbindlist
#' @inheritParams estimate_infections
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
#' # fit model to data to recover Rt estimates
#' est <- estimate_infections(reported_cases, generation_time = generation_time,
#'                            delays = list(incubation_period, reporting_delay),
#'                            stan_args = 
#'                              list(warmup = 200, 
#'                                   control = list(adapt_delta = 0.95, max_treedepth = 15),
#'                                   cores = ifelse(interactive(), 4, 1)))
#'                                   
#' # update Rt trajectory and simulate new infections using it
#' R <- c(rep(NA_real_, 40), rep(0.5, 17))
#' sims <- simulate_infections(est, R)
#' plot(sims)
#' }
simulate_infections <- function(estimates,
                                R = NULL,
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
                          pars = c("noise", "eta", "lp__", "infections",
                                   "reports", "imputed_reports", "r"),
                          include = FALSE)
  
  ## if R is given, update trajectories in stanfit object
  if (!is.null(R)) {
    R_mat <- matrix(rep(R, each = dim(draws$R)[1]),
                    ncol = length(R), byrow = FALSE)
    draws$R[!is.na(R_mat)] <- R_mat[!is.na(R_mat)]
  }
  
  # set samples if missing
  R_samples <- dim(draws$R)[1]
  if (is.null(samples)) {
    samples <- R_samples
  }else if(samples > R_samples) {
    samples <- R_samples
  }
  
  # extract parameters for extract_parameter_samples from passed stanfit object
  shift <- estimates$args$seeding_time
  dates <- na.omit(unique(estimates$samples$date))
  
  # Load model
  if (is.null(model)) {
    model <- stanmodels$simulate_infections
  }
  
  ## set up batch simulation
  batch_simulate <- function(estimates, draws, model,
                             shift, dates, nstart, nend) {
    # extract batch samples from draws
    draws <- purrr::map(draws, ~ as.matrix(.[nstart:nend, ]))
    
    ## prepare data for stan command
    data <- c(list(n = dim(draws$R)[1]), draws, estimates$args)
    
    ## simulate
    sims <- rstan::sampling(object = model,
                            data = data, chains = 1, iter = 1,
                            algorithm = "Fixed_param",
                            refresh = 0)
    
    out <- extract_parameter_samples(sims, data,
                                     reported_inf_dates = dates,
                                     reported_dates = dates[-(1:shift)],
                                     drop_length_1 = TRUE, merge = TRUE)
    return(out)
  }
  
  ## set up batching
  if (!is.null(batch_size)) {
    batch_no <- ceiling(samples / batch_size)
    nstarts <- seq(1, by = batch_size, length.out = batch_no)
    nends <- c(seq(batch_size, by = batch_size, length.out = batch_no - 1), samples)
    batches <- purrr::transpose(list(nstarts, nends))
  }else{
    batches <- list(list(1, samples))
  }


  ## simulate in batches
  progressr::with_progress({
    if (verbose) {
      p <- progressr::progressor(along = batches)
    }

  out <- future.apply::future_lapply(batches, 
                     function(batch) {
                       if (verbose) {
                         p()
                       }
                       batch_simulate(estimates, draws, model,
                                      shift, dates, batch[[1]], 
                                      batch[[2]])},
                     future.seed = TRUE)
  })
  
  ## join batches
  out <- purrr::transpose(out)
  out <- purrr::map(out, ~ data.table::rbindlist(.)[, sample := 1:.N])
  
  ## extract parameters for format_fit from passed stanfit object
  horizon <- estimates$args$horizon
  burn_in <- as.integer(min(dates) + shift - min(estimates$observations$date))
  start_date <- as.integer(min(dates) - shift)
  CrIs <- extract_CrIs(estimates$summarised) / 100

  format_out <- format_fit(posterior_samples = out,
                           horizon = horizon,
                           shift = shift,
                           burn_in = burn_in,
                           start_date = start_date,
                           CrIs = CrIs)

  format_out$observations <- estimates$observations
  class(format_out) <- c("estimate_infections", class(format_out))
  return(format_out)
}
