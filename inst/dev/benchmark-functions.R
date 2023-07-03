##' Create a benchmark profile
##'
##' This runs the `estimate_infections` function using a given stan model file
##' multiple times with given seeds and extracts the `cmdstanr` profiling
##' information each time.
##' @param model name of a model file for estimating infections in `inst/stan`
##' @param seeds a vector of random seeds to use; this determines how often
##'   `estimate_infections` is run
##' @return a data frame of profile informations, with the run id given as
##'   `iter`
create_profiles <- function(model, seeds = sample(.Machine$integer.max, 1)) {
  compiled_model <- EpiNow2:::epinow2_model(model = model, force_recompile = TRUE)
  profiles <- purrr::map(seeds, \(x) {
    set.seed(x)
    fit <- estimate_infections(
      reported_cases = reported_cases,
      generation_time = generation_time,
      delays = delays,
      rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
      stan = stan_opts(
        samples = 1000, chains = 1, model_options = list(model = model)
      )
    )
    return(fit$fit$profiles())
  })
  return(dplyr::bind_rows(profiles, .id = "iter"))
}
##' Calculate bootstrap mean and credible intervals
##'
##' Credible intervals are calculated from resampled quantiles
##' @param x numeric vector
##' @param n_boot number of bootstrap iterations
##' @return a `tibble` with one row, containing the mean, 90% credible intervals
##'  (`low`/`high`) and 99% credible intervals (`lower`/`higher`)
bootci <- function(x, n_boot) {
  m <- matrix(sample(x, n_boot * length(x), replace = TRUE), n_boot, length(x))
  means <- apply(m, 1, mean)
  return(list(tibble::tibble(
    mean = mean(x), 
    low = quantile(means, 0.05), 
    high = quantile(means, 0.95),
    lower = quantile(means, 0.005),
    higher = quantile(means, 0.995)
  )))
}


