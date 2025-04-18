##' Create a benchmark profile
##'
##' This runs the `estimate_infections` function using a given stan model file
##' multiple times with given seeds and extracts the `cmdstanr` profiling
##' information each time.
##' @param dir directory that contains the stan model file
##' @param seeds a vector of random seeds to use; this determines how often
##'   `estimate_infections` is run
##' @return a data.table of profile informations, with the run id given as
##'   `iter`
create_profiles <- function(dir = file.path("inst", "stan"),
                            seeds = sample(.Machine$integer.max, 1)) {
  compiled_model <- EpiNow2:::epinow2_cmdstan_model(dir = dir)
  profiles <- suppressMessages(purrr::map(seeds, \(x) {
    set.seed(x)
    fit <- estimate_infections(
      data = reported_cases,
      generation_time = gt_opts(fixed_generation_time),
      delays = delay_opts(delays),
      rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.2)),
      stan = stan_opts(
        samples = 1000, chains = 2, object = compiled_model,
        cores = 2
      ),
      verbose = FALSE
    )
    df <- as.data.table(rbindlist(fit$fit$profiles(), idcol = "chain"))

    return(df)
  }))
  return(data.table::rbindlist(profiles, idcol = "iter"))
}
##' Calculate bootstrap mean and credible intervals
##'
##' Credible intervals are calculated from resampled quantiles
##' @param x numeric vector
##' @param n_boot number of bootstrap iterations; if NULL (default) will take
##'   length of x
##' @return a `data.table` with one row, containing the mean, 50% credible
##'   intervals (`low`/`high`) and 90% credible intervals (`lower`/`higher`)
bootci <- function(x, n_boot = NULL) {
  if (is.null(n_boot)) n_boot <- length(x)
  m <- matrix(sample(x, n_boot * length(x), replace = TRUE), n_boot, length(x))
  means <- apply(m, 1, mean)
  dt <- data.table::data.table(
    mean = mean(x),
    low = quantile(means, 0.25),
    high = quantile(means, 0.75),
    lower = quantile(means, 0.05),
    higher = quantile(means, 0.95),
    lowest = range(means)[1],
    highest = range(means)[2]
  )
  return(list(dt))
}
