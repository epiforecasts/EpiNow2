library(EpiNow2)
library(data.table)
library(ggplot2)
source(here::here("inst", "dev", "recover-synthetic", "plot.R"))

old_opts <- options()
options(mc.cores = 4)

# set up example generation time
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
# set delays between infection and case report
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
reporting_delay <- dist_spec(
  mean = convert_to_logmean(2, 1), mean_sd = 0.1,
  sd = convert_to_logsd(2, 1), sd_sd = 0.1, max = 15
)

obs <- obs_opts(scale = list(mean = 0.1, sd = 0.025), return_likelihood = TRUE)

# fit model to data to recover realistic parameter estimates and define settings
# shared simulation settings
init <- estimate_infections(example_confirmed[1:100],
  generation_time = generation_time_opts(generation_time),
  delays = delay_opts(incubation_period + reporting_delay),
  rt = rt_opts(prior = list(mean = 2, sd = 0.1), rw = 14),
  gp = NULL, horizon = 0,
  obs = obs
)

# Rt scenario
R <- c(
  rep(2, 40), rep(0.5, 10), rep(1, 10), 1 + 0.04 * 1:20, rep(1.4, 5),
  1.4 - 0.02 * 1:20, rep(1.4, 10), rep(0.8, 5), 0.8 + 0.02 * 1:20
)
noisy_R <- R * rnorm(length(R), 1, 0.05)
# update Rt trajectory and simulate new infections using it
sims <- simulate_infections(init, R = noisy_R, samples = 10)

sim_R <- sims$summarised[variable == "R"]$median

# pull out simulated cases
posterior_sample <- sims$samples[sample == 1]
sim_cases <- posterior_sample[variable == "reported_cases", .(date, confirm = value)]
sim_inf <- posterior_sample[variable == "infections"]$value

save_ggplot(plot(sims), "sims")

gp <- list()
backcalc <- list()
weekly_rw <- list()
daily_rw <- list()
gp_rw <- list()

fit_daily <- FALSE

for (method in c("nuts")) {
  if (method == "vb") {
    stanopts <- stan_opts(method = "vb", trials = 5, iter = 50000)
  } else {
    stanopts <- stan_opts(adapt_delta = 0.9)
  }

  # GP
  gp[[method]] <-
    estimate_infections(sim_cases,
      generation_time = generation_time_opts(generation_time),
      delays = delay_opts(incubation_period + reporting_delay),
      rt = rt_opts(prior = list(mean = 2, sd = 0.25)),
      stan = stanopts,
      obs = obs,
      horizon = 0
    )
  # runtime ~ 10 minutes
  make_plots(
   gp[[method]], R, sim_inf, paste("gp", method, sep = "_")
  )

  # Backcalculation
  backcalc[[method]] <-
    estimate_infections(sim_cases,
      generation_time = generation_time_opts(generation_time),
      delays = delay_opts(incubation_period + reporting_delay),
      rt = NULL,
      stan = stanopts,
      obs = obs,
      horizon = 0
    )
  # runtime ~ 15 seconds
  make_plots(
    backcalc[[method]], R, sim_inf, paste("backcalc", method, sep = "_")
  )

  # RW (weekly)
  weekly_rw[[method]] <-
    estimate_infections(sim_cases,
      generation_time = generation_time_opts(generation_time),
      delays = delay_opts(incubation_period + reporting_delay),
      rt = rt_opts(
        prior = list(mean = 2, sd = 0.25),
        rw = 7
      ),
      gp = NULL,
      stan = stanopts,
      obs = obs,
      horizon = 0
    )
  # runtime ~ 5 minutes
  make_plots(
    weekly_rw[[method]], R, sim_inf, paste("weekly_rw", method, sep = "_")
  )

  # RW (every month) + stationary Guassian process
  gp_rw[[method]] <-
    estimate_infections(sim_cases,
      generation_time = generation_time_opts(generation_time),
      delays = delay_opts(incubation_period + reporting_delay),
      rt = rt_opts(
        prior = list(mean = 2, sd = 0.25), rw = 14, gp_on = "R0"
      ),
      stan = stanopts,
      obs = obs,
      horizon = 0
    )

  # runtime ~ 10 minutes
  make_plots(
    gp_rw[[method]], R, sim_inf, paste("gp_rw", method, sep = "_")
  )

  # Daily RW
  if (fit_daily) {
    daily_rw[[method]] <-
      estimate_infections(sim_cases,
        generation_time = generation_time_opts(generation_time),
        delays = delay_opts(incubation_period + reporting_delay),
        rt = rt_opts(
          prior = list(mean = 2, sd = 0.25),
          rw = 1
        ),
        gp = NULL,
        stan = stanopts,
        obs = obs,
        horizon = 0
      )
    # runtime ~ 10 minutes (with 40+ divergent transitions)
    make_plots(
      daily_rw[[method]], R, sim_inf, paste("daily_rw", method, sep = "_")
    )
  }
}

models <- c(gp, backcalc, weekly_rw, gp_rw)
model_names <-
  c(
    sapply(seq_along(gp), function(x) paste0("gp_", names(gp)[x])),
    sapply(seq_along(backcalc), function(x) paste0("backcalc_", names(backcalc)[x])),
    sapply(seq_along(weekly_rw), function(x) paste0("weekly_rw_", names(weekly_rw)[x])),
    sapply(seq_along(gp_rw), function(x) paste0("gp_rw_", names(gp_rw)[x]))
  )

if (fit_daily) {
  models <- c(models, daily_rw)
  models_names <- c(model_names,
    sapply(seq_along(daily_rw), function(x) paste0("daily_rw_", names(daily_rw)[x]))
  )
}
saveRDS(list(
  truth = list(
    noisy_R = noisy_R, R = R, sim_cases = sim_cases,
    sim_inf = sim_inf
  ),
  models = models, model_names = model_names
),
file = "synthetic.rds",
)

options(old_opts)
