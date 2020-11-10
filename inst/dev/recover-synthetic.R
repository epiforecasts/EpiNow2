library(EpiNow2)
library(data.table)
library(ggplot2)

options(mc.cores = 4)

# set up example generation time
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
# set delays between infection and case report
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
reporting_delay <- list(mean = convert_to_logmean(3, 1), mean_sd = 0.1,
                        sd = convert_to_logsd(3, 1), sd_sd = 0.1, max = 15)

# fit model to data to recover realistic parameter estimates and define settings
# shared simulation settings
init <- estimate_infections(example_confirmed[1:100], 
                            generation_time = generation_time,
                            delays = delay_opts(incubation_period, reporting_delay),
                            rt = rt_opts(prior = list(mean = 2, sd = 0.1)),
                            gp = gp_opts(ls_min = 10, boundary_scale = 1.5,
                                         basis_prop = 0.05), horizon = 0,
                            obs = obs_opts(scale = list(mean = 0.1, sd = 0.025)))


# Rt scenario
R <- c(rep(2, 20), rep(0.5, 10), rep(1, 10), 1 + 0.04 * 1:10, rep(1.4, 5), 
       1.4 - 0.02 * 1:20, rep(1.4, 10), rep(0.8, 5), 0.8 + 0.02 * 1:10)
noisy_R <- R * rnorm(100, 1, 0.05)
# update Rt trajectory and simulate new infections using it
sims <- simulate_infections(init, noisy_R, samples = 10)
plot(sims)

# pull out simulated cases
posterior_sample <- sims$samples[sample == 1]
sim_cases <- posterior_sample[variable == "reported_cases", .(date, confirm = value)]

# plot Rt vs data
plot_rt <- function(est, R) {
  plot_estimates(estimate = est$summarised[variable == "R"],
                 reported = data.table::data.table(date = est$summarised[variable == "R"]$date,
                                                   confirm = R),
                 obs_as_col = FALSE,
                 ylab = "Effective Reproduction No.",
                 hline = 1)
}

save_ggplot <- function(plot, name) {
  ggplot2::ggsave(paste0("inst/dev/figs/", name, ".png"),
                  plot, dpi = 300, width = 9, height = 6)
}

make_plot <- function(est, R, name) {
  plot <- plot_rt(est, R)
  save_ggplot(plot, name)
}

## GP
gp <- estimate_infections(sim_cases, generation_time = generation_time,
                          delays = delay_opts(incubation_period, reporting_delay),
                          rt = rt_opts(prior = list(mean = 2.5, sd = 0.25)),
                          gp = gp_opts(ls_min = 5, boundary_scale = 1.5,
                                       basis_prop = 0.2),
                          stan = stan_opts(control = list(adapt_delta = 0.9),
                                           warmup = 100),
                          horizon = 0)

make_plot(gp, R, "gp")

## Backcalculation
backcalc <- estimate_infections(sim_cases, generation_time = generation_time,
                                delays = delay_opts(incubation_period, reporting_delay),
                                rt = NULL,
                                gp = gp_opts(ls_min = 5, boundary_scale = 1.5,
                                             basis_prop = 0.1),
                                stan = stan_opts(control = list(adapt_delta = 0.9)),
                                horizon = 0)

make_plot(backcalc, R, "backcalc")

## RW (weekly)
weekly_rw <- estimate_infections(sim_cases, generation_time = generation_time,
                          delays = delay_opts(incubation_period, reporting_delay),
                          rt = rt_opts(prior = list(mean = 2.5, sd = 0.25),
                                       rw = 7),
                          gp = NULL,
                          stan = stan_opts(control = list(adapt_delta = 0.9)),
                          horizon = 0)

make_plot(weekly_rw, R, "weekly_rw")

## RW (every 2 days)
daily_rw <- estimate_infections(sim_cases, generation_time = generation_time,
                          delays = delay_opts(incubation_period, reporting_delay),
                          rt = rt_opts(prior = list(mean = 2.5, sd = 0.25),
                                       rw = 1),
                          gp = NULL,
                          stan = stan_opts(control = list(adapt_delta = 0.9)),
                          horizon = 0)


make_plot(daily_rw, R, "daily_rw")


