library(EpiNow2)
library(data.table)
library(ggplot2)

old_opts <- options()
options(mc.cores = 4)

# set up example generation time
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
# set delays between infection and case report
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
reporting_delay <- list(
  mean = convert_to_logmean(2, 1), mean_sd = 0.1,
  sd = convert_to_logsd(2, 1), sd_sd = 0.1, max = 15
)

# fit model to data to recover realistic parameter estimates and define settings
# shared simulation settings
init <- estimate_infections(example_confirmed[1:100],
  generation_time = generation_time,
  delays = delay_opts(incubation_period, reporting_delay),
  rt = rt_opts(prior = list(mean = 2, sd = 0.1), rw = 14),
  gp = NULL, horizon = 0,
  obs = obs_opts(scale = list(mean = 0.1, sd = 0.025))
)


# Rt scenario
R <- c(
  rep(2, 20), rep(0.5, 10), rep(1, 10), 1 + 0.04 * 1:10, rep(1.4, 5),
  1.4 - 0.02 * 1:20, rep(1.4, 10), rep(0.8, 5), 0.8 + 0.02 * 1:10
)
noisy_R <- R * rnorm(100, 1, 0.05)
# update Rt trajectory and simulate new infections using it
sims <- simulate_infections(init, R = noisy_R, samples = 10)
plot(sims)

sim_R <- sims$summarised[variable == "R"]$median

# pull out simulated cases
posterior_sample <- sims$samples[sample == 1]
sim_cases <- posterior_sample[variable == "reported_cases", .(date, confirm = value)]
sim_inf <- posterior_sample[variable == "infections"]$value

# plot Rt vs data
plot_rt <- function(est, R) {
  plot_estimates(
    estimate = est$summarised[variable == "R"],
    reported = data.table::data.table(
      date = est$summarised[variable == "R"]$date,
      confirm = R
    ),
    obs_as_col = FALSE,
    ylab = "Effective Reproduction No.",
    hline = 1
  )
}

save_ggplot <- function(plot, name) {
  ggplot2::ggsave(paste0("inst/dev/figs/", name, ".png"),
    plot,
    dpi = 300, width = 9, height = 6
  )
}

make_plot <- function(est, R, name) {
  plot <- plot_rt(est, R)
  save_ggplot(plot, name)
}

gp <- list()
backcalc <- list()
weekly_rw <- list()
daily_rw <- list()

fit_daily <- FALSE

for (method in c("nuts")) {
  if (method == "vb") {
    stanopts <- stan_opts(method = "vb", trials = 5, iter = 50000)
  } else {
    stanopts <- stan_opts(control = list(adapt_delta = 0.9))
  }

  # GP
  gp[[method]] <-
    estimate_infections(sim_cases,
      generation_time = generation_time,
      delays = delay_opts(incubation_period, reporting_delay),
      rt = rt_opts(prior = list(mean = 2, sd = 0.25)),
      stan = stanopts,
      obs = obs_opts(scale = list(mean = 0.1, sd = 0.025)),
      horizon = 0
    )
  # runtime ~ 5 minutes
  make_plot(gp[[method]], sim_R[8:100], paste("gp", method, sep = "_"))

  # Backcalculation
  backcalc[[method]] <-
    estimate_infections(sim_cases,
      generation_time = generation_time,
      delays = delay_opts(incubation_period, reporting_delay),
      rt = NULL,
      stan = stanopts,
      obs = obs_opts(scale = list(mean = 0.1, sd = 0.025)),
      horizon = 0
    )
  # runtime ~ 9 seconds
  make_plot(backcalc[[method]], R, paste("backcalc", method, sep = "_"))

  # RW (weekly)
  weekly_rw[[method]] <-
    estimate_infections(sim_cases,
      generation_time = generation_time,
      delays = delay_opts(incubation_period, reporting_delay),
      rt = rt_opts(
        prior = list(mean = 2, sd = 0.25),
        rw = 7
      ),
      gp = NULL,
      stan = stanopts,
      obs = obs_opts(scale = list(mean = 0.1, sd = 0.025)),
      horizon = 0
    )
  # runtime ~ 5 minutes
  make_plot(weekly_rw[[method]], R, paste("weekly_rw", method, sep = "_"))

  # RW
  if (fit_daily) {
    daily_rw[[method]] <-
      estimate_infections(sim_cases,
        generation_time = generation_time,
        delays = delay_opts(incubation_period, reporting_delay),
        rt = rt_opts(
          prior = list(mean = 2.5, sd = 0.25),
          rw = 1
        ),
        gp = NULL,
        stan = stanopts,
        obs = obs_opts(scale = list(mean = 0.1, sd = 0.025)),
        horizon = 0
      )
    # runtime ~ 10 minutes (with 40+ divergent transitions)
    make_plot(daily_rw[[method]], R, paste("daily_rw", method, sep = "_"))
  }
}

models <- c(gp, backcalc, weekly_rw)
model_names <-
  c(
    sapply(seq_along(gp), function(x) paste0("gp_", names(gp)[x])),
    sapply(seq_along(backcalc), function(x) paste0("backcalc_", names(backcalc)[x])),
    sapply(seq_along(weekly_rw), function(x) paste0("weekly_rw_", names(weekly_rw)[x]))
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

# library(dplyr)
# library(tidyr)
# library(rstan)
# library(loo)
# library(matrixStats)

# looic <- lapply(models, function(x) loo(x$fit, save_psis = TRUE))
# looic <- lapply(looic, function(x) x$estimates["looic", ])

# dl <- bind_rows(looic) %>%
#   mutate(
#     model = model_names,
#     model = sub("^(.*)_([^_]+)$", "\\1|\\2", model)
#   ) %>%
#   separate(model, c("model", "method"), sep = "\\|") %>%
#   arrange(Estimate)

# R_samples <- lapply(models, function(x) {
#   if ("R[1]" %in% names(x$fit)) {
#     rstan::extract(x$fit, "R")$R
#   } else {
#     rstan::extract(x$fit, "gen_R")$gen_R
#   }
# })
# inf_samples <- lapply(models, function(x) rstan::extract(x$fit, "infections")$infections)

# rcrps <- lapply(R_samples, function(x) if (!is.null(x)) scoringutils::crps(R, t(x)))
# names(rcrps) <- model_names
# icrps <- lapply(inf_samples, function(x) if (!is.null(x)) scoringutils::crps(sim_inf, t(x)))
# names(icrps) <- model_names

# rmeanae <- lapply(R_samples, function(x) if (!is.null(x)) abs(R - colMeans(x)))
# names(rmeanae) <- model_names
# imeanae <- lapply(inf_samples, function(x) if (!is.null(x)) abs(sim_inf - colMeans(x)))
# names(imeanae) <- model_names

# rmedianae <- lapply(R_samples, function(x) if (!is.null(x)) abs(R - colMedians(x)))
# names(rmedianae) <- model_names
# imedianae <- lapply(inf_samples, function(x) if (!is.null(x)) abs(sim_inf - colMedians(x)))
# names(imedianae) <- model_names

# rdf <- bind_rows(rcrps) %>%
#   mutate(metric = "CRPS") %>%
#   bind_rows(rmeanae) %>%
#   mutate(metric = if_else(is.na(metric), "AE of mean", metric)) %>%
#   bind_rows(rmedianae) %>%
#   mutate(metric = if_else(is.na(metric), "AE of median", metric)) %>%
#   mutate(time = rep(1:(n() / 3), times = 3)) %>%
#   pivot_longer(names_to = "model", c(-time, -metric))

# p <- ggplot(rdf, aes(x = time, y = value, colour = model)) +
#   geom_line() +
#   scale_colour_brewer("Model", palette = "Dark2") +
#   facet_wrap(~metric, ncol = 1, scale = "free_y") +
#   xlab("Time") +
#   ylab("Score") +
#   ggplot2::theme_bw() +
#   ggtitle("Reconstructing R")

# save_ggplot(p, "rscores")

# rdf %>%
#   select(-time) %>%
#   group_by(metric, model) %>%
#   summarise_all(mean) %>%
#   pivot_wider(names_from = "metric") %>%
#   arrange(CRPS)

# idf <- bind_rows(icrps) %>%
#   mutate(metric = "CRPS") %>%
#   bind_rows(imeanae) %>%
#   mutate(metric = if_else(is.na(metric), "AE of mean", metric)) %>%
#   bind_rows(imedianae) %>%
#   mutate(metric = if_else(is.na(metric), "AE of median", metric)) %>%
#   mutate(time = rep(1:(n() / 3), times = 3)) %>%
#   pivot_longer(names_to = "model", c(-time, -metric)) %>%
#   mutate(time = time - 7) %>%
#   filter(time >= 0)

# p <- ggplot(idf, aes(x = time, y = value, colour = model)) +
#   geom_line() +
#   scale_colour_brewer("Model", palette = "Dark2") +
#   facet_wrap(~metric, ncol = 1, scale = "free_y") +
#   xlab("Time") +
#   ylab("Score") +
#   ggplot2::theme_bw() +
#   ggtitle("Reconstructing infections")

# save_ggplot(p, "iscores")

# idf %>%
#   select(-time) %>%
#   group_by(metric, model) %>%
#   summarise_all(mean) %>%
#   pivot_wider(names_from = "metric") %>%
#   arrange(CRPS)

options(old_opts)