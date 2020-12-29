library("EpiNow2")
library("rstan")
library("dplyr")
library("ggplot2")
library("here")

# set number of cores to use
options(mc.cores = ifelse(interactive(), 4, 1))
# get example case counts
reported_cases <- example_confirmed[1:60]

# set up example generation time
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
# set delays between infection and case report 
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
reporting_delay <- list(mean = convert_to_logmean(3, 1), mean_sd = 0.1,
                        sd = convert_to_logsd(3, 1), sd_sd = 0.1, max = 10)
      
# default setting 
# here we assume that the observed data is truncated by the same delay as 
test <- c("infections", "gr", "gr_t-1", "R0", "R_t-1")

results <- lapply(test, function(x) {
  estimate_infections(reported_cases, generation_time = generation_time,
                      delays = delay_opts(incubation_period, reporting_delay),
                      rt = rt_opts(prior = list(mean = 2, sd = 0.1), gp_on = x),
                      stan = stan_opts(control = list(adapt_delta = 0.95)))			
})

fits <- lapply(results, function(x) x$fit)

times <- lapply(fits, get_elapsed_time)
times_tibbles <- lapply(times, as_tibble)
elapsed_times <- bind_rows(times_tibbles, .id = "gp_on") %>%
	mutate(total = warmup + sample) %>%
	group_by(gp_on) %>%
	summarise(mean_time = mean(total), .groups = "drop") %>%
	arrange(mean_time)

divergent <- lapply(fits, get_divergent_iterations)
divergent_tibbles <- lapply(divergent, as_tibble)
divergent_iterations <- bind_rows(divergent_tibbles, .id = "gp_on") %>%
	group_by(gp_on) %>%
	summarise(divergent_transitions = sum(value), .groups = "drop")

ease_of_fit <- elapsed_times %>%
	left_join(divergent_iterations, by = "gp_on") %>%
	arrange(divergent_transitions, mean_time)

summaries <- lapply(results, function(x) x$summarised)
R <- bind_rows(summaries, .id = "gp_on") %>%
	filter(variable == "R")

p <- ggplot(R, aes(x = date, y = median, ymin = lower_90, ymax = upper_90, 
	      colour = gp_on, fill = gp_on, group = interaction(gp_on, type))) +
  geom_line() + 
  geom_ribbon(linetype = 0, alpha = 0.2) + 
  xlab("") +
  ylab("R") +
  scale_colour_brewer("GP prior", palette = "Set1") +
  scale_fill_brewer("GP prior", palette = "Set1") +
  theme_minimal()

ggsave(here::here("inst", "dev", "figs", "compare_gp_r.png"), p)

cases <- bind_rows(summaries, .id = "gp_on") %>%
	        filter(variable == "reported_cases")

p <- ggplot(cases, aes(x = date)) +
  geom_line(aes(y = median, colour = gp_on, group = interaction(gp_on, type))) +
  geom_ribbon(linetype = 0, alpha = 0.2, 
	      aes(ymin = lower_90, ymax = upper_90, fill = gp_on, 
		  group = interaction(gp_on, type))) +
  geom_col(data = example_confirmed[1:67], aes(y = confirm), alpha = 0.3) +
  xlab("") +
  ylab("R") +
  scale_colour_brewer("GP prior", palette = "Set1") +
  scale_fill_brewer("GP prior", palette = "Set1") +
  theme_minimal()

ggsave(here::here("inst", "dev", "figs", "compare_gp_cases.png"), p)
