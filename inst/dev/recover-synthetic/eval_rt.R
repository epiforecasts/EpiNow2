## to be run following rt.R
library(dplyr)
library(loo)
library(tidyr)
library(scoringutils)
library(ggplot2)
source(here::here("inst", "dev", "recover-synthetic", "plot.R"))

synthetic <- readRDS("synthetic.rds")

looic <- lapply(synthetic$models, function(x) {
  est <- loo(x$fit, save_psis = TRUE)$estimates
  return(as_tibble(est, rownames = "metric"))
})
names(looic) <- synthetic$model_names

dl <- bind_rows(looic, .id = "model") %>%
  filter(metric == "looic") %>%
  mutate(
    model = sub("^(.*)_([^_]+)$", "\\1|\\2", model)
  ) %>%
  separate(model, c("model", "method"), sep = "\\|")

R_samples <- lapply(synthetic$models, function(x) {
  if ("R[1]" %in% names(x$fit)) {
    extract(x$fit, "R")$R
  } else {
    extract(x$fit, "gen_R")$gen_R
  }
})
inf_samples <- lapply(synthetic$models, function(x) {
  extract(x$fit, "infections")$infections
})

calc_crps <- function(x, truth) {
  len <- min(ncol(x), length(truth))
  reduced_truth <- tail(truth, len)
  reduced_x <- tail(t(x), len)
  return(crps_sample(reduced_truth, reduced_x))
}

rcrps <- lapply(R_samples, calc_crps, truth = synthetic$truth$R)
names(rcrps) <- synthetic$model_names
icrps <- lapply(inf_samples, calc_crps, truth = synthetic$truth$sim_inf)
names(icrps) <- synthetic$model_names

rdf <- bind_rows(rcrps) %>%
  mutate(metric = "CRPS") %>%
  mutate(time = 1:n()) %>%
  pivot_longer(names_to = "model", c(-time, -metric))

p <- ggplot(rdf, aes(x = time, y = value, colour = model)) +
  geom_line() +
  scale_colour_brewer("Model", palette = "Dark2") +
  xlab("Time") +
  ylab("CRPS") +
  ggplot2::theme_bw() +
  ggtitle("Reconstructing R")
save_ggplot(p, "CRPS", "rt")

rdf %>%
  select(-time) %>%
  group_by(metric, model) %>%
  summarise_all(mean) %>%
  pivot_wider(names_from = "metric") %>%
  arrange(CRPS)

idf <- bind_rows(icrps) %>%
  mutate(metric = "CRPS") %>%
  mutate(time = 1:n()) %>%
  pivot_longer(names_to = "model", c(-time, -metric))

p <- ggplot(idf, aes(x = time, y = value, colour = model)) +
  geom_line() +
  scale_colour_brewer("Model", palette = "Dark2") +
  xlab("Time") +
  ylab("CRPS") +
  ggplot2::theme_bw() +
  ggtitle("Reconstructing infections")
save_ggplot(p, "CRPS", "inf")

idf %>%
  select(-time) %>%
  group_by(metric, model) %>%
  summarise_all(mean) %>%
  pivot_wider(names_from = "metric") %>%
  arrange(CRPS)

