## to be run following rt.R
library(data.table)
library(scoringutils)
library(ggplot2)
library(rstan)
source(here::here("inst", "dev", "recover-synthetic", "plot.R"))

synthetic <- readRDS("synthetic.rds")

looic <- lapply(synthetic$models, function(x) {
  est <- loo(x$fit, save_psis = TRUE)$estimates
  dt <- data.table(est, keep.rownames = TRUE)
  setnames(dt, "rn", "metric")
  return(dt)
})
names(looic) <- synthetic$model_names

dl <- rbindlist(looic, idcol = "model")
dl <- dl[metric == "looic"]
dl[, model := sub("^(.*)_([^_]+)$", "\\1|\\2", model)]
dl[, c("model", "moethod") := tstrsplit(model, "\\|")]

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

rdf <- as.data.table(rcrps)
rdf[, metric := "CRPS"]
rdf[, time := 1:.N]
rdf <- melt(rdf, id.vars = c("metric", "time"), variable.name = "model")

p <- ggplot(rdf, aes(x = time, y = value, colour = model)) +
  geom_line() +
  scale_colour_brewer("Model", palette = "Dark2") +
  xlab("Time") +
  ylab("CRPS") +
  ggplot2::theme_bw() +
  ggtitle("Reconstructing R")
save_ggplot(p, "CRPS", "rt")

rmeans <- rdf[, list(mean_crps = mean(value)), by = c("metric", "model")]
setorder(rmeans, mean_crps)
rmeans

idf <- as.data.table(icrps)
idf[, metric := "CRPS"]
idf[, time := 1:.N]
idf <- melt(idf, id.vars = c("metric", "time"), variable.name = "model")

p <- ggplot(idf, aes(x = time, y = value, colour = model)) +
  geom_line() +
  scale_colour_brewer("Model", palette = "Dark2") +
  xlab("Time") +
  ylab("CRPS") +
  ggplot2::theme_bw() +
  ggtitle("Reconstructing infections")
save_ggplot(p, "CRPS", "inf")

imeans <- idf[, list(mean_crps = mean(value)), by = c("metric", "model")]
setorder(imeans, mean_crps)
imeans
