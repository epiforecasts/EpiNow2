library("data.table")
library("EpiNow2")
library("knitr")

## number of times to run estimate_infections
n_iter <- 10

## random seeds
seeds <- sample(.Machine$integer.max, n_iter)

source(file.path("touchstone", "setup.R"))
source(file.path("inst", "dev", "benchmark-functions.R"))

profiles <- list()

## generate profiles for different versions of the stan model
profiles[["branch"]] <- create_profiles(file.path("inst", "stan"), seeds)
profiles[["main"]] <- create_profiles(file.path("inst", "stan-main"), seeds)

## merge profiles from the two chains into one, round total time and only keep
## name and total_time columns; then average across chains; sort by time from
## longest to shortest
summary <- rbindlist(profiles, idcol = "branch")
setnames(summary, "name", "operation")

summary_ci <- summary[, rbindlist(bootci(total_time)), by = .(branch, operation)]
setorder(summary_ci, -mean)
## print
summary_ci <- summary_ci[,
  lapply(.SD, signif, 2), by = .(branch, operation)
]
summary_ci[, lapply(.SD, as.character)]
summary_means <- dcast(
  summary_ci, operation ~ branch, value.var = "mean"
)

## now calculate the percentage change
format_summary <- dcast(
  summary, operation + iter + chain ~ branch, value.var = "total_time"
)
changes <- format_summary[,
  change := round((branch - main) / branch * 100)
][,
  rbindlist(bootci(change)), by = .(operation)
][,
  mean := round(mean)
][,
  ci := paste0("(", round(lower), ", ", round(higher), ")")
]

format_summary <- merge(
  summary_means,
  changes[, .(operation, mean, ci)],
  by = "operation"
)

setorder(format_summary, -main)
format_summary <- format_summary[, lapply(.SD, as.character)]
setnames(format_summary, c("mean", "ci"), c("% change", "90% CI"))

sink(file = file.path("inst", "dev", "benchmark-results.md"))
knitr::kable(
  format_summary, align = "lrrrr",
  caption = "Benchmarking results (mean time in seconds)."
)
