library("data.table")
library("EpiNow2")
library("knitr")

## number of times to run estimate_infections
n_iter <- 15

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
][, list(
  mean = round(mean(change)),
  min = min(change),
  max = max(change)
), by = "operation"
][, list(
  mean = mean,
  range = paste0("(", min, ", ", max, ")"),
  trend = fcase(
    min > 0, "slowdown",
    max < 0, "speedup",
    default = "no change"
  )
), by = "operation"
]

if (any(changes$trend != "no change")) {
  format_summary <- merge(
    summary_means,
    changes[, .(operation, mean, range, trend)],
    by = "operation"
  )

  setorder(format_summary, -main)
  format_summary <- format_summary[, lapply(.SD, as.character)]
  setnames(format_summary, "mean", "% change")

  sink(file = file.path("inst", "dev", "benchmark-results.md"))
  knitr::kable(
    format_summary, align = "lrrrr",
    caption = "Benchmarking results (mean time in seconds)."
  )
}
