#!/usr/bin/env Rscript
##' Compare Stan profiling results between two versions
##'
##' This script reads profile CSV files from two versions and generates
##' a comparison report.
##'
##' Usage: Rscript benchmark-compare.R <main_profiles.csv> <branch_profiles.csv>
##'
##' Output: benchmark-results.md (if there are notable changes)

library("data.table")
library("knitr")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript benchmark-compare.R <main_profiles.csv> <branch_profiles.csv>")
}

main_file <- args[1]
branch_file <- args[2]
output_file <- if (length(args) >= 3) args[3] else "inst/dev/benchmark-results.md"

##' Calculate bootstrap mean and credible intervals
bootci <- function(x, n_boot = NULL) {
  if (is.null(n_boot)) n_boot <- length(x)
  m <- matrix(sample(x, n_boot * length(x), replace = TRUE), n_boot, length(x))
  means <- apply(m, 1, mean)
  data.table(
    mean = mean(x),
    low = quantile(means, 0.25),
    high = quantile(means, 0.75),
    lower = quantile(means, 0.05),
    higher = quantile(means, 0.95)
  )
}

## Read profiles
main_profiles <- fread(main_file)
branch_profiles <- fread(branch_file)

## Add branch labels
main_profiles[, branch := "main"]
branch_profiles[, branch := "branch"]

## Find common operations (handle interface changes gracefully)
main_ops <- unique(main_profiles$name)
branch_ops <- unique(branch_profiles$name)
common_ops <- intersect(main_ops, branch_ops)

if (length(common_ops) == 0) {
  message("No common profile operations between versions.")
  message("Main operations: ", paste(main_ops, collapse = ", "))
  message("Branch operations: ", paste(branch_ops, collapse = ", "))
  quit(status = 0)
}

## Report any operations that exist in only one version
main_only <- setdiff(main_ops, branch_ops)
branch_only <- setdiff(branch_ops, main_ops)

if (length(main_only) > 0) {
  message("Operations only in main: ", paste(main_only, collapse = ", "))
}
if (length(branch_only) > 0) {
  message("Operations only in branch: ", paste(branch_only, collapse = ", "))
}

## Filter to common operations and combine
main_profiles <- main_profiles[name %in% common_ops]
branch_profiles <- branch_profiles[name %in% common_ops]
summary <- rbindlist(list(main_profiles, branch_profiles))
setnames(summary, "name", "operation")

## Calculate summary statistics with bootstrap CIs
summary_ci <- summary[,
  bootci(total_time),
  by = .(branch, operation)
]
setorder(summary_ci, -mean)
summary_ci <- summary_ci[, lapply(.SD, signif, 2), by = .(branch, operation)]

summary_means <- dcast(summary_ci, operation ~ branch, value.var = "mean")

## Calculate percentage change
format_summary <- dcast(
  summary, operation + iter + chain ~ branch, value.var = "total_time"
)

changes <- format_summary[,
  change := round((branch - main) / main * 100)
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
), by = "operation"]

## Only output if there are notable changes
if (any(changes$trend != "no change")) {
  format_output <- merge(
    summary_means,
    changes[, .(operation, mean, range, trend)],
    by = "operation"
  )

  setorder(format_output, -main)
  format_output <- format_output[, lapply(.SD, as.character)]
  setnames(format_output, "mean", "% change")

  ## Build output
  output_lines <- character()

  ## Add note about excluded operations if any
  if (length(main_only) > 0 || length(branch_only) > 0) {
    output_lines <- c(
      output_lines,
      "",
      "**Note:** Some profile operations differ between versions:",
      ""
    )
    if (length(main_only) > 0) {
      output_lines <- c(
        output_lines,
        paste0("- Removed in branch: ", paste(main_only, collapse = ", "))
      )
    }
    if (length(branch_only) > 0) {
      output_lines <- c(
        output_lines,
        paste0("- Added in branch: ", paste(branch_only, collapse = ", "))
      )
    }
    output_lines <- c(output_lines, "")
  }

  sink(file = output_file)
  cat(knitr::kable(
    format_output, align = "lrrrr",
    caption = "Benchmarking results (mean time in seconds)."
  ))
  cat("\n")
  if (length(output_lines) > 0) {
    cat(paste(output_lines, collapse = "\n"))
  }
  sink()

  message("Results written to: ", output_file)
} else {
  message("No notable performance changes detected.")
}
