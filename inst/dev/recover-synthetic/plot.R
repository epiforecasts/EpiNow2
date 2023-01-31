# Plotting functions
# plot Rt vs data
plot_estimates_truth <- function(est, truth, stat = "R", ...) {
  plot_estimates(
    estimate = est$summarised[variable == stat],
    reported = data.table::data.table(
      date = sims$summarised[variable == stat]$date,
      confirm = truth
    ),
    obs_as_col = FALSE,
    ylab = stat,
  )
}

save_ggplot <- function(plot, name, prefix = "") {
  if (nchar(prefix) > 0) prefix <- paste0(prefix, "_")
  ggplot2::ggsave(
    here::here("inst", "dev", "figs", paste0(prefix, name, ".png")),
    plot,
    dpi = 300, width = 9, height = 6
  )
}

make_plots <- function(est, R, inf, name) {
  rt_plot <- plot_estimates_truth(est, R, hline = 1)
  inf_plot <- plot_estimates_truth(est, inf, stat = "infections")
  save_ggplot(rt_plot, name, "rt")
  save_ggplot(inf_plot, name, "inf")
}

