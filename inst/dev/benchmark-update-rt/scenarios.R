# Benchmark scenarios shared by bench.R and check-gradients.R
rt_scenario <- function(t, scenario) {
  n_centre <- t - 7
  weekly_bps <- floor(seq_len(t) / 7) + 1
  switch(scenario,
    a = list(gp_n = t - 1, bp_n = 0, stationary = 0, bps = rep(1, t)),
    b = list(gp_n = t - 7, bp_n = 0, stationary = 1, bps = rep(1, t)),
    c = list(gp_n = 0, bp_n = ceiling(t / 7) - 1, stationary = 0,
      bps = weekly_bps),
    d = list(gp_n = t - 1, bp_n = ceiling(t / 7) - 1, stationary = 0,
      bps = weekly_bps)
  ) |>
    c(list(t = t, n_centre = n_centre))
}
scenario_labels <- c(
  a = "non-stationary GP", b = "stationary GP", c = "weekly breakpoints",
  d = "non-stationary GP + weekly breakpoints"
)
