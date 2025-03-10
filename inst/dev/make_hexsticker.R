library(EpiNow2)
library(here)
library(hexSticker)
library(ggplot2)
library(parallel)
library(magick)

set.seed(4567)

# Make standard plot
options(mc.cores = detectCores() - 2)

# set an example generation time. In practice this should use an estimate
# from the literature or be estimated from data
generation_time <- Gamma(
  shape = Normal(1.3, 0.3),
  rate = Normal(0.37, 0.09),
  max = 14
)
# set an example incubation period. In practice this should use an estimate
# from the literature or be estimated from data
incubation_period <- LogNormal(
  meanlog = Normal(1.6, 0.06),
  sdlog = Normal(0.4, 0.07),
  max = 14
)
# set an example reporting delay. In practice this should use an estimate
# from the literature or be estimated from data
reporting_delay <- LogNormal(mean = 2, sd = 1, max = 10)

# example case data
reported_cases <- example_confirmed[1:40]

# estimate Rt and nowcast/forecast cases by date of infection
out <- epinow(
  data = reported_cases,
  generation_time = gt_opts(generation_time),
  rt = rt_opts(prior = LogNormal(mean = 2, sd = 0.1)),
  delays = delay_opts(incubation_period + reporting_delay)
)

out$plots$infections$layers[[2]] <- NULL

hex_plot <- out$plots$infections +
  theme_void() +
  theme_transparent() +
  theme(
    legend.position = "none",
    panel.background = element_blank()
  )

sticker(
  hex_plot,
  u_x = 0.37,
  u_y = 1.4,
  u_angle = 0,
  u_size = 106,
  u_color = "#7a74b3",
  url = "EpiNow2",
  package = "",
  s_width = 2.7,
  s_height = 1.5,
  s_x = 0.642,
  s_y = 0.51,
  h_fill = "#F5F5F5",
  h_color = "#918cc0",
  filename = here("man", "figures", "logo.png"),
  dpi = 1200,
  white_around_sticker = TRUE
)

# Make outside of hex sticker transparent
p <- image_read(here("man", "figures", "logo.png"))
fuzz <- 30

pp <- p |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = "+1+2"
  ) |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+", image_info(p)$width - 1, "+2")
  ) |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+1", "+", image_info(p)$height - 1)
  ) |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+", image_info(p)$width - 1, "+", image_info(p)$height - 1)
  )

image_write(image = pp, path = here("man", "figures", "logo.png"))

usethis::use_logo(here("man", "figures", "logo.png"))
pkgdown::build_favicons(pkg = ".", overwrite = TRUE)
