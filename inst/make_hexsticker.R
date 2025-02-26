library(EpiNow2)
library(hexSticker)
library(ggplot2)
library(parallel)
library(magick)

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

# hex plot
hex_plot <- out$plots$infections +
  theme_void() +
  theme_transparent() +
  theme(
    legend.position = "none",
    panel.background = element_blank()
  )

# Make and save hexsticker

#' Grey hex border and grey package name. Plot is large and pushed to the bottom
# to fill the space on the bottom.
#' See https://github.com/epiforecasts/EpiNow2/pull/1004#issuecomment-2682562866
grey_opt1 <- sticker(
  hex_plot,
  s_x = 1,
  s_y = 0.85,
  s_width = 1.9,
  s_height = 1.1,
  package = "EpiNow2",
  p_color = "#646770",
  p_size = 76,
  p_x = 1,
  p_y = 1.45,
  h_fill = "#F5F5F5",
  h_color = "#646770",
  filename = file.path("man", "figures", "logo.png"),
  dpi = 1200,
  white_around_sticker = TRUE
)

#' Grey hex border and black package name. Package name is placed below
#' the plot to remove the awkwardness of the black bottom of the plot. The curves in
#' plot complement the corners of the hexagon.
#' See https://github.com/epiforecasts/EpiNow2/pull/1004#issuecomment-2684934918
black_opt1 <- sticker(
  hex_plot,
  s_x = 0.95,
  s_y = 1.35,
  s_width = 1.50,
  s_height = 0.55,
  spotlight = TRUE,
  package = "EpiNow2",
  p_color = "grey20",
  p_size = 95,
  p_x = 1,
  p_y = 0.75,
  h_fill = "#F5F5F5",
  h_color = "grey45",
  filename = file.path("man", "figures", "logo.png"),
  dpi = 1200,
  white_around_sticker = TRUE
)

# Purple hex border with purple package name. Figure is large and pushed to the bottom
# to fill the space on the bottom. Alternative to grey_opt1 above.
# See https://github.com/epiforecasts/EpiNow2/pull/1004#issuecomment-2684782553
purple_opt1 <- sticker(
  hex_plot,
  s_x = 1.0,
  s_y = 0.55,
  s_width = 2.3,
  s_height = 1.1,
  package = "EpiNow2",
  p_color = "#6951A1",
  p_size = 96,
  p_x = 1,
  p_y = 1.35,
  h_fill = "#F5F5F5",
  h_color = "#6951A1",
  filename = file.path("man", "figures", "logo.png"),
  dpi = 1200,
  white_around_sticker = TRUE
)

#' Purple hex borders with purple package name. Smaller logo centered in the hex.
#' See https://github.com/epiforecasts/EpiNow2/pull/1004#issuecomment-2684835329
purple_opt2 <- sticker(
  hex_plot,
  s_x = 1.00,
  s_y = 0.85,
  s_width = 1.50,
  s_height = 0.65,
  package = "EpiNow2",
  p_color = "#6951A1",  # Darker text for better readability
  p_size = 80,  # Slightly larger font size
  p_x = 1,
  p_y = 1.5,  # Adjusted spacing
  h_fill = "#F5F5F5",
  h_color = "#6951A1",  # Darker hex border for contrast
  filename = file.path("man", "figures", "logo.png"),
  dpi = 1200,
  white_around_sticker = TRUE
)

#' Purple hex border and purple package name. Package name is placed below
#' the plot to remove the awkwardness of the black bottom of the plot. The curves in
#' plot complement the corners of the hexagon. Alternative to black_opt1.
#' See https://github.com/epiforecasts/EpiNow2/pull/1004#issuecomment-2684946714
purple_opt3 <- sticker(
  hex_plot,
  s_x = 0.95,
  s_y = 1.35,
  s_width = 1.50,
  s_height = 0.55,
  spotlight = TRUE,
  package = "EpiNow2",
  p_color = "#6951A1",
  p_size = 95,
  p_x = 1,
  p_y = 0.75,
  h_fill = "#F5F5F5",
  h_color = "#6951A1",
  filename = file.path("man", "figures", "logo.png"),
  dpi = 1200,
  white_around_sticker = TRUE
)

# Make outside of hex sticker transparent
p <- image_read(file.path("man", "figures", "logo.png"))
fuzz <- 50

pp <- p |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = "+1+1"
  ) |>
  image_fill(
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+", image_info(p)$width - 1, "+1")
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

image_write(image = pp, path = file.path("man", "figures", "logo.png"))

usethis::use_logo(file.path("man", "figures", "logo.png"))
pkgdown::build_favicons(pkg = ".", overwrite = TRUE)
