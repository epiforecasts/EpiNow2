library(magrittr)

## Traces as here: https://github.com/aakhmetz/nCoVSerialInterval2020/blob/master/scripts/A1.%20Stan%20simulations.ipynb
traces <- data.table::fread("data-raw/nishiura-lognormal-truncated.csv")


## Reparam traces as here: https://github.com/aakhmetz/nCoVSerialInterval2020/blob/master/scripts/A1.%20Stan%20simulations.ipynb
mean_si <- exp(traces$param1 + traces$param2^2/2)
sd_si <- sqrt((exp(traces$param2^2)-1)*exp(2*traces$param1+traces$param2^2))


samples <- purrr::map2(traces$param1[1:1000], traces$param2[1:1000], ~ rlnorm(1000, meanlog = .x, sdlog = .y)) %>%
  purrr::map( ~ c(0, round(.) %>%
                    table %>%
                    {. / sum(.)}))


max_sample_length <- samples %>%
  purrr::map_dbl(length) %>%
  max()

pad_samples <- samples %>%
  purrr::map(~ c(., rep(0, max_sample_length - length(.))))

covid_serial_intervals <- matrix(unlist(pad_samples), ncol = length(pad_samples))

usethis::use_data(covid_serial_intervals, overwrite = TRUE)

