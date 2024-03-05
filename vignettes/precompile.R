# Vignettes that have long run times

library("knitr")
orig_files <- list.files(pattern = "Rmd.orig$")
for (f in orig_files) {
  knit(f, sub(".orig$", "", f))
}
