dir <- "inst/stan/functions"
stan_fns <- rev(list.files(dir))
EpiNow2::expose_stan_fns(stan_fns, dir)
