
#' Estimate the time varying R0 - using EpiEstim
#'
#' @param cases A dataframe containing a list of local cases with the following variables: `date`, `cases`, and `import_status`
#' @param generation_times A matrix with columns representing samples and rows representing the probability of the generation timebeing on
#' that day.
#' @param rt_prior A list defining the reproduction number prior containing the mean (`mean_prior`) and standard deviation (`std_prior`)
#' @param windows Numeric vector, windows over which to estimate time-varying R. The best performing window will be 
#' selected per serial interval sample by default (based on which window best forecasts current cases). 
#' @param gt_samples Numeric, the number of samples to take from the generaiton times supplied
#' @param rt_samples Numeric, the number of samples to take from the estimated R distribution for each time point.
#' @param min_est_date Date to begin estimation.
#' @param forecast_model An uninitialised bsts model passed to `EpiSoon::forecast_rt` to be used for forecasting
#' future Rt values. An example of the required structure is: `function(ss, y){bsts::AddSemilocalLinearTrend(ss, y = y)}`.
#' @param horizon Numeric, defaults to 0. The horizon over which to forecast Rts and cases.
#' @return A list of `data.table`'s containing the date and summarised R estimate and optionally a case forecast
#' @export
#' @importFrom EpiEstim estimate_R make_config
#' @importFrom purrr map2 map2_dbl safely map
#' @importFrom EpiSoon predict_current_cases forecast_rt forecast_cases score_case_forecast
#' @importFrom data.table setDT rbindlist .SD dcast copy
#' @examples
#'
#' ## Nowcast Rts                  
#' estimates <- estimate_R0(cases = EpiSoon::example_obs_cases, 
#'                          generation_times = as.matrix(EpiNow::covid_generation_times[,2]), 
#'                          rt_prior = list(mean_prior = 2.6, std_prior = 2),
#'                          windows = c(1, 3, 7), rt_samples = 10, gt_samples = 1)
#'                          
#'                          
#' estimates$rts
#'  
#'\dontrun{ 
#'## Nowcast Rts, forecast Rts and the forecast cases
#' estimates <- estimate_R0(cases = EpiSoon::example_obs_cases, 
#'                          generation_times = as.matrix(EpiNow::covid_generation_times[,1]), 
#'                          rt_prior = list(mean_prior = 2.6, std_prior = 2),
#'                          windows = c(1, 3, 7), rt_samples = 10, gt_samples = 20,
#'                          min_est_date =  as.Date("2020-02-18"),
#'                          forecast_model = function(...){
#'                               EpiSoon::fable_model(model = fable::ETS(y ~ trend("A")), ...)
#'                               },
#'                          horizon = 14)
#'                                            
#'## Rt estimates and forecasts
#' estimates$rts
#' 
#' 
#' 
#' ## Case forecasts
#' estimates$cases
#' }
estimate_R0 <- function(cases = NULL, generation_times = NULL,
                        rt_prior = NULL, windows = NULL, 
                        gt_samples = 100, rt_samples = 100,
                        min_est_date = NULL, forecast_model = NULL, 
                        horizon = 0) {
  
  data.table::setDTthreads(1)
  ##Generic mean gamma sampler
  mean_rgamma <- function(samples, mean, sd) {
      theta <- sd^2/mean
      k <- mean/theta
      samples <- stats::rgamma(samples, shape = k, scale = theta)
      
      return(samples)
  }
  
  ## Adjust input based on the presence of imported cases
  if (suppressWarnings(length(unique(cases$import_status)) > 1)) {
    
    ## Select columns
    incid <- data.table::setDT(cases)[, .(date, cases, import_status)]
    
    ## Fill in any missing data
    incid <- incid[incid[, .(date = seq(min(date), max(date), by = "days"))],
                   on = "date"][
                     is.na(cases), cases := 0]
    
    ## Spread to wide and fill in missing combinations
    incid <- data.table::dcast(incid, date ~ import_status, 
                                 value.var = "cases", fill = 0)
 
    ## Predict cases forward in time using just local cases
    summed_cases <- data.table::copy(incid)[, `:=`(cases = local, imported = NULL)]
  
  }else{
    incid <- data.table::setDT(cases)[, .(date, I = cases)]

    incid <- incid[incid[, .(date = seq(min(date), max(date), by = "days"))],
                   on = "date"][
      is.na(I), I := 0]

    summed_cases <- data.table::copy(incid)[, cases := I][, I := NULL]
  }
 
  ## Calculate when to start the window estimation of Rt
  min_case_date <- summed_cases[cases > 0][date == min(date)]$date
    
  if (!is.null(min_est_date)){
    wait_time <- as.numeric(min_est_date - min_case_date) + 1
  }else{
    wait_time <- 1
  }
 
  if (wait_time > nrow(incid)){
    wait_time <- nrow(incid)
  }
  
  
  ## Sample serial intervals
  generation_times_index <- sample(1:ncol(generation_times),
                             gt_samples,
                             replace = ncol(generation_times) < gt_samples)

   
  ### Estimate R across serial interval samples
  ### Forecast ahead if given a model and horizon for each sample
  estimates <- purrr::map(generation_times_index, function(index) {

    ### Estimate and score R over multiple windows
    est_r <- purrr::map(windows,
                        function(window) {
                          

                          window_start <- seq(max(wait_time - window, 2),
                                              nrow(incid) - window)
                          window_end <- window_start + window


                          ## estimate R
                          R <- suppressWarnings(
                            EpiEstim::estimate_R(incid,
                                                 method = "si_from_sample",
                                                 si_sample = generation_times[, index],
                                                 config = do.call(EpiEstim::make_config,
                                                                  c(rt_prior,
                                                                    list(t_start = window_start,
                                                                         t_end = window_end)
                                                                  )))$R
                          )

                          ## Filter out NA values, choose columns and make into data.table
                          R <- data.table::setDT(R)
                          R <- R[!is.na(`Mean(R)`), .(t_start, t_end, `Mean(R)`, `Std(R)`)]

                          ## Take samples from the assumed gamma distribution
                          R <- R[, .(date = EpiNow::add_dates(incid$date, .N), mean_R = `Mean(R)`,
                                     sd_R = `Std(R)`, sample_R = purrr::map2(`Mean(R)`, `Std(R)`,
                                                                       ~ sort(mean_rgamma(rt_samples, .x, .y))),
                                     sample = list(1:rt_samples))]

                          R <- R[, .(sample_R = unlist(sample_R), sample = unlist(sample)),
                                 by = c("date", "mean_R", "sd_R")]

                         ## Make current case predictions from past cases and current Rt values
                          preds <-
                            purrr::map(
                              split(R, by = "sample"),
                              ~ EpiSoon::predict_current_cases(
                                rts = .[,.(date, rt = sample_R)],
                                cases = summed_cases,
                                serial_interval = generation_times[, index]
                              ))

                          preds <- data.table::rbindlist(preds, idcol = "sample")
                          preds <- preds[, `:=`(sample = as.numeric(sample), horizon = 0)][,
                                          .(date, cases, sample, horizon)]

                          ## Score the forecast
                          scores <- data.table::setDT(
                            EpiSoon::score_case_forecast(preds, summed_cases,
                                                         scores = "crps"))[,horizon := NULL]

                          R <- R[scores, on = "date", nomatch = 0][, "window" := window]
                          R <- data.table::setDF(R)
                          return(R)
                        })

    ## Join output
    est_r <- data.table::rbindlist(est_r)

    ## Choose best scoring (according to CRPS) window at each timepoint
    est_r <- est_r[, .SD[crps == min(crps)], by = "date"]

    ## Check to see how many Rt data points have been returned
    ## If fewer than 3 then turn off forecasting
    if (length(unique(est_r$date)) < 3) {
        horizon <- 0
    }


      if (horizon > 0 & !is.null(forecast_model)) {

        safe_forecast <- purrr::safely(EpiSoon::forecast_rt)

        ## Forecast Rts using the mean estimate
        rt_forecasts <-
          data.table::setDT(
            safe_forecast(rts = est_r[sample == 1, .(date, rt = mean_R)],
                          model = forecast_model,
                          horizon = horizon,
                          samples = rt_samples)[[1]]
          )

        ##Forecast the variance using the same model structure
        sd_forecasts <-
          data.table::setDT(
            safe_forecast(rts = est_r[sample == 1, .(date, rt = sd_R)],
                          model = forecast_model,
                          horizon = horizon,
                          samples = rt_samples)[[1]]
          )[, sd_rt := rt][,rt := NULL]

        ## Join mean and sd forecasts
        rt_forecasts <- rt_forecasts[sd_forecasts, on = c("date", "sample", "horizon")]

        ## Add gamma noise  and drop mean and sd forecasts
        ## Assume that the minumum allowed gamma noise is 1e-4
        ## Zero sd will result in rgamma draws that are also 0 and so must be avoided
        rt_forecasts <- rt_forecasts[sd_rt <= 0, sd_rt := 1e-4][,
                rt := purrr::map2_dbl(rt, sd_rt, ~ mean_rgamma(1, mean = .x, sd = .y))][,
                    .(sample, date, horizon, rt)]

        ## Forecast cases
        case_forecasts <-
          data.table::setDT(
            EpiSoon::forecast_cases(
              cases = summed_cases,
              fit_samples = rt_forecasts,
              rdist = rpois,
              serial_interval = generation_times[, index]
            )
          )[,rt_type := "forecast"]

        ## Join Rt estimates and forecasts
        est_r <- est_r[, `:=`(R = sample_R, rt_type = "nowcast")][,
                  `:=`(mean_R = NULL, sd_R = NULL, sample_R = NULL)][,
                   .(date, R, sample, crps, window, rt_type)]

        est_r <- data.table::rbindlist(
          list(est_r,
          rt_forecasts[,.(date, R = rt, sample, rt_type = "forecast")]
          ), fill = TRUE)

        return(list(rts = est_r, cases = case_forecasts))
      }else{

        ## Return just nowcast if no forecast has been run
        est_r <- est_r[, .(date, R = sample_R, sample,
                           crps, window, rt_type = "nowcast")]

        return(list(rts = est_r))
      }
  })

  ## Reorganise list output
  estimates <- purrr::transpose(estimates)

  ## Function to bind si sample outputs
  join_gt_samples <- function(df) {
    df <- data.table::rbindlist(df, idcol = "gt_sample")

    df <- df[, sample := as.numeric(sample) * as.numeric(gt_sample)][,
               gt_sample := NULL]
  }

  ## Make sample unique for Rts
  estimates$rts <- join_gt_samples(estimates$rts)

  ## If forecast has been run do the same for cases
  if (horizon > 0 & !is.null(forecast_model)) {
    estimates$cases <- join_gt_samples(estimates$cases)
  }
  
  return(estimates)
}



