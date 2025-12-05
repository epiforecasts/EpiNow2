# Regional Summary Output

**\[maturing\]** Used to produce summary output either internally in
`regional_epinow` or externally.

## Usage

``` r
regional_summary(
  regional_output = NULL,
  data,
  results_dir = NULL,
  summary_dir = NULL,
  target_date = NULL,
  region_scale = "Region",
  all_regions = TRUE,
  return_output = is.null(summary_dir),
  plot = TRUE,
  max_plot = 10,
  ...
)
```

## Arguments

- regional_output:

  A list of output as produced by
  [`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md)
  and stored in the `regional` list.

- data:

  A `<data.frame>` of disease reports (confirm) by date (date), and
  region (`region`).

- results_dir:

  An optional character string indicating the location of the results
  directory to extract results from.

- summary_dir:

  A character string giving the directory in which to store summary of
  results.

- target_date:

  A character string giving the target date for which to extract results
  (in the format "yyyy-mm-dd"). Defaults to latest available estimates.

- region_scale:

  A character string indicating the name to give the regions being
  summarised.

- all_regions:

  Logical, defaults to `TRUE`. Should summary plots for all regions be
  returned rather than just regions of interest.

- return_output:

  Logical, defaults to FALSE. Should output be returned, this
  automatically updates to TRUE if no directory for saving is specified.

- plot:

  Logical, defaults to `TRUE`. Should regional summary plots be
  produced.

- max_plot:

  Numeric, defaults to 10. A multiplicative upper bound on the\\ number
  of cases shown on the plot. Based on the maximum number of reported
  cases.

- ...:

  Additional arguments passed to `report_plots`.

## Value

A list of summary measures and plots

## See also

[`regional_epinow()`](https://epiforecasts.io/EpiNow2/dev/reference/regional_epinow.md)

## Examples

``` r
# get example output from regional_epinow model
regional_out <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_regional_epinow.rds"
))

regional_summary(
  regional_output = regional_out$regional,
  data = regional_out$summary$reported_cases
)
#> INFO [2025-12-05 20:02:52] No summary directory specified so returning summary output
#> $latest_date
#> [1] "2020-04-21"
#> 
#> $results
#> $results$estimates
#> $results$estimates$summarised
#> Indices: <variable>, <region>
#>        region       date                 variable  strat     type       median
#>        <char>     <Date>                   <char> <char>   <char>        <num>
#>   1: testland 2020-02-22                        R   <NA> estimate    2.1755546
#>   2: testland 2020-02-23                        R   <NA> estimate    2.1416454
#>   3: testland 2020-02-24                        R   <NA> estimate    2.1012037
#>   4: testland 2020-02-25                        R   <NA> estimate    2.0607501
#>   5: testland 2020-02-26                        R   <NA> estimate    2.0219041
#>  ---                                                                          
#> 560: realland 2020-04-26           reported_cases   <NA> forecast 2744.5000000
#> 561: realland 2020-04-27           reported_cases   <NA> forecast 2602.5000000
#> 562: realland 2020-04-28           reported_cases   <NA> forecast 2111.0000000
#> 563: realland       <NA> reporting_overdispersion   <NA>     <NA>    0.1679094
#> 564: realland       <NA>                      rho   <NA>     <NA>   18.3912056
#>              mean           sd     lower_90     lower_50     lower_20
#>             <num>        <num>        <num>        <num>        <num>
#>   1:    2.1846025   0.10794295    2.0177562    2.1003475    2.1492054
#>   2:    2.1474213   0.09670777    1.9931312    2.0712255    2.1163732
#>   3:    2.1089527   0.08801488    1.9729887    2.0415659    2.0806889
#>   4:    2.0693779   0.08152743    1.9441177    2.0117011    2.0417723
#>   5:    2.0288621   0.07673437    1.9036506    1.9713526    2.0079071
#>  ---                                                                 
#> 560: 2790.1350000 702.42545825 1731.6500000 2362.2500000 2584.2000000
#> 561: 2680.3250000 809.32458308 1577.7000000 2087.7500000 2395.2000000
#> 562: 2230.4000000 712.77557837 1260.7000000 1740.5000000 1961.0000000
#> 563:    0.1682596   0.01932362    0.1370876    0.1558891    0.1633468
#> 564:   19.5438277   6.13393532   11.7078772   15.3024172   17.1811641
#>          upper_20     upper_50     upper_90
#>             <num>        <num>        <num>
#>   1:    2.2162031    2.2600171    2.3616361
#>   2:    2.1727190    2.2128760    2.3126088
#>   3:    2.1285358    2.1745113    2.2609432
#>   4:    2.0891854    2.1330387    2.2176273
#>   5:    2.0433392    2.0835454    2.1659812
#>  ---                                       
#> 560: 2863.2000000 3125.7500000 3966.6000000
#> 561: 2788.8000000 3096.2500000 4347.3500000
#> 562: 2313.0000000 2578.7500000 3496.3500000
#> 563:    0.1707838    0.1791721    0.2038153
#> 564:   19.8542697   22.9181453   30.3380606
#> 
#> 
#> 
#> $summarised_results
#> $summarised_results$table
#>      Region New infections per day Expected change in reports
#>      <char>                 <char>                     <fctr>
#> 1: realland     1760 (942 -- 3880)          Likely decreasing
#> 2: testland     1709 (764 -- 4526)          Likely decreasing
#>    Effective reproduction no.           Rate of growth
#>                        <char>                   <char>
#> 1:         0.89 (0.76 -- 1.1) -0.031 (-0.069 -- 0.021)
#> 2:         0.89 (0.72 -- 1.1) -0.031 (-0.084 -- 0.026)
#>    Doubling/halving time (days)
#>                          <char>
#> 1:              -23 (33 -- -10)
#> 2:             -23 (27 -- -8.2)
#> 
#> $summarised_results$data
#>      region           estimate  median   mean      sd lower_90 lower_50
#>      <fctr>             <char>   <num>  <num>   <num>    <num>    <num>
#> 1: realland 1760 (942 -- 3880) 1760.00 2064.0 1074.00   942.00  1352.00
#> 2: testland 1709 (764 -- 4526) 1709.00 2124.0 1292.00   764.00  1210.00
#> 3: realland 0.89 (0.76 -- 1.1)    0.89    0.9    0.11     0.76     0.83
#> 4: testland 0.89 (0.72 -- 1.1)    0.89    0.9    0.12     0.72     0.81
#>    lower_20 upper_20 upper_50 upper_90                     metric
#>       <num>    <num>    <num>    <num>                     <fctr>
#> 1:  1587.00  2030.00  2535.00   3880.0     New infections per day
#> 2:  1488.00  2146.00  2749.00   4526.0     New infections per day
#> 3:     0.87     0.92     0.96      1.1 Effective reproduction no.
#> 4:     0.85     0.94     0.98      1.1 Effective reproduction no.
#>    Expected change in reports prob_control
#>                        <fctr>       <list>
#> 1:          Likely decreasing         0.84
#> 2:          Likely decreasing         0.78
#> 3:          Likely decreasing         0.84
#> 4:          Likely decreasing         0.78
#> 
#> $summarised_results$regions_by_inc
#> [1] "realland" "testland"
#> 
#> 
#> $summary_plot

#> 
#> $summarised_measures
#> $summarised_measures$rt
#>        region       date  strat     type    median      mean         sd
#>        <char>     <Date> <char>   <char>     <num>     <num>      <num>
#>   1: realland 2020-02-22   <NA> estimate 2.1965091 2.2083670 0.11686624
#>   2: realland 2020-02-23   <NA> estimate 2.1553367 2.1699645 0.10632806
#>   3: realland 2020-02-24   <NA> estimate 2.1204048 2.1304785 0.09860815
#>   4: realland 2020-02-25   <NA> estimate 2.0819848 2.0900056 0.09334381
#>   5: realland 2020-02-26   <NA> estimate 2.0449176 2.0486246 0.08995381
#>  ---                                                                   
#> 130: testland 2020-04-24   <NA> forecast 0.8944846 0.9015978 0.12445024
#> 131: testland 2020-04-25   <NA> forecast 0.8944846 0.9015978 0.12445024
#> 132: testland 2020-04-26   <NA> forecast 0.8944846 0.9015978 0.12445024
#> 133: testland 2020-04-27   <NA> forecast 0.8944846 0.9015978 0.12445024
#> 134: testland 2020-04-28   <NA> forecast 0.8944846 0.9015978 0.12445024
#>      lower_90  lower_50  lower_20  upper_20  upper_50 upper_90
#>         <num>     <num>     <num>     <num>     <num>    <num>
#>   1: 2.049067 2.1353738 2.1692977 2.2323750 2.2928182 2.387440
#>   2: 2.010039 2.1034287 2.1322803 2.1873761 2.2476014 2.335752
#>   3: 1.980214 2.0631701 2.0952684 2.1495342 2.2013806 2.279709
#>   4: 1.939899 2.0314610 2.0634560 2.1022222 2.1509765 2.243858
#>   5: 1.904171 1.9887547 2.0223399 2.0667743 2.0995346 2.197421
#>  ---                                                          
#> 130: 0.717204 0.8075987 0.8546392 0.9365739 0.9836182 1.094407
#> 131: 0.717204 0.8075987 0.8546392 0.9365739 0.9836182 1.094407
#> 132: 0.717204 0.8075987 0.8546392 0.9365739 0.9836182 1.094407
#> 133: 0.717204 0.8075987 0.8546392 0.9365739 0.9836182 1.094407
#> 134: 0.717204 0.8075987 0.8546392 0.9365739 0.9836182 1.094407
#> 
#> $summarised_measures$growth_rate
#>        region       date  strat     type      median        mean          sd
#>        <char>     <Date> <char>   <char>       <num>       <num>       <num>
#>   1: realland 2020-02-23   <NA> estimate  0.24471768  0.24440892 0.011303876
#>   2: realland 2020-02-24   <NA> estimate  0.23787832  0.23724623 0.009858402
#>   3: realland 2020-02-25   <NA> estimate  0.22898804  0.22927240 0.008785085
#>   4: realland 2020-02-26   <NA> estimate  0.22062263  0.22096076 0.008114626
#>   5: realland 2020-02-27   <NA> estimate  0.21207793  0.21237831 0.007753683
#>  ---                                                                        
#> 128: testland 2020-04-24   <NA> forecast -0.03063276 -0.02861478 0.034976116
#> 129: testland 2020-04-25   <NA> forecast -0.03068320 -0.02863692 0.035130344
#> 130: testland 2020-04-26   <NA> forecast -0.03071984 -0.02865644 0.035235880
#> 131: testland 2020-04-27   <NA> forecast -0.03074878 -0.02867318 0.035314372
#> 132: testland 2020-04-28   <NA> forecast -0.03077213 -0.02868717 0.035374656
#>         lower_90    lower_50    lower_20    upper_20     upper_50   upper_90
#>            <num>       <num>       <num>       <num>        <num>      <num>
#>   1:  0.22341293  0.23720117  0.24224383  0.24800319  0.252048080 0.26096927
#>   2:  0.21765445  0.23156016  0.23561463  0.24014595  0.243968983 0.25185809
#>   3:  0.21416801  0.22422604  0.22782035  0.23197997  0.234968560 0.24346732
#>   4:  0.20854258  0.21668688  0.21961447  0.22304461  0.225938358 0.23435144
#>   5:  0.19965914  0.20749859  0.21085043  0.21423195  0.216761654 0.22544625
#>  ---                                                                        
#> 128: -0.08318073 -0.05491414 -0.04049717 -0.01747933 -0.004585665 0.02535367
#> 129: -0.08358477 -0.05498174 -0.04046760 -0.01745463 -0.004467709 0.02552564
#> 130: -0.08388781 -0.05502790 -0.04043318 -0.01743246 -0.004389319 0.02563856
#> 131: -0.08411816 -0.05506112 -0.04038107 -0.01741971 -0.004332948 0.02571963
#> 132: -0.08428930 -0.05508511 -0.04033417 -0.01742958 -0.004291000 0.02578029
#> 
#> $summarised_measures$cases_by_infection
#>        region       date  strat     type median   mean     sd lower_90 lower_50
#>        <char>     <Date> <char>   <char>  <num>  <num>  <num>    <num>    <num>
#>   1: realland 2020-02-22   <NA> estimate  206.8  206.3   12.0    186.5    198.7
#>   2: realland 2020-02-23   <NA> estimate  264.6  263.3   15.0    238.4    254.2
#>   3: realland 2020-02-24   <NA> estimate  334.6  333.8   18.9    302.4    320.8
#>   4: realland 2020-02-25   <NA> estimate  420.4  419.9   23.7    382.8    403.8
#>   5: realland 2020-02-26   <NA> estimate  523.8  523.7   29.5    477.2    502.8
#>  ---                                                                           
#> 130: testland 2020-04-24   <NA> forecast 1962.1 2225.1  978.2   1055.2   1521.7
#> 131: testland 2020-04-25   <NA> forecast 1890.7 2195.2 1053.7    975.8   1436.5
#> 132: testland 2020-04-26   <NA> forecast 1823.1 2168.4 1131.0    898.1   1357.8
#> 133: testland 2020-04-27   <NA> forecast 1764.3 2144.8 1210.4    830.7   1281.1
#> 134: testland 2020-04-28   <NA> forecast 1709.4 2124.2 1292.2    765.0   1210.1
#>      lower_20 upper_20 upper_50 upper_90
#>         <num>    <num>    <num>    <num>
#>   1:    203.7    210.1    214.7    225.1
#>   2:    259.6    267.6    273.8    288.1
#>   3:    328.8    339.1    347.5    362.1
#>   4:    413.3    428.0    437.1    453.6
#>   5:    516.0    534.8    543.0    565.0
#>  ---                                    
#> 130:   1768.5   2312.9   2770.8   4147.0
#> 131:   1693.4   2269.0   2772.4   4242.9
#> 132:   1618.9   2223.7   2761.0   4335.2
#> 133:   1550.3   2179.5   2752.5   4429.6
#> 134:   1488.6   2146.7   2749.1   4526.2
#> 
#> $summarised_measures$cases_by_report
#>        region       date  strat     type median   mean    sd lower_90 lower_50
#>        <char>     <Date> <char>   <char>  <num>  <num> <num>    <num>    <num>
#>   1: realland 2020-02-22   <NA> estimate   34.0   35.8  10.2     23.0     29.0
#>   2: realland 2020-02-23   <NA> estimate   54.0   53.5  13.0     33.0     44.0
#>   3: realland 2020-02-24   <NA> estimate   62.0   63.9  14.8     43.0     54.0
#>   4: realland 2020-02-25   <NA> estimate   72.5   73.1  16.0     46.0     63.0
#>   5: realland 2020-02-26   <NA> estimate   83.0   84.0  18.4     56.0     71.8
#>  ---                                                                          
#> 130: testland 2020-04-24   <NA> forecast 2720.0 2844.1 750.0   1861.5   2325.5
#> 131: testland 2020-04-25   <NA> forecast 2411.0 2531.7 662.7   1595.8   2104.8
#> 132: testland 2020-04-26   <NA> forecast 2725.5 2805.1 781.5   1806.8   2248.2
#> 133: testland 2020-04-27   <NA> forecast 2411.5 2558.3 799.1   1563.3   1947.8
#> 134: testland 2020-04-28   <NA> forecast 2025.5 2190.7 735.6   1212.6   1670.0
#>      lower_20 upper_20 upper_50 upper_90
#>         <num>    <num>    <num>    <num>
#>   1:     32.0     36.4     41.0     54.0
#>   2:     50.6     56.4     62.0     76.0
#>   3:     59.0     66.0     71.2     90.0
#>   4:     70.0     76.0     82.0     99.1
#>   5:     78.0     87.0     95.5    116.0
#>  ---                                    
#> 130:   2518.6   2861.0   3262.0   4133.1
#> 131:   2316.4   2589.6   2939.2   3684.5
#> 132:   2521.6   2854.0   3162.2   4254.1
#> 133:   2215.0   2642.2   2999.2   4066.5
#> 134:   1877.2   2294.0   2600.2   3461.8
#> 
#> 
#> $reported_cases
#> Index: <region>
#>            date confirm   region
#>          <Date>   <num>   <char>
#>   1: 2020-02-22      14 testland
#>   2: 2020-02-23      62 testland
#>   3: 2020-02-24      53 testland
#>   4: 2020-02-25      97 testland
#>   5: 2020-02-26      93 testland
#>  ---                            
#> 116: 2020-04-17    3786 realland
#> 117: 2020-04-18    3493 realland
#> 118: 2020-04-19    3491 realland
#> 119: 2020-04-20    3047 realland
#> 120: 2020-04-21    2256 realland
#> 
#> $high_plots
#> $high_plots$infections

#> 
#> $high_plots$reports

#> 
#> $high_plots$R

#> 
#> $high_plots$growth_rate

#> 
#> 
#> $plots
#> $plots$infections

#> 
#> $plots$reports

#> 
#> $plots$R

#> 
#> $plots$growth_rate

#> 
#> 
```
