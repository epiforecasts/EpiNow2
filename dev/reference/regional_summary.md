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
#> INFO [2026-01-29 15:37:36] No summary directory specified so returning summary output
#> $latest_date
#> [1] "2020-04-21"
#> 
#> $results
#> $results$estimates
#> $results$estimates$summarised
#> Indices: <variable>, <region>
#>        region       date       variable       parameter strat     type
#>        <char>     <Date>         <char>          <char> <int>   <char>
#>   1: testland 2020-02-22              R               R    NA estimate
#>   2: testland 2020-02-23              R               R    NA estimate
#>   3: testland 2020-02-24              R               R    NA estimate
#>   4: testland 2020-02-25              R               R    NA estimate
#>   5: testland 2020-02-26              R               R    NA estimate
#>  ---                                                                  
#> 560: realland 2020-04-24 reported_cases imputed_reports    NA forecast
#> 561: realland 2020-04-25 reported_cases imputed_reports    NA forecast
#> 562: realland 2020-04-26 reported_cases imputed_reports    NA forecast
#> 563: realland 2020-04-27 reported_cases imputed_reports    NA forecast
#> 564: realland 2020-04-28 reported_cases imputed_reports    NA forecast
#>           median        mean           sd    lower_90    lower_50    lower_20
#>            <num>       <num>        <num>       <num>       <num>       <num>
#>   1:    2.184287    2.189490   0.10658258    2.028976    2.121626    2.163104
#>   2:    2.144420    2.152334   0.09674741    2.002565    2.092390    2.123740
#>   3:    2.106303    2.113817   0.09014187    1.974097    2.053383    2.078273
#>   4:    2.064986    2.074099   0.08602712    1.944435    2.014499    2.044069
#>   5:    2.027879    2.033331   0.08352446    1.913818    1.971920    2.004880
#>  ---                                                                         
#> 560: 2885.000000 2937.105000 767.07727885 1801.650000 2396.500000 2688.400000
#> 561: 2408.000000 2477.175000 660.01500240 1506.950000 1966.250000 2231.800000
#> 562: 2754.500000 2855.470000 901.57105150 1538.900000 2171.000000 2510.600000
#> 563: 2379.000000 2564.695000 786.53940343 1559.500000 1969.000000 2225.800000
#> 564: 2123.000000 2198.985000 733.84611149 1159.200000 1719.500000 1947.000000
#>         upper_20    upper_50    upper_90
#>            <num>       <num>       <num>
#>   1:    2.216040    2.263164    2.360740
#>   2:    2.173351    2.219161    2.301823
#>   3:    2.134551    2.177657    2.257131
#>   4:    2.090072    2.137158    2.215881
#>   5:    2.049114    2.091113    2.167830
#>  ---                                    
#> 560: 3152.800000 3375.750000 4158.350000
#> 561: 2575.600000 2875.250000 3650.650000
#> 562: 3002.000000 3369.500000 4378.600000
#> 563: 2654.200000 3024.250000 4048.550000
#> 564: 2310.400000 2550.500000 3651.900000
#> 
#> 
#> 
#> $summarised_results
#> $summarised_results$table
#>      Region New infections per day Expected change in reports
#>      <char>                 <char>                     <fctr>
#> 1: realland     1781 (633 -- 4186)          Likely decreasing
#> 2: testland     1905 (805 -- 4772)          Likely decreasing
#>    Effective reproduction no.           Rate of growth
#>                        <char>                   <char>
#> 1:         0.89 (0.68 -- 1.1)  -0.032 (-0.095 -- 0.02)
#> 2:         0.91 (0.73 -- 1.1) -0.027 (-0.082 -- 0.027)
#>    Doubling/halving time (days)
#>                          <char>
#> 1:             -22 (34 -- -7.3)
#> 2:             -26 (26 -- -8.4)
#> 
#> $summarised_results$data
#>      region           estimate  median    mean      sd lower_90 lower_50
#>      <fctr>             <char>   <num>   <num>   <num>    <num>    <num>
#> 1: testland 1905 (805 -- 4772) 1905.00 2153.00 1241.00   805.00  1380.00
#> 2: realland 1781 (633 -- 4186) 1781.00 2048.00 1117.00   633.00  1311.00
#> 3: testland 0.91 (0.73 -- 1.1)    0.91    0.90    0.12     0.73     0.83
#> 4: realland 0.89 (0.68 -- 1.1)    0.89    0.89    0.12     0.68     0.82
#>    lower_20 upper_20 upper_50 upper_90                     metric
#>       <num>    <num>    <num>    <num>                     <fctr>
#> 1:  1666.00  2122.00  2590.00   4772.0     New infections per day
#> 2:  1595.00  2090.00  2538.00   4186.0     New infections per day
#> 3:     0.88     0.93     0.97      1.1 Effective reproduction no.
#> 4:     0.86     0.93     0.97      1.1 Effective reproduction no.
#>    Expected change in reports prob_control
#>                        <fctr>       <list>
#> 1:          Likely decreasing         0.83
#> 2:          Likely decreasing          0.8
#> 3:          Likely decreasing         0.83
#> 4:          Likely decreasing          0.8
#> 
#> $summarised_results$regions_by_inc
#> [1] "testland" "realland"
#> 
#> 
#> $summary_plot

#> 
#> $summarised_measures
#> $summarised_measures$rt
#>        region       date parameter strat     type    median      mean        sd
#>        <char>     <Date>    <char> <int>   <char>     <num>     <num>     <num>
#>   1: realland 2020-02-22         R    NA estimate 2.2049351 2.2138416 0.1295158
#>   2: realland 2020-02-23         R    NA estimate 2.1628476 2.1734210 0.1195927
#>   3: realland 2020-02-24         R    NA estimate 2.1214420 2.1318778 0.1122113
#>   4: realland 2020-02-25         R    NA estimate 2.0797718 2.0893933 0.1068067
#>   5: realland 2020-02-26         R    NA estimate 2.0389297 2.0461299 0.1027379
#>  ---                                                                           
#> 130: testland 2020-04-24         R    NA forecast 0.9050343 0.9048163 0.1203174
#> 131: testland 2020-04-25         R    NA forecast 0.9050343 0.9048163 0.1203174
#> 132: testland 2020-04-26         R    NA forecast 0.9050343 0.9048163 0.1203174
#> 133: testland 2020-04-27         R    NA forecast 0.9050343 0.9048163 0.1203174
#> 134: testland 2020-04-28         R    NA forecast 0.9050343 0.9048163 0.1203174
#>       lower_90  lower_50  lower_20  upper_20  upper_50 upper_90
#>          <num>     <num>     <num>     <num>     <num>    <num>
#>   1: 2.0292447 2.1252332 2.1745094 2.2389873 2.2855863 2.430352
#>   2: 2.0053999 2.0957583 2.1311870 2.1889461 2.2386155 2.381127
#>   3: 1.9748117 2.0573242 2.0937232 2.1485770 2.1930990 2.332049
#>   4: 1.9352677 2.0176237 2.0573835 2.1056189 2.1434051 2.282499
#>   5: 1.8926357 1.9743047 2.0180830 2.0615800 2.0978918 2.240053
#>  ---                                                           
#> 130: 0.7276063 0.8305671 0.8847349 0.9272735 0.9721334 1.113391
#> 131: 0.7276063 0.8305671 0.8847349 0.9272735 0.9721334 1.113391
#> 132: 0.7276063 0.8305671 0.8847349 0.9272735 0.9721334 1.113391
#> 133: 0.7276063 0.8305671 0.8847349 0.9272735 0.9721334 1.113391
#> 134: 0.7276063 0.8305671 0.8847349 0.9272735 0.9721334 1.113391
#> 
#> $summarised_measures$growth_rate
#>        region       date parameter strat     type      median        mean
#>        <char>     <Date>    <char> <int>   <char>       <num>       <num>
#>   1: realland 2020-02-23         r    NA estimate  0.24642046  0.24567281
#>   2: realland 2020-02-24         r    NA estimate  0.23873611  0.23808137
#>   3: realland 2020-02-25         r    NA estimate  0.23008418  0.22969389
#>   4: realland 2020-02-26         r    NA estimate  0.22123275  0.22101239
#>   5: realland 2020-02-27         r    NA estimate  0.21188251  0.21211660
#>  ---                                                                     
#> 128: testland 2020-04-24         r    NA forecast -0.02654710 -0.02740693
#> 129: testland 2020-04-25         r    NA forecast -0.02654198 -0.02743037
#> 130: testland 2020-04-26         r    NA forecast -0.02653824 -0.02745253
#> 131: testland 2020-04-27         r    NA forecast -0.02653589 -0.02747235
#> 132: testland 2020-04-28         r    NA forecast -0.02653470 -0.02748941
#>               sd    lower_90    lower_50    lower_20    upper_20     upper_50
#>            <num>       <num>       <num>       <num>       <num>        <num>
#>   1: 0.011553636  0.22766567  0.23627354  0.24296215  0.24927555  0.254815040
#>   2: 0.010423616  0.22085349  0.22994658  0.23459431  0.24205922  0.246119242
#>   3: 0.009674003  0.21274432  0.22261193  0.22725307  0.23287270  0.237237268
#>   4: 0.009216050  0.20531988  0.21508906  0.21885134  0.22334342  0.227937246
#>   5: 0.008904631  0.19562401  0.20604698  0.20978129  0.21483127  0.218880604
#>  ---                                                                         
#> 128: 0.034104299 -0.08085388 -0.04773931 -0.03261762 -0.02048912 -0.007756095
#> 129: 0.034271145 -0.08155502 -0.04781847 -0.03262071 -0.02035925 -0.007681798
#> 130: 0.034391500 -0.08184558 -0.04787422 -0.03260894 -0.02026943 -0.007633468
#> 131: 0.034484877 -0.08199478 -0.04791398 -0.03260046 -0.02022123 -0.007599391
#> 132: 0.034559069 -0.08211174 -0.04794139 -0.03259634 -0.02018291 -0.007574850
#>        upper_90
#>           <num>
#>   1: 0.26299273
#>   2: 0.25339695
#>   3: 0.24458760
#>   4: 0.23528659
#>   5: 0.22560042
#>  ---           
#> 128: 0.02642144
#> 129: 0.02671093
#> 130: 0.02690073
#> 131: 0.02703714
#> 132: 0.02714005
#> 
#> $summarised_measures$cases_by_infection
#>        region       date  parameter strat     type median   mean     sd
#>        <char>     <Date>     <char> <int>   <char>  <num>  <num>  <num>
#>   1: realland 2020-02-22 infections    NA estimate  208.3  208.4   13.3
#>   2: realland 2020-02-23 infections    NA estimate  266.9  266.4   16.2
#>   3: realland 2020-02-24 infections    NA estimate  337.9  338.0   19.8
#>   4: realland 2020-02-25 infections    NA estimate  426.3  425.2   24.0
#>   5: realland 2020-02-26 infections    NA estimate  532.3  530.4   29.0
#>  ---                                                                   
#> 130: testland 2020-04-24 infections    NA forecast 2117.5 2257.1  944.4
#> 131: testland 2020-04-25 infections    NA forecast 2061.9 2226.9 1016.1
#> 132: testland 2020-04-26 infections    NA forecast 2008.2 2199.7 1089.4
#> 133: testland 2020-04-27 infections    NA forecast 1956.1 2175.4 1164.5
#> 134: testland 2020-04-28 infections    NA forecast 1905.3 2153.8 1241.7
#>      lower_90 lower_50 lower_20 upper_20 upper_50 upper_90
#>         <num>    <num>    <num>    <num>    <num>    <num>
#>   1:    187.8    199.0    205.5    212.1    216.3    229.2
#>   2:    242.6    255.2    262.3    271.0    276.6    289.5
#>   3:    307.1    324.0    333.3    342.8    351.3    368.4
#>   4:    386.9    408.3    417.6    431.9    442.6    465.2
#>   5:    480.5    511.6    521.5    538.7    550.7    581.1
#>  ---                                                      
#> 130:   1120.0   1646.2   1906.7   2270.4   2724.1   4288.7
#> 131:   1037.1   1579.4   1839.3   2235.1   2686.7   4424.2
#> 132:    954.4   1513.4   1779.7   2199.3   2654.1   4543.8
#> 133:    876.8   1447.7   1721.1   2160.2   2621.9   4656.6
#> 134:    805.5   1380.3   1666.5   2122.7   2590.1   4772.4
#> 
#> $summarised_measures$cases_by_report
#>        region       date       parameter strat     type median   mean    sd
#>        <char>     <Date>          <char> <int>   <char>  <num>  <num> <num>
#>   1: realland 2020-02-22 imputed_reports    NA estimate   34.0   35.5   9.9
#>   2: realland 2020-02-23 imputed_reports    NA estimate   52.5   54.0  12.9
#>   3: realland 2020-02-24 imputed_reports    NA estimate   65.0   66.8  15.5
#>   4: realland 2020-02-25 imputed_reports    NA estimate   73.0   73.1  15.8
#>   5: realland 2020-02-26 imputed_reports    NA estimate   84.0   86.1  20.0
#>  ---                                                                       
#> 130: testland 2020-04-24 imputed_reports    NA forecast 2854.5 2981.7 750.9
#> 131: testland 2020-04-25 imputed_reports    NA forecast 2448.5 2544.1 669.2
#> 132: testland 2020-04-26 imputed_reports    NA forecast 2825.0 2950.3 837.8
#> 133: testland 2020-04-27 imputed_reports    NA forecast 2485.5 2573.2 742.9
#> 134: testland 2020-04-28 imputed_reports    NA forecast 2116.0 2212.1 730.0
#>      lower_90 lower_50 lower_20 upper_20 upper_50 upper_90
#>         <num>    <num>    <num>    <num>    <num>    <num>
#>   1:     22.0     28.0     31.6     36.0     42.0     53.1
#>   2:     34.0     45.8     50.0     56.0     61.0     78.1
#>   3:     43.0     57.0     61.0     69.4     77.0     93.0
#>   4:     50.0     62.0     69.6     77.4     84.2     99.0
#>   5:     54.0     71.0     79.0     91.0    102.0    117.1
#>  ---                                                      
#> 130:   1976.8   2425.0   2705.2   3041.4   3458.2   4348.5
#> 131:   1639.6   2048.0   2273.4   2645.4   2987.8   3783.1
#> 132:   1740.7   2461.5   2635.6   3006.2   3388.8   4372.6
#> 133:   1556.6   2079.8   2294.4   2626.2   3037.8   4019.5
#> 134:   1308.9   1721.8   1920.6   2303.4   2542.2   3645.5
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
