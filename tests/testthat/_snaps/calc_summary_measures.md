# calc_summary_measures works as expected with default arguments

       type median mean      sd lower_90 lower_50 lower_20 upper_20 upper_50
    1:  car    5.5  5.5 3.02765     1.45     3.25      4.6      6.4     7.75
       upper_90
    1:     9.55

# calc_CrI works as expected when grouping

       type median mean      sd lower_90 lower_50 lower_20 upper_20 upper_50
    1:  car    5.5  5.5 3.02765     1.45     3.25      4.6      6.4     7.75
       upper_90
    1:     9.55

# calc_CrI works as expected when given a custom CrI list

       type median mean      sd lower_95 lower_40 lower_10 upper_10 upper_40
    1:  car    5.5  5.5 3.02765    1.225      3.7     5.05     5.95      7.3
       upper_95
    1:    9.775

