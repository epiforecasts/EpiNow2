# calc_CrI works as expected with default arguments

       . lower_90 lower_50 lower_20 upper_20 upper_50 upper_90
    1: .     1.45     3.25      4.6      6.4     7.75     9.55

# calc_CrI works as expected when grouping

       type lower_90 lower_50 lower_20 upper_20 upper_50 upper_90
    1:  car     1.45     3.25      4.6      6.4     7.75     9.55

# calc_CrI works as expected when given a custom CrI list

       . lower_95 lower_40 lower_10 upper_10 upper_40 upper_95
    1: .    1.225      3.7     5.05     5.95      7.3    9.775

