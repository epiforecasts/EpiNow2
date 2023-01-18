# report_cases can simulate infections forward

    Code
      reported_cases
    Output
      $samples
         sample       date value
          <int>     <Date> <int>
      1:      1 2020-02-23     2
      2:      1 2020-02-24     6
      3:      1 2020-02-25    13
      4:      1 2020-02-26    21
      5:      1 2020-02-27    33
      6:      1 2020-02-28    43
      7:      1 2020-02-29    66
      8:      1 2020-03-01   122
      9:      1 2020-03-02   129
      
      $summarised
               date median  mean    sd lower_90 lower_50 lower_20 upper_20 upper_50
             <Date>  <num> <num> <num>    <num>    <num>    <num>    <num>    <num>
      1: 2020-02-23      2     2    NA        2        2        2        2        2
      2: 2020-02-24      6     6    NA        6        6        6        6        6
      3: 2020-02-25     13    13    NA       13       13       13       13       13
      4: 2020-02-26     21    21    NA       21       21       21       21       21
      5: 2020-02-27     33    33    NA       33       33       33       33       33
      6: 2020-02-28     43    43    NA       43       43       43       43       43
      7: 2020-02-29     66    66    NA       66       66       66       66       66
      8: 2020-03-01    122   122    NA      122      122      122      122      122
      9: 2020-03-02    129   129    NA      129      129      129      129      129
         upper_90
            <num>
      1:        2
      2:        6
      3:       13
      4:       21
      5:       33
      6:       43
      7:       66
      8:      122
      9:      129
      

