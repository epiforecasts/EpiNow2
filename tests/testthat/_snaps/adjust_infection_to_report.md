# adjust_infection_to_report can correctly handle a simple mapping

    Code
      adjust_infection_to_report(cases, delay_defs = list(incubation_def, delay_def))
    Output
                 date cases
        1: 2020-02-24     1
        2: 2020-02-25     3
        3: 2020-02-26     7
        4: 2020-02-27    11
        5: 2020-02-28    24
       ---                 
      124: 2020-06-26   233
      125: 2020-06-27   235
      126: 2020-06-28   261
      127: 2020-06-29   281
      128: 2020-06-30   266

# adjust_infection_to_report can correctly handle a mapping with a day
           of the week effect

    Code
      adjust_infection_to_report(cases, delay_defs = list(incubation_def, delay_def),
      reporting_effect = c(1.1, rep(1, 4), 0.95, 0.95))
    Output
                 date cases
        1: 2020-02-25     1
        2: 2020-02-26     8
        3: 2020-02-27    13
        4: 2020-02-28    27
        5: 2020-02-29    38
       ---                 
      123: 2020-06-26   234
      124: 2020-06-27   224
      125: 2020-06-28   238
      126: 2020-06-29   297
      127: 2020-06-30   267

