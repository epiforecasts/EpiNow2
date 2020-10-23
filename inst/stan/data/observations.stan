  int t;                             // unobserved time
  int rt;                            // observed time
  int horizon;                       // forecast horizon
  int<lower = 0> cases[rt - horizon];// observed cases
  vector<lower = 0>[t] shifted_cases;// median shifted smoothed cases
  