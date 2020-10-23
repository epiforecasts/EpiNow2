
vector day_of_week_effect(vector reports, int[] day_of_week, vector effect) {
  int t = num_elements(reports);
  // scale day of week effect
  vector[7] scaled_effect = 7 * effect;
  vector[t] scaled_reports;
  for (s in 1:t) {
    // add reporting effects (adjust for simplex scale)
    scaled_reports[s] = reports[s] * scaled_effect[day_of_week[s]];
   }
  return(scaled_reports);
}

   
void report_lp(int[] cases, vector reports, 
               real[] rep_phi, int phi_prior,
               int model_type, int horizon) {
  int t = num_elements(reports) - horizon;
  if (model_type) {
    //overdispersion
    rep_phi[model_type] ~ exponential(phi_prior);
    target += neg_binomial_2_lpmf(cases | reports[1:t], rep_phi[model_type]);
  }else{
    target += poisson_lpmf(cases | reports[1:t]);
  }
}
