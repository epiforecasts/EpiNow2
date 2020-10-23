
void report_lp(int[] cases, vector reports, real[] rep_phi,
                 int model_type, int horizon) {
  int t = num_elements(reports) - horizon;
  if (model_type) {
    target += neg_binomial_2_lpmf(cases | reports[1:t], rep_phi[model_type]);
  }else{
    target += poisson_lpmf(cases | reports[1:t]);
  }
}
