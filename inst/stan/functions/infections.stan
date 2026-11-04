/**
 * Infection Modeling Functions
 *
 * This group of functions handles the generation, calculation, and backcalculation
 * of infection time series in the model. These functions implement the core
 * epidemiological dynamics, including the renewal equation approach.
 *
 * @ingroup infections_estimation
 */

/**
 * Calculate infectiousness for a single time point
 *
 * This function computes the weighted sum of past infections with the generation
 * time distribution to determine the current infectiousness.
 *
 * @param infections Vector of infection counts
 * @param gt_rev_pmf Vector of reversed generation time PMF
 * @param seeding_time Number of time steps used for seeding
 * @param index Current time index (relative to seeding_time)
 * @return The infectiousness at the specified time point
 *
 * @par Example
 * With `infections = {10, 20, 30, 40, 50}`, `gt_rev_pmf = {0.2, 0.5, 0.3}`,
 * `seeding_time = 0` and `index = 3`, the infectiousness is the weighted
 * sum of the three most recent infections up to time 3:
 * `update_infectiousness(infections, gt_rev_pmf, 0, 3)` returns
 * `10 * 0.2 + 20 * 0.5 + 30 * 0.3 = 21`.
 *
 * @ingroup infections_estimation
 */
real update_infectiousness(vector infections, vector gt_rev_pmf,
                           int seeding_time, int index){
  int gt_length = num_elements(gt_rev_pmf);
  // work out where to start the convolution of past infections with the
  // generation time distribution: (current_time - maximal generation time) if
  // that is >= 1, otherwise 1
  int inf_start = max(1, (index + seeding_time - gt_length + 1));
  // work out where to end the convolution: current_time
  int inf_end = (index + seeding_time);
  // number of indices of the generation time to sum over
  // (inf_end - inf_start + 1)
  int pmf_accessed = min(gt_length, index + seeding_time);
  // calculate the elements of the convolution
  real new_inf = dot_product(
    infections[inf_start:inf_end], tail(gt_rev_pmf, pmf_accessed)
  );
  return(new_inf);
}

/**
 * Apply process noise to the expected number of new infections
 *
 * Draws new infections from a log-normal approximation to either a Poisson
 * or a negative binomial distribution, using a non-centred
 * parameterisation with `eta ~ std_normal()`. The log-normal is chosen to
 * match the mean and variance of the target distribution. The Poisson
 * variance is `expected` and the negative binomial variance is
 * `expected + expected^2 * overdispersion^2`, matching the parameterisation
 * of the reporting overdispersion. With
 * `s2 = log(1 + variance / expected^2)` new infections are
 * `exp(log(expected) - s2 / 2 + sqrt(s2) * eta)`, which keeps them positive.
 *
 * @param expected Expected number of new infections
 * @param eta Standard normal noise term
 * @param infection_process Process model (0 = deterministic, 1 = Poisson,
 * 2 = negative binomial)
 * @param overdispersion Overdispersion of the negative binomial (one over
 * the square root of `phi`); ignored unless `infection_process` is 2
 * @return The number of new infections. Returned unchanged if
 * `infection_process` is 0 or `expected` is not positive.
 *
 * @par Example
 * With `expected = 100`, `eta = 0` and a Poisson process,
 * `s2 = log(1.01)` so `apply_infection_noise(100, 0, 1, 0)` returns the
 * log-normal median `100 / sqrt(1.01)`, approximately 99.5.
 *
 * @ingroup infections_estimation
 */
real apply_infection_noise(real expected, real eta, int infection_process,
                           real overdispersion) {
  if (infection_process == 0 || expected <= 0) {
    return(expected);
  }
  // squared coefficient of variation of the Poisson or negative binomial
  real cv2 = inv(expected);
  if (infection_process == 2) {
    cv2 += square(overdispersion);
  }
  real s2 = log1p(cv2);
  return(exp(log(expected) - s2 / 2 + sqrt(s2) * eta));
}

/**
 * @ingroup infections_estimation
 * @brief Generate infections using a renewal equation approach
 *
 * This function implements the renewal equation to generate a time series of
 * infections based on reproduction numbers and the generation time distribution.
 * It can also account for population depletion if a population size is specified.
 *
 * @param R Vector of reproduction numbers
 * @param uot Unobserved time (seeding time)
 * @param gt_rev_pmf Vector of reversed generation time PMF
 * @param initial_infections Array of initial infection values
 * @param pop Initial susceptible population (0 for unlimited)
 * @param use_pop Population adjustment mode (0=none, 1=forecast only, 2=all)
 * @param pop_floor Minimum susceptible population (floor to prevent instability)
 * @param ht Horizon time
 * @param obs_scale Whether to scale by fraction observed (1) or not (0)
 * @param fraction_observed Fraction of infections that are observed
 * @param initial_as_scale Whether initial infections are a scaling factor (1) or not (0)
 * @param infection_eta Vector of standard normal noise terms, one per
 * renewal time step (empty if infections are deterministic)
 * @param infection_process Process model (0 = deterministic, 1 = Poisson,
 * 2 = negative binomial), see apply_infection_noise()
 * @param infection_overdispersion Overdispersion of the negative binomial
 * process model
 * @return A vector of infection counts
 */
vector generate_infections(vector R, int uot, vector gt_rev_pmf,
                           array[] real initial_infections, real pop,
                           int use_pop, real pop_floor, int ht, int obs_scale, real fraction_observed,
                           int initial_as_scale, vector infection_eta,
                           int infection_process,
                           real infection_overdispersion) {
  // time indices and storage
  int ot = num_elements(R);
  int nht = ot - ht;
  int t = ot + uot;
  real exp_adj_Rt;
  vector[t] infections = rep_vector(0, t);
  vector[ot] cum_infections;
  vector[ot] infectiousness;
  real growth = R_to_r(R[1], gt_rev_pmf, 1e-3);
  // Initialise infections using daily growth
  if (initial_as_scale) {
    infections[1] = exp(initial_infections[1] - growth * uot);
    if (obs_scale) {
      infections[1] = infections[1] / fraction_observed;
    }
  } else {
    infections[1] = exp(initial_infections[1]);
  }
  if (uot > 1) {
    real exp_growth = exp(growth);
    for (s in 2:uot) {
      infections[s] = infections[s - 1] * exp_growth;
    }
  }
  // calculate cumulative infections
  if (use_pop) {
    cum_infections[1] = sum(infections[1:uot]);
  }
  // iteratively update infections
  for (s in 1:ot) {
    infectiousness[s] = update_infectiousness(infections, gt_rev_pmf, uot, s);
    if ((use_pop == 1 && s > nht) || use_pop == 2) {
      real susceptible = fmax(pop_floor, pop - cum_infections[s]);
      exp_adj_Rt = exp(-R[s] * infectiousness[s] / susceptible);
      infections[s + uot] = susceptible * fmax(0, 1 - exp_adj_Rt);
    } else{
      infections[s + uot] = R[s] * infectiousness[s];
    }
    if (infection_process) {
      infections[s + uot] = apply_infection_noise(
        infections[s + uot], infection_eta[s], infection_process,
        infection_overdispersion
      );
    }
    if (use_pop && s < ot) {
      cum_infections[s + 1] = cum_infections[s] + infections[s + uot];
    }
  }
  return(infections);
}

/**
 * Backcalculate infections from cases
 *
 * This function estimates infections by working backwards from observed cases,
 * applying noise to account for uncertainty in the process.
 *
 * @param shifted_cases Vector of shifted case counts
 * @param noise Vector of noise values
 * @param fixed Whether to use fixed (1) or variable (0) noise
 * @param prior Prior type to use (0: noise only, 1: cases * noise, 2: random walk)
 * @return A vector of infection counts
 *
 * @par Example
 * With `shifted_cases = {10, 20, 30}`, no noise (`noise = {0, 0, 0}`,
 * so `exp(noise) = 1`) and `prior = 1`, each case count is scaled by 1 and
 * offset by the `1e-5` numerical floor:
 * `deconvolve_infections(shifted_cases, noise, 0, 1)` returns
 * approximately `{10, 20, 30}`.
 *
 * @ingroup infections_estimation
 */
vector deconvolve_infections(vector shifted_cases, vector noise, int fixed,
                             int prior) {
  int t = num_elements(shifted_cases);
  vector[t] infections = rep_vector(1e-5, t);
  if (!fixed) {
    vector[t] exp_noise = exp(noise);
    if (prior == 1) {
      infections = infections + shifted_cases .* exp_noise;
    } else if (prior == 0) {
     infections = infections + exp_noise;
    } else if (prior == 2) {
      infections[1] = infections[1] + shifted_cases[1] * exp_noise[1];
      for (i in 2:t) {
        infections[i] = infections[i - 1] * exp_noise[i];
      }
    }
  } else {
    infections = infections + shifted_cases;
  }
  return(infections);
}
