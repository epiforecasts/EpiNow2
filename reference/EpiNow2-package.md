# EpiNow2: Estimate and Forecast Real-Time Infection Dynamics

Estimates the time-varying reproduction number, rate of spread, and
doubling time using a renewal equation approach combined with Bayesian
inference via Stan. Supports Gaussian process and random walk priors for
modelling changes in transmission over time. Accounts for delays between
infection and observation (incubation period, reporting delays),
right-truncation in recent data, day-of-week effects, and observation
overdispersion. Can estimate relationships between primary and secondary
outcomes (e.g., cases to hospitalisations or deaths) and forecast both.
Runs across multiple regions in parallel. Based on Abbott et al. (2020)
[doi:10.12688/wellcomeopenres.16006.1](https://doi.org/10.12688/wellcomeopenres.16006.1)
and Gostic et al. (2020)
[doi:10.1101/2020.06.18.20134858](https://doi.org/10.1101/2020.06.18.20134858)
.

## See also

Useful links:

- <https://epiforecasts.io/EpiNow2/>

- <https://epiforecasts.io/EpiNow2/dev/>

- <https://github.com/epiforecasts/EpiNow2>

- Report bugs at <https://github.com/epiforecasts/EpiNow2/issues>

## Author

**Maintainer**: Sebastian Funk <sebastian.funk@lshtm.ac.uk>
([ORCID](https://orcid.org/0000-0002-2842-3406))

Authors:

- Sam Abbott <sam.abbott@lshtm.ac.uk>
  ([ORCID](https://orcid.org/0000-0001-8057-8037))

- Joel Hellewell <joel.hellewell@lshtm.ac.uk>
  ([ORCID](https://orcid.org/0000-0003-2683-0849))

- Katharine Sherratt <katharine.sherratt@lshtm.ac.uk>

- Katelyn Gostic <kgostic@uchicago.edu>

- Joe Hickson <joseph.hickson@metoffice.gov.uk>

- Hamada S. Badr <badr@jhu.edu>
  ([ORCID](https://orcid.org/0000-0002-9808-2344))

- Michael DeWitt <me.dewitt.jr@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-8940-1967))

- James M. Azam <james.azam@lshtm.ac.uk>
  ([ORCID](https://orcid.org/0000-0001-5782-7330))

- Adrian Lison <adrian.lison@bsse.ethz.ch>
  ([ORCID](https://orcid.org/0000-0002-6822-8437))

Other contributors:

- Robin Thompson <robin.thompson@lshtm.ac.uk> \[contributor\]

- Sophie Meakin <sophie.meakin@lshtm.ac.uk> \[contributor\]

- James Munday <james.munday@lshtm.ac.uk> \[contributor\]

- Nikos Bosse \[contributor\]

- Paul Mee <paul.mee@lshtm.ac.uk> \[contributor\]

- Peter Ellis <peter.ellis2013nz@gmail.com> \[contributor\]

- Pietro Monticone <pietro.monticone@edu.unito.it> \[contributor\]

- Lloyd Chapman <lloyd.chapman1@lshtm.ac.uk> \[contributor\]

- Andrew Johnson <andrew.johnson@arjohnsonau.com> \[contributor\]

- Kaitlyn Johnson <johnsonkaitlyne9@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-8011-0012)) \[contributor\]

- Adam Howes <adamthowes@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-2386-4031)) \[contributor\]
