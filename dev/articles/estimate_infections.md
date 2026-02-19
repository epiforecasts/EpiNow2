# Model definition: estimate_infections()

## Infection model

[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
supports a range of model formulations. Here we describe the most
commonly used and highlight other options. The two main models for how
new infections arise in the model are a *renewal equation model* and a
*non-mechanistic infection model*. The initialisation of both of these
models involves estimating an initial infection trajectory during a
`seeding_time` $t_{seed}$ (set to the mean of modelled delays from
infection to observation) that precedes the first observation at time
$t = 0$.

### Renewal equation model

This is the default model and is used when `rt != NULL`. New infections
are generated at discrete time steps of one day via the renewal
equation^(\[1\]). These infections are then mapped to observations via
discrete convolutions with delay distributions.

#### Initialisation

The model is initialised before the first observed data point by
assuming constant exponential growth for the mean of modelled delays
from infection to case report (called `seeding_time` $t_{seed}$ in the
model):

$$\begin{array}{rlr}
I_{- t_{seed}} & {= \frac{1}{\xi}\exp\left( \iota - rt_{seed} \right)\iota} & {\sim {Normal}\left( \iota_{0},2 \right)} \\
\iota_{0} & {= \max\left( 0,\log\left( C_{init} \right) \right)} & 
\end{array}$$

where $I_{t}$ is the number of latent infections on day $t$, $r$ is an
estimate of the initial growth rate, $\xi$ is the proportion reported
(see [Delays and scaling](#delays-and-scaling)), is a scaling factor and
$C_{init}$ is the mean of the first 7 days of cases (or all cases if
fewer than 7 days of data are available).

The initial growth rate $r$ is estimated from the first estimated value
of the reproduction number $R_{t}$ by solving the linear system
\[wallinga2006how\]

$$M( - r) - 1/R_{0} = 0.$$

where

$$M(r) = \sum\limits_{i = 1}^{n}g_{i}e^{ri}.$$

is the moment generating function of the discretised generation time
distribution (see [Infections](#infections)).

#### Infections

For the time window of the observed data and beyond infections are then
modelled by weighting previous infections with the generation time and
scaling by the instantaneous reproduction number:

$$I_{t} = R_{t}\sum\limits_{\tau = 1}^{g_{max}}g\left( \tau|\theta_{g} \right)I_{t - \tau} = R_{t}\lambda_{t}$$

where $g_{\tau} = g\left( \tau|\theta_{g} \right)$ is the discretised
distribution of generation times with parameters $\theta_{g}$ and
maximum $g_{max}$, and
$\lambda_{t} = \sum_{\tau = 1}^{g_{max}}g\left( \tau|\theta_{g} \right)I_{t - \tau}$
is the total infectiousness at time $t$. Generation times can be
specified as coming from a distribution with uncertainty by giving mean
and standard deviations of normal priors of the distributional
parameters. By default this prior is weighted by default by the number
of observations although this can be changed by the user. It is
truncated to be positive where relevant for the given distribution.
Alternatively, generation times can be specified as coming from a given
distribution with set parameters.

The distribution of generation times $g$ here represents the probability
that somebody who became infectious on day 0 and who infects someone
else during their course of infection does so on day $\tau > 0$,
assuming that infection cannot happen on day 0. If not given this
defaults to a fixed generation time of 1, in which case $R_{t}$
represents the exponential of the daily growth rate of infections.

#### Time-varying reproduction number

Different options are available for setting a prior for $R_{t}$, the
instantaneous reproduction number at time $t$. The default prior is an
approximate zero-mean Gaussian Process (GP) for the first differences in
time on the log scale,

$$\log R_{t} - \log R_{t - 1} \sim {GP}_{t}$$

More details on the mathematical form of the GP approximation and
implementation are given in the [Gaussian Process implementation
details](https://epiforecasts.io/EpiNow2/dev/articles/gaussian_process_implementation_details.md)
vignette. Other choices for the prior of $R_{t}$ are available such as a
GP prior for the difference between $R_{t}$ and its mean value (implying
that in the absence of data $R_{t}$ will revert to its prior mean rather
than the last value with robust support from the data).

$$\log R_{t} - \log R_{0} \sim {GP}_{t}$$

or, as a specific case of a Gaussian Process, a random walk of arbitrary
length $w$.

$$\begin{aligned}
{\log R_{t \div w}} & {\sim {Normal}\left( R_{t \div {(w - 1)}},\sigma_{R} \right)} \\
\sigma_{R} & {\sim {HalfNormal}(0,0.1)}
\end{aligned}$$

where $\div$ indicates interval-valued division (i.e. the floor of the
division), such that for example $w = 1$ indicates a daily and $w = 7$ a
weekly random walk.

The choice of prior for the time-varying reproduction number impact
run-time, smoothness of the estimates and real-time behaviour and may
alter the best use-case for the model.

The prior distribution of the initial reproduction number $R_{0}$ can be
set by the user. By default this is a log-normal distribution with mean
1 and standard deviation 1.

$$R_{0} \sim {LogNormal}\left( - 1/2\log(2),\sqrt{\log(2)} \right)$$

The simplest possible process model option is to use no time-varying
prior and rely on just the intial fixed reproduction number $R_{0}$.

#### Beyond the end of the observation period

Beyond the end of the observation period ($T$), by default, the Gaussian
process is assumed to continue. However, here again there are a range of
options. These include fixing transmission dynamics (optionally this can
also be done before the end of the observation period), and adjusting
for depletion of the susceptible population.

#### Adjusting for susceptible population depletion

The model can account for the depletion of susceptible individuals over
time, which reduces transmission potential even if the intrinsic
reproduction number remains constant. This is controlled via the `pop`,
`pop_period`, and `pop_floor` parameters in
[`rt_opts()`](https://epiforecasts.io/EpiNow2/dev/reference/rt_opts.md).

When a population size is specified (via the `pop` parameter), the model
adjusts both infections and the reproduction number estimates to account
for susceptible depletion. The adjustment uses the following formula for
infections:

$$I_{t} = S_{t}\left( 1 - \exp\left( \frac{- R_{t}\lambda_{t}}{S_{t}} \right) \right),$$

where $S_{t} = \max\left( N_{\text{floor}},N - I_{t - 1}^{c} \right)$ is
the susceptible population at time $t$, $N$ is the initial population
size, $I_{t}^{c} = \sum_{s < t}I_{s}$ are cumulative infections by
$t - 1$, $R_{t}$ is the unadjusted reproduction number, and
$\lambda_{t}$ is the total infectiousness. The parameter
$N_{\text{floor}}$ (controlled by `pop_floor`, default 1.0) provides a
numerical stability floor and can be interpreted as representing ongoing
importation.

The adjustment can be applied in two ways, controlled by the
`pop_period` parameter: - `"forecast"` (default): adjustment applied
only to forecasts beyond the observation period - `"all"`: adjustment
applied to both the observation period and forecasts

When population adjustment is enabled, the returned reproduction number
estimates (`R`) are back-calculated from the adjusted infections to
reflect the effective reproduction occurring given the current
susceptible population:

$$R_{t}^{\text{adjusted}} = \frac{I_{t}}{\lambda_{t}}$$

where $I_{t}$ are the adjusted infections and $\lambda_{t}$ is the total
infectiousness. This gives an effective reproduction number that is
consistent with the infection dynamics. The unadjusted reproduction
number (representing transmission in a fully susceptible population) is
also returned as `R_unadjusted` for reference. This adjustment approach
is based on the one implemented in the `epidemia` R package^(\[2\]).

### Non-Mechanistic infection model

This is an alternative model that can be used by setting `rt = NULL`
that assumes less epidemiological mechanism by directly modelling
infections on a log scale with a range of process models. By default,
this uses a Gaussian Process prior for the number of new infections each
day (on the log scale) although alternatively infections can be
estimated using a prior based on a fixed backwards mapping of observed
cases. In general, these model options will be more computationally
efficient than the renewal process model but may be less robust due to
the lack of an epidemiological process model (i.e. more dependence is
placed on the assumptions of the Gaussian process prior).

#### Initialisation

In order to initialise this model, an initial estimate $I_{est}$ of the
infection trajectory is first created by first shifting observations
back in time by $t_{seed}$ and then smoothing the observation data with
a moving average of window size $z$ (default: $z = 14$), allocated to
the centre of the window:

$$\begin{array}{r}
{I_{{est},t < T - t_{seed}} = \frac{1}{z}\sum\limits_{\max{( - t_{seed},t - z/2)}}^{\min{(t_{obs},t + z/2)}}I_{{obs},t + t_{seed}}}
\end{array}$$

where $T$ is the day of the last observation, $z/2$ is rounded up to the
nearest integer in the limits of the sum, and $I_{obs,t < 0} = 0$. Any
date with $I_{{est},t} = 0$ cases following this procedure is then
allocated 1 case to facilitate further processing in the model.

For any times $t > T - t_{seed}$ the number of infections is then
estimated by fitting an exponential curve to the final week of data and
extrapolating this until the end of the forecast horizon.

#### Infections

By default, a Gaussian Process prior is used for the number of
infections, resulting in smoother estimates of the infection curve. In
this case, as in the renewal equation model there are two alternative
formulations available. The default uses an approximate zero-mean GP for
the differences between modelled infections and the initial estimate,

$$\log I_{t} - \log I_{{est},t} \sim {GP}_{t}$$

Alternatively, one can use is an approximate zero-mean Gaussian Process
(GP) for the first differences in time on the log scale,

$$\log I_{t} - \log I_{t - 1} \sim {GP}_{t}$$

with $\log I_{0} - \log I_{{est},0} \sim {GP}_{0}$

More details on the mathematical form of the Gaussian process
approximation are given in the [Gaussian Process implementation
details](https://epiforecasts.io/EpiNow2/dev/articles/gaussian_process_implementation_details.md)
vignette.

As for the renewal equation model, the Gaussian process can be replaced
by a random walk of arbitrary length $w$.

When using a fixed shift from infections to reported cases there is no
process model and so $I_{est}$ is used as the estimated infection curve
(potentially scaled to take into account underreporting, see section
[Delays and scaling](#delays-and-scaling)).

#### Time-varying reproduction number

In this model there is no prior on the time-varying reproduction number.
Instead, this is calculated from the renewal equation as a
post-processing step

$$R_{t} = \frac{I_{t}}{\sum\limits_{\tau = 1}^{g_{max}}g\left( \tau|\mu_{g},\sigma_{g} \right)I_{t - \tau}}$$

and optionally smoothed using a centred rolling mean with a window size
that can be set by the user.

#### Beyond the end of the observation period

Beyond the end of the observation period, by default, if using a
Gaussian process it is assumed to continue. Alternatively, incidence can
be fixed at ether the estimate at $T$ or a less recent, more certain
estimate.

## Delays and scaling

If infections are observed with a delay (for example, the incubation
period if based on symptomatic cases, and any delay from onset to
report), they are convolved in the model to infections at the time scale
of observations $D_{t}$ using delay distributions $\xi$, scaled by an
underreporting factor \$(which is 1 if all infections are observed).
This model can be defined mathematically as follows,

$$D_{t} = \xi\sum\limits_{\tau = 0}^{\xi_{max}}\xi\left( \tau|\mu_{\xi},\sigma_{\xi} \right)I_{t - \tau}$$

where $\xi\left( \tau|\theta_{\tau} \right)$ is the combined discrete
distribution of delays with parameters $\theta_{\xi}$ and maximum
$\xi_{max}$.

Delays can either be specified as coming from a distribution with
uncertainty by giving mean and standard deviations of normal priors for
the distributional parameters, weighted by default by the number of
observations and truncated to be positive where relevant for the given
distribution; or they can be specified as coming from a distribution
with given parameters, or as fixed values.

The scaling factor \$represents the proportion of cases that are
ultimately reported, which by default is set to 1 (i.e. no
underreporting) but can instead be estimated with a given prior
distribution.

## Observation model

The modelled counts $D_{t}$ are related to observations $C_{t}$. By
default this is assumed to follow a negative binomial distribution with
overdispersion $\varphi$ (alternatively it can be modelled as a Poisson,
in which case $\varphi$ is not used):

$$\begin{aligned}
C_{t} & {\sim {NegBinom}\left( \omega_{(t{\mspace{8mu}\operatorname{mod}\ n_{\omega}})}D_{t},\varphi \right)}
\end{aligned}$$

where $\omega_{t{\mspace{8mu}\operatorname{mod}\ n_{\omega}}}$ is a
daily reporting effect of cyclicity $n_{\omega}$. If $n_{\omega} = 7$
this corresponds to a day-of-the-week reporting effect.

In the model overdispersion is characterised by a “dispersion”
parameter, which is defined as one over the square root of $varphi$.
With this, the priors for the observation model are

$$\begin{aligned}
\frac{\omega}{n_{\omega}} & {\sim {Dirichlet}(1,\ldots,1)} \\
\frac{1}{\sqrt{\varphi}} & {\sim {HalfNormal}(0,1)}
\end{aligned}$$

### Truncation

The model supports counts that are right-truncated, i.e. reported with a
delay leading to recent counts being subject to future upwards revision.
Denoting the final truncated counts with $D_{t}^{*}$ they are obtained
form the final modelled cases $D_{t}$ by applying a given discrete
truncation distribution
$\zeta\left( \tau|\mu_{\zeta},\sigma_{\zeta} \right)$ with cumulative
mass function $Z\left( \tau|\mu_{\zeta} \right)$:

$$D_{t}^{*} = Z\left( T - t|\mu_{Z},\sigma_{Z} \right)D_{t}$$

If truncation is applied, the modelled cases $D_{t}$ are replaced by the
truncated counts before confronting them with observations $C_{t}$ as
described above.

## References

1\.

Fraser, C. (2007). Estimating individual and household reproduction
numbers in an emerging epidemic. *PLOS ONE*, *2*(8), 1–12.
<https://doi.org/10.1371/journal.pone.0000758>

2\.

Bhatt, S., Ferguson, N., Flaxman, S., Gandy, A., Mishra, S., & Scott, J.
A. (n.d.). *Semi-Mechanistic Bayesian modeling of COVID-19 with Renewal
Processes*. 14.
