# Model definition: estimate_secondary()

**This is a work in progress. Please consider submitting a PR to improve
it.**

This model is based on a discrete convolution of primary cases, scaled
based on the fraction (here described as the secondary fraction but
depending on application potentially being the case fatality ratio, case
hospitalisation ratio, or the hospitalisation fatality ratio), and a
delay distribution that is assumed to follow a discretised daily log
normal distribution. This model can be thought of as a discrete time
ordinary differential equation approximation generalised to log normal,
rather than exponential, delay distributions.

We generalise this simple model beyond the incidence cases to also
include prevalence indicators (for example hospital admissions and
occupancy) where the secondary notifications can be thought of as
depending on secondary notifications from the previous timestep, scaled
current primary notifications, and minus scaled historic primary
notifications weighted by some delay distribution.

This model can be defined as follows,

$${\widehat{S}}_{t} = \delta_{p}S_{t} + \alpha\left( \delta_{p}P_{t} + \delta_{c}\sum\limits_{\tau = 0}^{D}\xi\left( \tau|\mu,\sigma \right)P_{t - \tau} \right)$$

where $S_{t}$ and $P_{t}$ are observed primary and secondary
notifications, ${\widehat{S}}_{t}$ are expected secondary notifications,
$\delta_{p} = 1$ and $\delta_{c} = - 1$ when $S_{t}$ is a prevalence
measure, $delta_{p} = 0$ and $\delta_{c} = 1$ when it is an incidence
based measure. $\alpha$ and $\xi$ are defined as the secondary fraction
and delay from primary to secondary notification (or delay from
secondary notification to recovery etc in the prevalence case) with
$\alpha$ typically being of most interest to those interpreting the
models posterior estimates. We further assume that $\xi$ follows a
discretised log normal distribution described by its mean $\mu$ and
standard deviation $\sigma$ on the log scale (where we take the
cumulative mass function for time $t$ minus the cumulative mass function
for $t - 1$) normalised by the maximum allowed delay $D$ such that
$\sum_{\tau = 0}^{D}{\xi\left( \tau|\mu,\sigma \right)} = 1$.

The above definition captures our mechanistic assumptions for the
expectation of secondary notifications but does not account for
potential observation noise or reporting patterns. Here we assume a
negative binomial observation model (though our implementation also
supports a Poisson observation model) in order to capture potential
reporting overdispersion via $\varphi$ and adjust expected counts using
an optional day of the week effect based on a simplex
$\omega_{(t{\mspace{8mu}\operatorname{mod}\ 7})}$ (such that
$\sum_{t = 0}^{6}w_{t} = 7$ so the total effect over a week is
balanced). This gives the following observation process,

$$S_{t} \sim {NB}\left( \omega_{t{\mspace{8mu}\operatorname{mod}\ 7}}{\widehat{S}}_{t},\varphi \right).$$
