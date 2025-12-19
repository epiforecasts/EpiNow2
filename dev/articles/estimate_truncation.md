# Model definition: estimate_truncation()

**This is a work in progress. Please consider submitting a PR to improve
it.**

This model deals with the problem of *nowcasting*, or adjusting for
right-truncation in reported count data. This occurs when the quantity
being observed, for example cases, hospitalisations or deaths, is
reported with a delay, resulting in an underestimation of recent counts.
The
[`estimate_truncation()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_truncation.md)
model attempts to infer parameters of the underlying delay distributions
from multiple snapshots of past data. It is designed to be a simple
model that can integrate with the other models in the package and
therefore may not be ideal for all uses. For a more principled approach
to nowcasting please consider using the
[epinowcast](https://package.epinowcast.org) package.

Given snapshots $C_{t}^{i}$ reflecting reported counts for time $t$
where $i = 1\ldots S$ is in order of recency (earliest snapshots first)
and $S$ is the number of past snapshots used for estimation, we try to
infer the parameters of a discrete truncation distribution
$\zeta\left( \tau|\mu_{\zeta},\sigma_{\zeta} \right)$ with corresponding
probability mass function $Z(\tau|\mu_{\zeta}$).

The model assumes that final counts $D_{t}$ are related to observed
snapshots via the truncation distribution such that

\$\$\begin{equation} C^{i \< S)\_{t}\_\sim \mathrm{NegBinom}\left(Z
(T_i - t \| \mu\_{Z}, \sigma\_{Z}) D(t) + \sigma, \varphi\right)
\end{equation}\$\$

where $T_{i}$ is the date of the final observation in snapshot $i$,
$Z(\tau)$ is defined to be zero for negative values of $\tau$ and
$\sigma$ is an additional error term.

The final counts $D_{t}$ are estimated from the most recent snapshot as

$$D_{t} = \frac{C^{S}}{Z\left( T_{S} - t|\mu_{Z},\sigma_{Z} \right)}$$

Relevant priors are:

$$\begin{aligned}
\mu_{\zeta} & {\sim {Normal}(0,1)} \\
\sigma_{\zeta} & {\sim {HalfNormal}(0,1)} \\
\frac{1}{\sqrt{\varphi}} & {\sim {HalfNormal}(0,0.25)} \\
\sigma & {\sim {HalfNormal}(0,1)}
\end{aligned}$$
