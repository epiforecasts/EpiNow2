# Gaussian Process implementation details

## Overview

We make use of Gaussian Processes in several places in `EpiNow2`. For
example, the default model for
[`estimate_infections()`](https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.md)
uses a Gaussian Process to model the 1st order difference on the log
scale of the reproduction number. This vignette describes the
implementation details of the approximate Gaussian Process used in
`EpiNow2`.

## Definition

The one-dimensional Gaussian Processes (${GP}_{t}$) we use can be
written as

$${GP}\left( \mu(t),k(t,t\prime) \right)$$

where $\mu(t)$ and $k(t,t\prime)$ are the mean and covariance functions,
respectively. In our case as set out above, we have

$$\begin{aligned}
{\mu(t)} & {\equiv 0} \\
{k(t,t\prime)} & {= k\left( |t - t\prime| \right) = k(\Delta t)}
\end{aligned}$$

with the following choices available for the kernel $k$

### Matérn 3/2 covariance kernel (the default)

$$k(\Delta t) = \alpha^{2}\left( 1 + \frac{\sqrt{3}\Delta t}{\rho} \right)\exp\left( - \frac{\sqrt{3}\Delta t}{\rho} \right)$$

with $\rho > 0$ and $\alpha > 0$ the length scale and magnitude,
respectively, of the kernel. Note that here and later we use a slightly
different definition of $\alpha$ compared to Riutort-Mayol et
al.^(\[1\]), where this is defined as our $\alpha^{2}$.

### Squared exponential kernel

$$k(\Delta t) = \alpha^{2}\exp\left( - \frac{1}{2}\frac{\left( \Delta t^{2} \right)}{l^{2}} \right)$$

### Ornstein-Uhlenbeck (Matérn 1/2) kernel

$$k(\Delta t) = \alpha^{2}\exp\left( - \frac{\Delta t}{2\rho^{2}} \right)$$

### Matérn 5/2 covariance kernel

$$k(\Delta t) = \alpha\left( 1 + \frac{\sqrt{5}\Delta t}{\rho} + \frac{5}{3}\left( \frac{\Delta t}{l} \right)^{2} \right)\exp\left( - \frac{\sqrt{5}\Delta t}{\rho} \right)$$

## Hilbert space approximation

In order to make our models computationally tractable, we approximate
the Gaussian Process using a Hilbert space approximation to the Gaussian
Process^(\[1\]), centered around mean zero.

$${GP}\left( 0,k(\Delta t) \right) \approx \sum\limits_{j = 1}^{m}\left( S_{k}\left( \sqrt{\lambda_{j}} \right) \right)^{\frac{1}{2}}\phi_{j}(t)\beta_{j}$$

with $m$ the number of basis functions to use in the approximation,
which we calculate from the number of time points $t_{GP}$ to which the
Gaussian Process is being applied (rounded up to give an integer value),
as is recommended^(\[1\]).

$$m = bt_{GP}$$

and values of $\lambda_{j}$ given by

$$\lambda_{j} = \left( \frac{j\pi}{2L} \right)^{2}$$

where $L$ is a positive number termed boundary condition, and
$\beta_{j}$ are regression weights with standard normal prior

$$\beta_{j} \sim {Normal}(0,1)$$

The function $S_{k}(x)$ is the spectral density relating to a particular
covariance function $k$. In the case of the Matérn kernel of order $\nu$
this is given by

$$S_{k}(x) = \alpha^{2}\frac{2\sqrt{\pi}\Gamma(\nu + 1/2)(2\nu)^{\nu}}{\Gamma(\nu)\rho^{2\nu}}\left( \frac{2\nu}{\rho^{2}} + \omega^{2} \right)^{- {(\nu + \frac{1}{2})}}$$

For $\nu = 3/2$ (the default in `EpiNow2`) this simplifies to

$$S_{k}(\omega) = \alpha^{2}\frac{4\left( \sqrt{3}/\rho \right)^{3}}{\left( \left( \sqrt{3}/\rho \right)^{2} + \omega^{2} \right)^{2}} = \left( \frac{2\alpha\left( \sqrt{3}/\rho \right)^{\frac{3}{2}}}{\left( \sqrt{3}/\rho \right)^{2} + \omega^{2}} \right)^{2}$$

For $\nu = 1/2$ it is

$$S_{k}(\omega) = \alpha^{2}\frac{2}{\rho\left( 1/\rho^{2} + \omega^{2} \right)}$$

and for $\nu = 5/2$ it is

$$S_{k}(\omega) = \alpha^{2}\frac{16\left( \sqrt{5}/\rho \right)^{5}}{3\left( \left( \sqrt{5}/\rho \right)^{2} + \omega^{2} \right)^{3}}$$

In the case of a squared exponential the spectral kernel density is
given by

$$S_{k}(\omega) = \alpha^{2}\sqrt{2\pi}\rho\exp\left( - \frac{1}{2}\rho^{2}\omega^{2} \right)$$

The functions $\phi_{j}(x)$ are the eigenfunctions of the Laplace
operator,

$$\phi_{j}(t) = \frac{1}{\sqrt{L}}\sin\left( \sqrt{\lambda_{j}}\left( t^{*} + L \right) \right)$$

with time rescaled linearly to be between -1 and 1,

$$t^{*} = \frac{t - \frac{1}{2}t_{GP}}{\frac{1}{2}t_{GP}}$$

Relevant default priors are

$$\begin{aligned}
\alpha & {\sim {HalfNormal}(0,0.01)} \\
\rho & {\sim {LogNormal}\left( \mu_{\rho},\sigma_{\rho} \right)} \\
 & 
\end{aligned}$$

with $\rho$ additionally constrained with an upper bound of $60$ and
$\mu_{\rho}$ and $\sigma_{\rho}$ calculated using a mean of 21 and
standard deviation of 7.

Furthermore, by default we set.

$$\begin{aligned}
b & {= 0.2} \\
L & {= 1.5}
\end{aligned}$$

These values as well as the prior distributions of relevant parameters
can all be changed by the user.

## References

1\.

Riutort-Mayol, G., Bürkner, P.-C., Andersen, M. R., Solin, A., &
Vehtari, A. (2020). *Practical hilbert space approximate bayesian
gaussian processes for probabilistic programming*.
<https://arxiv.org/abs/2004.11408>
