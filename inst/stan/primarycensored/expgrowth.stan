/**
  * Exponential growth probability density function (PDF)
  * @ingroup exponential_growth_distributions
  *
  * @param x Value at which to evaluate the PDF
  * @param min Lower bound of the distribution
  * @param max Upper bound of the distribution
  * @param r Rate parameter for exponential growth
  * @return The PDF evaluated at x
  */
real expgrowth_pdf(real x, real min, real max, real r) {
  if (x < min || x > max) {
    return 0;
  }
  if (abs(r) < 1e-10) {
    return 1 / (max - min);
  }
  return r * exp(r * (x - min)) / (exp(r * max) - exp(r * min));
}

/**
  * Exponential growth log probability density function (log PDF)
  * @ingroup exponential_growth_distributions
  *
  * @param x Value at which to evaluate the log PDF
  * @param min Lower bound of the distribution
  * @param max Upper bound of the distribution
  * @param r Rate parameter for exponential growth
  * @return The log PDF evaluated at x
  */
real expgrowth_lpdf(real x, real min, real max, real r) {
  if (x < min || x > max) {
    return negative_infinity();
  }
  if (abs(r) < 1e-10) {
    return -log(max - min);
  }
  return log(r) + r * (x - min) - log(exp(r * max) - exp(r * min));
}

/**
  * Exponential growth cumulative distribution function (CDF)
  * @ingroup exponential_growth_distributions
  *
  * @param x Value at which to evaluate the CDF
  * @param min Lower bound of the distribution
  * @param max Upper bound of the distribution
  * @param r Rate parameter for exponential growth
  * @return The CDF evaluated at x
  */
real expgrowth_cdf(real x, real min, real max, real r) {
  if (x < min) {
    return 0;
  }
  if (x > max) {
    return 1;
  }
  if (abs(r) < 1e-10) {
    return (x - min) / (max - min);
  }
  return (exp(r * (x - min)) - exp(r * min)) / (exp(r * max) - exp(r * min));
}

/**
  * Exponential growth log cumulative distribution function (log CDF)
  * @ingroup exponential_growth_distributions
  *
  * @param x Value at which to evaluate the log CDF
  * @param min Lower bound of the distribution
  * @param max Upper bound of the distribution
  * @param r Rate parameter for exponential growth
  * @return The log CDF evaluated at x
  */
real expgrowth_lcdf(real x, real min, real max, real r) {
  if (x < min) {
    return negative_infinity();
  }
  if (x > max) {
    return 0;
  }
  return log(expgrowth_cdf(x | min, max, r));
}

/**
  * Exponential growth random number generator
  * @ingroup exponential_growth_distributions
  *
  * @param min Lower bound of the distribution
  * @param max Upper bound of the distribution
  * @param r Rate parameter for exponential growth
  * @return A random draw from the exponential growth distribution
  */
real expgrowth_rng(real min, real max, real r) {
  real u = uniform_rng(0, 1);
  if (abs(r) < 1e-10) {
    return min + u * (max - min);
  }
  return min + log(u * (exp(r * max) - exp(r * min)) + exp(r * min)) / r;
}
