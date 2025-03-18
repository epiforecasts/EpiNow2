# EpiNow2 Stan Functions

This documentation covers EpiNow2's Stan functions.

## Model Components

* [Reproduction Number (Rt) Functions](./html/rt_8stan.html)
  Functions for calculating, updating, and converting reproduction numbers, including approaches based on Cori et al. The reproduction number represents the average number of secondary infections caused by a single infected individual.

* [Infection Modeling Functions](./html/infections_8stan.html)
  Functions for generating and backcalculating infection time series, implementing the renewal equation approach and handling population depletion effects.

* [Secondary Reports Functions](./html/secondary_8stan.html)
  Functions for calculating and manipulating secondary reports derived from primary reports through various transformations, representing different epidemiological quantities.

* [Delay Distribution Functions](./html/delays_8stan.html)
  Functions for creating, manipulating, and applying delay distributions that represent the time between events.

* [Gaussian Process Functions](./html/gaussian__process_8stan.html)
  Functions implementing approximate Gaussian processes for Stan using Hilbert space methods, supporting various kernel types.

* [Observation Model Functions](./html/observation__model_8stan.html)
  Functions for modeling the observation process, including reporting effects, truncation, and likelihood calculations.

### Utilities

* [Probability Mass Function (PMF) Utilities](./html/pmfs_8stan.html)
  Functions for creating and manipulating probability mass functions, particularly for discretizing continuous distributions for use in delay modeling.

* [Parameter Management Functions](./html/params_8stan.html)
  Functions for parameter access, retrieval, and prior specification. These handle parameters that can be either fixed (specified in advance) or variable (estimated during inference).

* [Convolution Functions](./html/convolve_8stan.html)
  Functions for performing discrete convolutions, including convolving infections with delay distributions to generate reported cases.

### Others

* [Generated Quantities Functions](./html/generated__quantities_8stan.html)
  Functions for calculating additional quantities from model outputs, such as direct Rt estimation from infections and growth rate calculations.

## Function Index

For a complete alphabetical listing of all functions, see the [function index](./html/functions.html).
