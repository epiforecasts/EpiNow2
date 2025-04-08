/**
 * @defgroup infections_estimation Infections Estimation
 * @brief Functions for estimating infection trajectories
 *
 * This group contains functions for estimating and modeling infection dynamics,
 * including implementation of the renewal equation and backcalculation approaches.
 * Core infection estimation functions from infections.stan and related generated
 * quantities functions.
 */

/**
 * @defgroup rt_estimation Rt Estimation
 * @brief Functions for estimating reproduction numbers
 *
 * This group contains functions for estimating and processing reproduction numbers (Rt),
 * including updating Rt values, calculating growth rates, and converting between
 * reproduction numbers and growth rates. Includes functions from rt.stan and related
 * generated quantities.
 */

/**
 * @defgroup secondary_reports_estimation Secondary Reports Estimation
 * @brief Functions for estimating secondary epidemiological reports
 *
 * This group contains functions from secondary.stan for estimating secondary
 * epidemiological reports based on primary reports, considering various options
 * for combining historical and current data.
 */

/**
 * @defgroup estimates_smoothing Estimates Smoothing
 * @ingroup infections_estimation rt_estimation secondary_reports_estimation
 * @brief Functions for smoothing estimates using Gaussian processes
 *
 * Functions from gaussian_process.stan for implementing approximate Gaussian processes
 * using Hilbert space methods to smooth infection and Rt trajectories.
 */

/**
 * @defgroup observation_model Observation Model
 * @ingroup infections_estimation rt_estimation secondary_reports_estimation
 * @brief Functions for modeling the observation process
 *
 * This group contains functions related to how infections or reports are observed,
 * including day-of-week effects, reporting delays, and various likelihood models.
 */

/**
 * @defgroup handlers_and_helpers Handlers and Helpers
 * @brief Utility functions and parameter handlers
 *
 * This group contains utility functions and parameter handlers from convolve.stan,
 * params.stan, delays.stan, and pmfs.stan.
 */

/**
 * @defgroup convolution_functions Convolution Functions
 * @ingroup handlers_and_helpers
 * @brief Functions for convolving time series
 *
 * Functions from convolve.stan for implementing convolutions between infections
 * and delay distributions.
 */

/**
 * @defgroup parameter_handlers Parameter Handlers
 * @ingroup handlers_and_helpers
 * @brief Functions for parameter management
 *
 * Functions from params.stan for accessing, manipulating, and applying priors to
 * model parameters.
 */

/**
 * @defgroup delay_handlers Delay Handlers
 * @ingroup handlers_and_helpers
 * @brief Functions for delay distribution handling
 *
 * Functions from delays.stan for creating, parameterizing and applying delay
 * distributions.
 */

/**
 * @defgroup pmf_handlers PMF Handlers
 * @ingroup handlers_and_helpers
 * @brief Functions for probability mass function handling
 *
 * Functions from pmfs.stan for handling and manipulating probability mass functions.
 */
