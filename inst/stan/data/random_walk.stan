// Random walk states. The step standard deviation is an ordinary parameter
// (data and functions in params.stan); the arrays below reference it by
// parameter id.
int<lower = 0> n_rw_components;
array[n_rw_components] int<lower = 1> rw_sd_id; // parameter id of each step sd
int<lower = 1> state_rw_period; // time steps between random walk steps

// A random-walk component steps on a regular grid (state_rw_period, above) or
// on explicit knots, i.e. absolute 1-indexed time points at which a new step
// starts (rw_knots_n[c] == 0 means component c uses the regular grid). Knots
// are supplied resolved to plain time indices (see RW()); how many of a
// component's knots fall within its own free-noise window is computed in
// transformed data, since the window is only known there.
int<lower = 0> n_rw_knots; // total knots across all components
array[n_rw_components] int<lower = 0> rw_knots_n; // knot count per component
array[n_rw_components] int<lower = 0> rw_knots_offset; // offset into rw_knots
array[n_rw_knots] int<lower = 1> rw_knots;
