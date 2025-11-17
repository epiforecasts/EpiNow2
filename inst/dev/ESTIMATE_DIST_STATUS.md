# estimate_dist() Implementation Status

## Summary

Working on issue #350 to generalize `estimate_delay()` to `estimate_dist()` using primarycensored.

## Current Blocker

**param_bounds parameter** for `pcd_as_stan_data()` - we need to find the correct format/defaults.

## Two Approaches Developed

### 1. MLE Approach (READY TO TEST) ✓

**File**: `R/estimate_dist_mle.R`

**Status**: Complete, ready for testing

**How it works**:
- Uses `primarycensored::fitdistdoublecens()`
- Maximum Likelihood Estimation (not MCMC)
- Simpler interface, no param_bounds needed
- Returns point estimates, not full posterior

**Usage**:
```r
library(EpiNow2)

# Fit lognormal distribution
delays <- rlnorm(100, log(5), 0.5)
result <- estimate_dist_mle(delays, dist = "lognormal")
```

**Pros**:
- Simple to implement
- No param_bounds problem
- Fast (no MCMC sampling)
- Works with fitdistrplus backend

**Cons**:
- Point estimates only (no uncertainty quantification)
- Less appropriate for Bayesian workflow
- May not support all primarycensored features

### 2. Stan/MCMC Approach (BLOCKED) ⚠️

**File**: `R/estimate_dist_prototype.R`

**Status**: Blocked on param_bounds parameter

**How it works**:
- Uses `primarycensored::pcd_cmdstan_model()`
- Calls `pcd_as_stan_data()` to prepare data
- Full Bayesian inference with Stan
- Returns posterior samples

**The param_bounds Problem**:

`pcd_as_stan_data()` requires a `param_bounds` argument, but we don't know:
1. The correct format (we tried `list(lower = c(...), upper = c(...))`)
2. What sensible defaults should be for each distribution
3. Whether there's a helper function to generate these

**User feedback**: Our arbitrary bounds "make no sense"

## Investigation Tools Created

### test_param_bounds.R

Comprehensive test script to find the correct param_bounds format:
- Tests calling without param_bounds (to see if there's a default)
- Searches for helper functions
- Examines vignette code for examples
- Tests with wide uninformative bounds

**To run**:
```bash
Rscript inst/dev/test_param_bounds.R
```

### Other Diagnostic Scripts

- `check_param_bounds.R` - Explores param_bounds requirements
- `check_vignette_bounds.R` - Looks for examples in vignette
- `inspect_fitdist.R` - Examines fitdistdoublecens internals
- `check_backends.R` - Investigates Stan vs fitdistrplus backends

## Next Steps

### Option A: Get param_bounds Working

1. Run `test_param_bounds.R` to find correct format
2. Check primarycensored documentation/vignette
3. Look at primarycensored source code
4. Update `estimate_dist_prototype.R` with correct bounds

### Option B: Use MLE Approach First

1. Test `estimate_dist_mle.R`
2. Write tests
3. Document
4. Add Stan approach later when param_bounds is solved

### Option C: Hybrid Approach

1. Implement `estimate_dist()` with `method` parameter:
   - `method = "mle"` uses fitdistdoublecens
   - `method = "stan"` uses pcd_cmdstan_model
2. Default to MLE for simplicity
3. Allow users to choose based on their needs

## Backend Compatibility

### cmdstanr vs rstan

Issue: primarycensored uses cmdstanr, but current EpiNow2 uses rstan

**Options**:
1. Make estimate_dist() cmdstanr-only
2. Convert cmdstanr results to rstan using `rstan::read_stan_csv()`
3. Keep estimate_delay() for rstan compatibility
4. Extract Stan code and compile with rstan manually

**User preference**: "it would be nice to be able to use rstan"

## Questions for User

1. Should we proceed with MLE approach (`estimate_dist_mle`) as a first implementation?
2. Is cmdstanr-only acceptable, or do we need rstan support?
3. Do you know where to find sensible param_bounds defaults for primarycensored?
4. Should estimate_dist() support multiple methods (mle, stan)?

## Files Created

### Core Implementation
- `R/estimate_dist_prototype.R` - Stan approach (blocked)
- `R/estimate_dist_mle.R` - MLE approach (ready)
- `R/estimate_delay_deprecated.R` - Deprecation wrapper

### Tests
- `tests/testthat/test-estimate_dist_prototype.R`

### Documentation/Diagnostics
- `inst/dev/ESTIMATE_DIST_STATUS.md` (this file)
- `inst/dev/test_param_bounds.R` - Investigation tool
- `inst/dev/estimate_dist_prototype_notes.md` - Design notes
- `inst/dev/VERIFICATION_RESULTS.md` - primarycensored verification
- Multiple check_*.R scripts

## References

- Issue #350: https://github.com/epiforecasts/EpiNow2/issues/350
- primarycensored: https://primarycensored.epinowcast.org/
- Park et al. (2024): https://doi.org/10.1101/2024.01.12.24301247
