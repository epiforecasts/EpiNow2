# Verification Steps for primarycensored Integration

## Quick Test Commands

Run these in R to verify what primarycensored provides:

```r
# 1. Check available functions
library(primarycensored)
ls("package:primarycensored")

# 2. Look for fitting functions
grep("fit|stan|model|cmdstan", ls("package:primarycensored"), value = TRUE)

# 3. Check if key functions exist
exists("pcd_cmdstan_model", where = "package:primarycensored")
exists("pcd_as_stan_data", where = "package:primarycensored")

# 4. Get help on model function (if it exists)
?pcd_cmdstan_model

# 5. Check vignettes
vignette(package = "primarycensored")
```

## What We Already Know

From PR #1140, we know primarycensored provides:

- `dprimarycensored()` - PMF for primary event censored distributions
- `qprimarycensored()` - Quantile function for primary event censored distributions

These are used in `discrete_pmf()` for creating PMFs from continuous distributions.

## What We Need to Find

For `estimate_dist()` implementation, we need:

1. **Model fitting function**
   - Is it `pcd_cmdstan_model()` or something else?
   - Does it return a CmdStanModel or CmdStanMCMC object?

2. **Data preparation function**
   - Is it `pcd_as_stan_data()` or similar?
   - What parameters does it take?
   - What format does it expect?

3. **Parameter names in Stan model**
   - What are the parameter names for lognormal? (mu/sigma or meanlog/sdlog?)
   - What about gamma? (alpha/beta or shape/rate?)
   - What about other distributions?

4. **Input data format**
   - Does it accept vectors or data frames?
   - How to specify censoring windows?
   - How to specify truncation?

## Testing the Prototype

Once you've verified the above, test the prototype with:

```r
# Load EpiNow2 functions
devtools::load_all()

# Generate test data
set.seed(123)
delays <- rpois(100, lambda = 5) + 1

# Try the prototype (will likely fail on first attempt)
result <- estimate_dist(
  delays,
  dist = "lognormal",
  samples = 500,
  chains = 2,
  cores = 1,
  verbose = TRUE
)

# If successful, check output
class(result)
result$distribution
names(result$parameters)
```

## Expected Issues

1. **Function names wrong** - We guessed `pcd_cmdstan_model()` and `pcd_as_stan_data()`
2. **Parameter names wrong** - Stan model might use different names than we assumed
3. **Data format wrong** - Might need different structure than we assumed
4. **Missing dependencies** - cmdstanr might not be installed or CmdStan not set up

## Fallback Plan

If primarycensored doesn't provide fitting functions:

1. Use primarycensored only for `dprimarycensored()` and `qprimarycensored()` (already done)
2. Keep the existing `dist_fit.stan` model
3. Update `dist_fit.stan` to use primarycensored's approach for censoring
4. Make `estimate_dist()` a wrapper around improved `dist_fit()`

## Next Actions

1. Run the test script: `Rscript inst/dev/test_primarycensored.R`
2. Check package GitHub: https://github.com/epinowcast/primarycensored
3. Read any vignettes or examples about model fitting
4. Update `estimate_dist_prototype.R` based on findings
5. Test with real data
