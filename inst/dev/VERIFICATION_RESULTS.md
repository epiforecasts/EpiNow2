# primarycensored Verification Results

## ✅ Confirmed Functions

### 1. `pcd_cmdstan_model()` - EXISTS
- **Purpose**: Creates a CmdStanModel with primarycensored Stan functions
- **Returns**: A CmdStanModel object
- **Usage**:
  ```r
  model <- pcd_cmdstan_model(
    include_paths = primarycensored::pcd_stan_path(),
    ...  # Additional args passed to cmdstanr::cmdstan_model()
  )
  ```
- **Features**: Supports multiple distributions, primary/secondary censoring, truncation, reduce_sum for parallelism

### 2. `pcd_as_stan_data()` - EXISTS
- **Purpose**: Prepares data for Stan model
- **Parameters** (need to verify exact signature with `?pcd_as_stan_data`)

### 3. Distribution functions - EXIST
- `dprimarycensored()` - PMF (already used in EpiNow2)
- `pprimarycensored()` - CDF
- `qprimarycensored()` - Quantile (already used in EpiNow2)
- `rprimarycensored()` - Random generation

### 4. Additional fitting function
- `fitdistdoublecens()` - Alternative fitting wrapper (investigate further)

## 📋 Available Vignettes

1. **"Getting Started with primarycensored"** - Main tutorial
2. **"Analytic solutions"** - Analytical methods
3. **"Why it works"** - Methodology explanation

Access with: `browseVignettes("primarycensored")`

## 🔍 Next Verification Steps

Run these scripts to get more details:

### 1. Check function signatures and data structure
```r
source("inst/dev/check_pcd_details.R")
```

This will show:
- Exact parameters for `pcd_as_stan_data()`
- Structure of Stan data object
- Available distributions
- Stan model parameters

### 2. Check vignette examples
```r
source("inst/dev/check_vignette.R")
```

or directly:
```r
browseVignettes("primarycensored")
vignette("primarycensored", package = "primarycensored")
```

### 3. Find Stan parameter names
Need to determine what parameters the Stan model returns:
- For lognormal: `mu`, `sigma`? or `meanlog`, `sdlog`?
- For gamma: `alpha`, `beta`? or `shape`, `rate`?

This can be found by:
- Inspecting Stan model code (at `pcd_stan_files()`)
- Fitting a test model and checking `fit$summary()`

## 🎯 Prototype Status

### What Works
- ✅ Functions we assumed exist: `pcd_cmdstan_model()`, `pcd_as_stan_data()`
- ✅ Overall structure of `estimate_dist()` is sound
- ✅ Data conversion approach is valid

### What Needs Verification
- ❓ Exact parameters for `pcd_as_stan_data()` (delay vs delays? other args?)
- ❓ Stan model parameter names
- ❓ Whether we need to specify priors or distribution type in stan_data
- ❓ How to specify which distribution to fit (lognormal vs gamma)

### What Needs Updating in Prototype

1. **Check `pcd_as_stan_data()` signature**
   - May have different parameter names
   - May need additional arguments (distribution type? priors?)

2. **Update `.extract_dist_spec()`**
   - Use actual parameter names from Stan model
   - May need to handle different output structure

3. **Add distribution specification**
   - Figure out how to tell the Stan model which distribution to use
   - May be in stan_data or as model parameter

## 📝 Test Plan

### Step 1: Understand pcd_as_stan_data()
```r
?pcd_as_stan_data
args(pcd_as_stan_data)

# Test with example data
delays <- rpois(100, 5) + 1
stan_data <- pcd_as_stan_data(delays, delays + 1, 1, 1, max(delays) + 10)
str(stan_data)
```

### Step 2: Fit a test model
```r
library(primarycensored)
library(cmdstanr)

# Prepare data
delays <- rlnorm(100, log(5), 0.5) |> round() |> as.integer()
stan_data <- pcd_as_stan_data(
  delay = delays,
  delay_upper = delays + 1,
  pwindow = 1,
  swindow = 1,
  D = max(delays) + 10
)

# Get model
model <- pcd_cmdstan_model()

# Fit
fit <- model$sample(
  data = stan_data,
  chains = 2,
  iter_warmup = 500,
  iter_sampling = 500
)

# Check parameters
fit$summary()
```

### Step 3: Update prototype based on findings
- Fix parameter names
- Adjust data preparation
- Test extraction logic

### Step 4: Test estimate_dist()
```r
devtools::load_all()
delays <- rlnorm(100, log(5), 0.5) |> round() |> as.integer()
result <- estimate_dist(delays, dist = "lognormal", samples = 1000)
result
```

## 🚀 Expected Workflow

Based on what we know, the workflow should be:

```r
estimate_dist(data) →
  .convert_to_pcd_data() →
    pcd_as_stan_data() → stan_data

  pcd_cmdstan_model() → model

  model$sample(data = stan_data) → fit (CmdStanMCMC)

  .extract_dist_spec(fit) →
    fit$draws() → parameter samples
    wrap in Normal() distributions
    new_dist_spec() → result
```

This matches our prototype design! Just need to fix the details.

## 📚 Resources

- Package docs: https://primarycensored.epinowcast.org/
- GitHub: https://github.com/epinowcast/primarycensored
- CRAN: https://CRAN.R-project.org/package=primarycensored
- Vignettes: `browseVignettes("primarycensored")`
