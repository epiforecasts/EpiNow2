# estimate_dist() Prototype Implementation Notes

## Overview

This prototype demonstrates a simplified approach to implementing `estimate_dist()` by directly using the `primarycensored` package rather than creating custom S3 generics.

## Key Design Decisions

### 1. No Custom S3 Generic for Extraction

**Why not `as_dist_spec.CmdStanMCMC()`?**

- `CmdStanMCMC` is too generic - many Stan models return this class
- Would create method dispatch conflicts with other packages
- Not all `CmdStanMCMC` objects contain distribution parameters

**Solution:** Direct extraction within `estimate_dist()`

- `.extract_dist_spec()` is a private helper function (prefixed with `.`)
- Takes a `CmdStanMCMC` fit object and extracts known parameters
- Only called within the controlled context of `estimate_dist()`
- Avoids S3 dispatch complexity

### 2. Backend Requirements

- **cmdstanr only**: primarycensored is designed for cmdstanr
- rstan compatibility via `read_stan_csv()` is possible but not prioritised
- Consistent with EpiNow2's direction (cmdstanr in Suggests)

### 3. Data Format Handling

**Two input formats supported:**

1. **Vector** (backwards compatible):
   ```r
   delays <- c(3, 5, 2, 7, ...)
   estimate_dist(delays)
   ```
   - Assumes daily censoring (uniform primary/secondary distributions)
   - No truncation applied

2. **Linelist** (new, more correct):
   ```r
   linelist <- data.frame(
     ptime_lwr = ...,  # Primary event time lower bound
     stime_lwr = ...,  # Secondary event time lower bound
     ptime_upr = ...,  # Optional: primary event upper bound
     stime_upr = ...,  # Optional: secondary event upper bound
     obs_time = ...    # Optional: observation time (for truncation)
   )
   estimate_dist(linelist)
   ```

## Implementation Flow

```
estimate_dist(data)
    ↓
1. Validate inputs & check dependencies
    ↓
2. .convert_to_pcd_data()
    ├─ Vector → .vector_to_pcd_data()
    │   └─ primarycensored::pcd_as_stan_data()
    └─ Linelist → .linelist_to_pcd_data()
        └─ primarycensored::pcd_as_stan_data()
    ↓
3. primarycensored::pcd_cmdstan_model() → model
    ↓
4. model$sample(data) → fit (CmdStanMCMC object)
    ↓
5. .extract_dist_spec(fit)
    ├─ fit$draws() → posterior samples
    ├─ Extract parameters (mu, sigma, alpha, beta, etc.)
    ├─ Wrap in Normal() distributions (uncertainty)
    └─ new_dist_spec() → dist_spec object
    ↓
6. Return dist_spec
```

## Parameter Name Mapping

The prototype assumes primarycensored uses these parameter names (TO BE VERIFIED):

| Distribution | primarycensored params | dist_spec params      |
|--------------|------------------------|------------------------|
| Lognormal    | `mu`, `sigma`          | `meanlog`, `sdlog`     |
| Gamma        | `alpha`, `beta`        | `shape`, `rate`        |
| Weibull      | `shape`, `scale`       | `shape`, `scale`       |

**IMPORTANT:** This needs verification against actual primarycensored Stan model.

## Comparison with Current Approach

### Current: `bootstrapped_dist_fit()`
```
1. Bootstrap sample the data
2. For each bootstrap:
   - Fit dist_fit.stan (exponential/lognormal/gamma)
   - Extract parameters
3. Pool all posterior samples
4. Create Normal() distributions from pooled samples
5. Return new_dist_spec()
```

### Proposed: `estimate_dist()`
```
1. Convert data to primarycensored format
2. Fit single unified model (accounts for censoring/truncation)
3. Extract parameters from fit
4. Create Normal() distributions from posteriors
5. Return new_dist_spec()
```

**Key advantages:**
- Single model instead of 3 separate approaches
- Proper censoring and truncation handling
- More statistically correct (following Park et al. 2024)
- No bootstrap subsampling needed (uncertainty in posteriors)

## Open Questions

### Critical Questions

1. **What parameter names does primarycensored actually use?**
   - Need to inspect `primarycensored::pcd_cmdstan_model()` Stan code
   - May differ from our assumptions

2. **How does primarycensored::pcd_as_stan_data() work?**
   - Current prototype assumes this function exists
   - Need to verify actual interface
   - May need to construct Stan data manually

3. **Do we need the bootstrap approach?**
   - Current `bootstrapped_dist_fit()` subsamples data
   - Rationale: "account for fact underlying delay varies over time"
   - primarycensored might handle this differently
   - Decision: Keep or remove bootstrap logic?

### Implementation Questions

4. **Deprecation timeline for estimate_delay()?**
   - Soft deprecation (warning) in 1.8.0?
   - Hard deprecation (error) in 1.9.0?
   - Remove in 2.0.0?

5. **Do we keep dist_fit.stan?**
   - Needed for backwards compatibility during deprecation
   - Can remove once `estimate_delay()` is fully removed
   - Or keep as fallback if primarycensored unavailable?

6. **Integration with existing workflows?**
   - Does output work with `estimate_infections()`?
   - Any breaking changes to `dist_spec` structure?
   - Test with existing vignettes

7. **Should we accept epidist linelist objects directly?**
   ```r
   # Option A: Accept epidist objects
   linelist <- epidist::as_epidist_linelist_data(...)
   estimate_dist(linelist)  # Detect and convert

   # Option B: Users convert manually
   pcd_data <- epidist_to_pcd(linelist)
   estimate_dist(pcd_data)
   ```

### Documentation Questions

8. **How much detail in vignettes?**
   - Censoring concepts (primary vs secondary)
   - When to use vector vs linelist input
   - How to interpret results
   - Comparison with old method

9. **Examples for NEWS.md**
   - Show migration path from `estimate_delay()`
   - Highlight new capabilities

## Next Steps

### Phase 1: Verification (DO THIS FIRST!)
1. Install and explore primarycensored
2. Check actual Stan model parameters
3. Verify `pcd_as_stan_data()` interface (or find correct function)
4. Run a simple fit and inspect output structure
5. Update prototype based on findings

### Phase 2: Core Implementation
1. Fix parameter names based on verification
2. Implement data conversion helpers
3. Add comprehensive input validation
4. Test with real data

### Phase 3: Integration
1. Update `estimate_delay()` with deprecation
2. Ensure `dist_spec` output works downstream
3. Update vignettes
4. Add comprehensive tests

### Phase 4: Polish
1. Error messages and edge cases
2. Performance optimization
3. Documentation
4. NEWS.md entry

## Files Created (Prototype)

- `R/estimate_dist_prototype.R` - Main implementation
- `R/estimate_delay_deprecated.R` - Deprecation wrapper
- `tests/testthat/test-estimate_dist_prototype.R` - Tests
- `inst/dev/estimate_dist_prototype_notes.md` - This file

## Questions for Maintainers

Before proceeding with full implementation, please clarify:

1. Is the overall approach (direct primarycensored use) acceptable?
2. Should we keep bootstrap subsampling or trust primarycensored's approach?
3. Preferred deprecation timeline for `estimate_delay()`?
4. Should we support both rstan and cmdstanr, or cmdstanr only?
5. Integration with epidist package - how deep?

## References

- Park et al. (2024): https://doi.org/10.1101/2024.01.12.24301247
- Charniga et al. (2024): https://doi.org/10.48550/arXiv.2405.08841
- primarycensored: https://primarycensored.epinowcast.org/
- Issue #350: https://github.com/epiforecasts/EpiNow2/issues/350
