# Issue #1100: Many broken `@seealso` links throughout

## Summary
Multiple `@seealso` roxygen2 tags throughout the EpiNow2 codebase are malformed, resulting in broken documentation links on the pkgdown website. The issue affects at least 33 instances across 9 R files. According to roxygen2 documentation standards, function references in `@seealso` tags must be formatted with parentheses (e.g., `[function_name()]`) to generate proper hyperlinks.

## Root Cause and Technical Details
The roxygen2 `@seealso` tag requires specific markdown syntax to create hyperlinks:
- **Correct format**: `[function_name()]` creates a clickable link
- **Incorrect format**: `function_name` without brackets/parentheses creates plain text
- **Comparison**:
  - Working example: `epinow()` documentation uses `@seealso [estimate_infections()] [forecast_infections()] [regional_epinow()]`
  - Broken example: `summary.epinow()` documentation uses `@seealso summary.estimate_infections epinow`

## Acceptance Criteria
1. All `@seealso` references must use proper roxygen2 markdown link syntax: `[function_name()]`
2. Function references must include parentheses to indicate they are functions
3. S3 methods should be referenced appropriately (e.g., `summary.epinow()` not just `summary`)
4. All links must be verifiable in the generated pkgdown documentation
5. Non-function references (if any exist) should be properly marked or converted to function references where appropriate

## Files with Broken References

### R/create.R (1 broken reference)
- Line 173: `@seealso rt_settings` - **BROKEN** (function doesn't exist, should likely reference `rt_opts()`)

### R/epinow-internal.R (1 broken reference)
- Line 51: `@seealso estimate_infections` - needs `[estimate_infections()]`

### R/estimate_secondary.R (2 broken references)
- Line 370: `@seealso plot estimate_secondary` - needs `[plot.estimate_secondary()] [estimate_secondary()]`
- Line 452: `@seealso estimate_secondary` - needs `[estimate_secondary()]`

### R/estimate_truncation.R (1 broken reference)
- Line 289: `@seealso plot estimate_truncation` - needs `[plot.estimate_truncation()] [estimate_truncation()]`

### R/opts.R (1 broken reference)
- Line 1045: `@seealso fill_missing` - needs `[fill_missing()]`

### R/plot.R (2 broken references)
- Line 380: `@seealso plot report_plots estimate_infections` - needs proper S3 method reference and function brackets
- Line 416: `@seealso plot plot.estimate_infections report_plots estimate_infections` - needs function brackets

### R/report.R (1 malformed reference)
- Line 138: `@seealso [plot_estimates()] of` - appears to be incomplete/malformed

### R/summarise.R (5 broken references)
- Line 157: `@seealso regional_epinow` - needs `[regional_epinow()]`
- Line 440: `@seealso regional_summary` - needs `[regional_summary()]`
- Line 517: `@seealso regional_summary regional_epinow` - needs `[regional_summary()] [regional_epinow()]`
- Line 775: `@seealso summary.estimate_infections epinow` - needs `[summary.estimate_infections()] [epinow()]`
- Line 826: `@seealso summary estimate_infections report_summary` - needs proper format

**Total: 14 broken references across 8 files**

## Specific Fix Patterns

**Pattern 1: Missing brackets entirely**
```r
# Before
#' @seealso estimate_infections

# After
#' @seealso [estimate_infections()]
```

**Pattern 2: Multiple references without brackets**
```r
# Before
#' @seealso summary.estimate_infections epinow

# After
#' @seealso [summary.estimate_infections()] [epinow()]
```

**Pattern 3: Non-existent function reference**
```r
# Before
#' @seealso rt_settings

# After (rt_settings doesn't exist, likely meant rt_opts)
#' @seealso [rt_opts()]
```

## Edge Cases

1. **Non-existent function: `rt_settings`** - should be `[rt_opts()]`
2. **S3 Method References** - use specific method names (e.g., `plot.estimate_infections()`)
3. **Generic Function References** - use specific S3 method names rather than base generics
4. **Incomplete Reference in R/report.R** - `@seealso [plot_estimates()] of` has trailing "of"

## Verification Strategy

After fixes:
1. Run `devtools::document()` to regenerate documentation
2. Build pkgdown site with `pkgdown::build_site()`
3. Manually check each fixed reference in the HTML output
4. Verify all links are clickable and point to correct documentation pages
5. Check for any new warnings from roxygen2 about unresolvable links
