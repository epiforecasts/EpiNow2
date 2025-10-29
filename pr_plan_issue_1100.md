# Feature Implementation Plan: Fix Broken @seealso Links in Roxygen2 Documentation

## Overview

This plan addresses issue #1100 by fixing 14 broken `@seealso` references across 8 R files in the EpiNow2 package. The fixes will convert plain text function references to proper roxygen2 link syntax `[function_name()]`, ensuring documentation hyperlinks work correctly.

**Key objectives:**
- Fix all 14 broken `@seealso` references
- Resolve the non-existent `rt_settings` reference
- Handle S3 method names appropriately
- Remove trailing text and format multi-function references correctly
- Ensure all changes conform to 80-character line limit and UK English standards
- Verify documentation builds without errors

**Success criteria:**
- All `@seealso` references use proper roxygen2 syntax
- `devtools::document()` runs without warnings
- Generated HTML documentation contains working hyperlinks
- No references to non-existent functions

## Detailed Fixes Required

### R/create.R (2 fixes)

**Fix 1 - Line 173:**
```r
# BEFORE:
#' @seealso rt_settings

# AFTER:
#' @seealso [rt_opts()]
```

**Fix 2 - Line 260:**
```r
# BEFORE:
#' @seealso backcalc_opts

# AFTER:
#' @seealso [backcalc_opts()]
```

### R/epinow-internal.R (1 fix)

**Fix - Line 51:**
```r
# BEFORE:
#' @seealso estimate_infections

# AFTER:
#' @seealso [estimate_infections()]
```

### R/estimate_secondary.R (2 fixes)

**Fix 1 - Line 370:**
```r
# BEFORE:
#' @seealso plot estimate_secondary

# AFTER:
#' @seealso [plot.estimate_secondary()] [estimate_secondary()]
```

**Fix 2 - Line 452:**
```r
# BEFORE:
#' @seealso estimate_secondary

# AFTER:
#' @seealso [estimate_secondary()]
```

### R/estimate_truncation.R (1 fix)

**Fix - Line 289:**
```r
# BEFORE:
#' @seealso plot estimate_truncation

# AFTER:
#' @seealso [plot.estimate_truncation()] [estimate_truncation()]
```

### R/opts.R (1 fix)

**Fix - Line 1045:**
```r
# BEFORE:
#' @seealso fill_missing

# AFTER:
#' @seealso [fill_missing()]
```

### R/plot.R (2 fixes)

**Fix 1 - Line 380:**
```r
# BEFORE:
#' @seealso plot report_plots estimate_infections

# AFTER:
#' @seealso [plot.estimate_infections()] [report_plots()]
#' [estimate_infections()]
```

**Fix 2 - Line 416:**
```r
# BEFORE:
#' @seealso plot plot.estimate_infections report_plots estimate_infections

# AFTER:
#' @seealso [plot.estimate_infections()] [report_plots()]
#' [estimate_infections()]
```

### R/report.R (1 fix)

**Fix - Line 138:**
```r
# BEFORE:
#' @seealso [plot_estimates()] of

# AFTER:
#' @seealso [plot_estimates()]
```

### R/summarise.R (5 fixes)

**Fix 1 - Line 157:**
```r
# BEFORE:
#' @seealso regional_epinow

# AFTER:
#' @seealso [regional_epinow()]
```

**Fix 2 - Line 440:**
```r
# BEFORE:
#' @seealso regional_summary

# AFTER:
#' @seealso [regional_summary()]
```

**Fix 3 - Line 517:**
```r
# BEFORE:
#' @seealso regional_summary regional_epinow

# AFTER:
#' @seealso [regional_summary()] [regional_epinow()]
```

**Fix 4 - Line 775:**
```r
# BEFORE:
#' @seealso summary.estimate_infections epinow

# AFTER:
#' @seealso [summary.estimate_infections()] [epinow()]
```

**Fix 5 - Line 826:**
```r
# BEFORE:
#' @seealso summary estimate_infections report_summary

# AFTER:
#' @seealso [summary.estimate_infections()] [estimate_infections()]
#' [report_summary()]
```

## Verification Strategy

1. Run `devtools::document()` to regenerate documentation
2. Check for any warnings about unresolvable links
3. Verify generated `.Rd` files contain proper `\link{}` syntax
4. Build pkgdown site locally to check hyperlinks work
5. Run `devtools::check()` to ensure no new issues
6. Verify all existing tests still pass

## Edge Cases Handled

1. **rt_settings → rt_opts()**: Corrected non-existent function reference
2. **S3 methods**: Using specific method names (plot.estimate_secondary, plot.estimate_truncation, summary.estimate_infections)
3. **80-character limit**: Long lines split across multiple @seealso lines
4. **Trailing text**: Removed "of" from R/report.R line 138
5. **Duplicate references**: Removed duplicate "plot" in R/plot.R line 416

## Reference

See `issue_analysis_1100.md` for complete analysis of broken links.
