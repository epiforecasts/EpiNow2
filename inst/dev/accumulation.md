# Supporting missing data

We want to support reporting patterns that include *accumulation* (i.e. batch reporting of data from multiple dates, for example weekly) and *missingness* (dates which are lacking reports) in incidence and prevalence data.

## Proposed interface

`estimate_infections()` and related functions expect data that has no date gaps.
If any dates are missing then an error is thrown.

### Missing data

Any dates between the minimum date and maximum date in the data that is either absent, or present with an `NA` value (currently called `confirm`) is interpreted as missing and ignored in the likelihood.
All other data points are used in the likelihood.
This matches the current default behaviour, introduced in version 1.5.0.

### Accumulation

If instead modelled values on these days should be accumulated onto the next reporting date, the passed `data.frame` must have an additional logical column, `accumulate`.
If `accumulate` is TRUE then the modelled value of the observed latent variable on that day is added to any existing accumulation and stored for later observation.
If `accumulate` is FALSE the modelled value is added to any stored accumulated variables before potentially being used in the likelihood on that day (if not `NA`).
Subsequently the stored accumulated variable is reset to zero.

### Example

| date       | confirm | accumulate |
|------------|---------|------------|
| 2024-10-23 | NA      | TRUE       |
| 2024-10-24 | 10      | FALSE      |
| 2024-10-26 | NA      | TRUE       |
| 2024-10-27 | NA      | TRUE       |
| 2024-10-28 | 17      | FALSE      |

The likelihood is evaluated on two days, 24 October and 28 October.
On 24 October the data (10) is compared to (modelled value on 23 October) + (modelled value on 24 October).
On 28 October the data (17) is compared to (modelled value on 26 October) + (modelled value on 27 October) + (modelled value on 28 October).

## Helper functions

A helper function, `fill_missing_dates()` can be used to convert weekly data to the required format, i.e.

| date       | confirm |
|------------|---------|
| 2024-10-24 | 10      |
| 2024-10-31 | 17      |
| 2024-11-07 | 11      |

can be converted with `fill_missing_dates(missing = "accumulate", initial = 7)` to

| date       | confirm | accumulate |
|------------|---------|------------|
| 2024-10-18 | NA      | TRUE       |
| 2024-10-19 | NA      | TRUE       |
| 2024-10-20 | NA      | TRUE       |
| 2024-10-21 | NA      | TRUE       |
| 2024-10-22 | NA      | TRUE       |
| 2024-10-23 | NA      | TRUE       |
| 2024-10-24 | 10      | FALSE      |
| 2024-10-25 | NA      | TRUE       |
| 2024-10-26 | NA      | TRUE       |
| 2024-10-27 | NA      | TRUE       |
| 2024-10-28 | NA      | TRUE       |
| 2024-10-29 | NA      | TRUE       |
| 2024-10-30 | NA      | TRUE       |
| 2024-10-31 | 17      | FALSE      |
| 2024-11-01 | NA      | TRUE       |
| 2024-11-02 | NA      | TRUE       |
| 2024-11-03 | NA      | TRUE       |
| 2024-11-04 | NA      | TRUE       |
| 2024-11-05 | NA      | TRUE       |
| 2024-11-06 | NA      | TRUE       |
| 2024-11-07 | 11      | TRUE       |
