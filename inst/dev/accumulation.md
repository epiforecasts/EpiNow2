# Supporting missing data

We want to support reporting patterns that include *accumulation* (i.e. batch reporting of data from multiple dates, for example weekly) and *missingness* (dates which are lacking reports) in incidence and prevalence data.

## Proposed interface

`estimate_infections()` and related functions expect data that has no date gaps.
If any dates are missing then an error is thrown.

### Missing data

Any dates between the minimum date and maximum date in the data that is either absent, or present with an `NA` value (currently called `confirm`) is interpreted as missing and ignored in the likelihood.
This matches the current default behaviour, introduced in version 1.5.0.

### Accumulation

If instead modelled values on these days should be accumulated onto the next reporting date, the passed `data.frame` must have an additional numeric column, `accumulate`.
If `accumulate` is greater than zero and the value in the data is `NA` then the modelled value of the observed latent variable on that day is not used in the likelihood and instead added to any existing accumulation and stored for later observation.
If the value in the data is not `NA` then the modelled value is multiplied with the value in `accumulate` and added to any stored accumulated variables.
This is then used in the likelihood.

### Example

| date       | confirm | accumulate |
|------------|---------|------------|
| 2024-10-23 | NA      | 0.5        |
| 2024-10-24 | 10      | 1          |
| 2024-10-26 | NA      | 1          |
| 2024-10-27 | NA      | 0          |
| 2024-10-28 | 17      | 1          |

The likelihood is evaluated on two days, 24 October and 28 October.
On 24 October the data (10) is compared to 0.5 * (modelled value on 23 October) + (modelled value on 24 October).
On 28 October the data (17) is compared to (modelled value on 26 October) + (modelled value on 28 October).

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
| 2024-10-18 | NA      | 1          |
| 2024-10-19 | NA      | 1          |
| 2024-10-20 | NA      | 1          |
| 2024-10-21 | NA      | 1          |
| 2024-10-22 | NA      | 1          |
| 2024-10-23 | NA      | 1          |
| 2024-10-24 | 10      | 1          |
| 2024-10-25 | NA      | 1          |
| 2024-10-26 | NA      | 1          |
| 2024-10-27 | NA      | 1          |
| 2024-10-28 | NA      | 1          |
| 2024-10-29 | NA      | 1          |
| 2024-10-30 | NA      | 1          |
| 2024-10-31 | 17      | 1          |
| 2024-11-01 | NA      | 1          |
| 2024-11-02 | NA      | 1          |
| 2024-11-03 | NA      | 1          |
| 2024-11-04 | NA      | 1          |
| 2024-11-05 | NA      | 1          |
| 2024-11-06 | NA      | 1          |
| 2024-11-07 | 11      | 1          |
