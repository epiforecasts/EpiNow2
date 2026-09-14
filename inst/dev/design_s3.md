# S3 return types for model types

All three models to return an object (`epinowfit` or the like) inheriting from list and containing

- `fit` (the stan object)
- `args` (the arguments that have been passed to stan)
- `observations` (the timed data frame that has been passed as observations)

with an appropriate `print()` method that shows some appropriate summary of what the object contains (no results).

# Mapping of existing return elements to accessor functions

## estimate_infections

- `samples`: `get_samples()` S3 generic method for extracting formatted posterior samples; built on low-level `extract_samples()` which extracts raw Stan arrays. Note: `format_simulation_output()` is a separate internal function for simulation outputs;
- `summarised`: `summary()`
- `fit`: accessed directly via `object$fit` (no accessor function needed)
- `args`: accessed directly via `object$args` (used internally to identify parameter ids)
- `observations`: accessed directly via `object$observations` (used internally by `summary()` and `get_samples()`)

# estimate_secondary

- `predictions`: `get_predictions`? or `predict()`? This could be used for all three models to get posterior predictions, which would help with a few other things (plotting, scoringutils integration...)
-  `posterior`: same as `samples` in `estimate_infections`
- `data`: same as `observations` in `estimate_infections`
- `fit`: same as `fit` in `estimate_infections`

# estimate_truncation

- `dist` this will have to be accessed using the summary or `get_samples()` - we could add an extra accessor function to get a `dist_fit()` using mean and sd of samples if we think this is a good idea
- `obs`: same as `observations` in `estimate_infections`
- `last_obs`: this is really no needed as contained in `obs`
- `cmf`: not needed as can be obtained from `dist` (and is already included in plotting `<dist_spec>`)
- `data`: same as `observations` in `estimate_infections`
- `fit`: same as `fit` in `estimate_infections`
