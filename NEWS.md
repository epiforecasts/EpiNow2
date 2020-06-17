# EpiNow 0.3.0

* Added documentation.
* Added GitHub Actions for documentation and CRAN check.
* Added function to map from simulated cases by date of infection to cases by date of report.
* Added function examples
* Update README with introduction, quick start guide, and contribution information
* Added a getting started vignette.
* Improve CRAN compliance and cleaned up CRAN check results

# EpiNow 0.2.0 

* Time-varying window for Rt estimation.
* Refactored all code in `data.table`.
* Refactored code base.
* Added mapping from onset cases to infection with upscaling to account for additional truncation.
* Added minimal example to all functions
* Added an approximate mapping for reporting delay that has an adaptive case threshold.
* Switched to using a generation time estimate rather than a serial interval (see `data-raw/ganyani-generation-time.rs)

# EpiNow 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Extract tooling from global nowcasts.
* Parameterised the report to be generalised.
* Added a regional pipeline and made a national summary nowcast optional.
* Added summary and plotting functions built to work from saved results.
* Added a function to summarise results across regions