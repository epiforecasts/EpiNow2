#' @keywords internal
#' @import Rcpp
#' @import methods
#' @import rstantools
#' @importFrom rstan sampling extract
#' @importFrom distspec Dirichlet Fixed Gamma LogNormal NonParametric Normal
#' @importFrom distspec bound_dist convert_to_logmean convert_to_logsd
#' @importFrom distspec discretise fix_parameters get_distribution
#' @importFrom distspec get_parameters get_pmf is_constrained lower_bounds
#' @importFrom distspec natural_params ndist new_dist_spec sd
#' @useDynLib EpiNow2, .registration=TRUE
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
#' @importFrom lifecycle deprecate_warn
#' @importFrom lifecycle deprecate_stop
## usethis namespace: end
NULL
