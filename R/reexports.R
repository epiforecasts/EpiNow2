# Distribution interface re-exported from distspec for backwards compatibility.

# Warn only when a re-exported function is called via the `EpiNow2::` prefix
# (the usage being deprecated). Bare calls resolve here too, because EpiNow2
# sits above distspec on the search path, but are left silent since they were
# never the target and behave identically to distspec.
reexport_deprecate <- function(name) {
  fn <- sys.call(-1L)[[1L]]
  if (is.call(fn) && identical(fn[[1L]], quote(`::`))) {
    deprecate_warn(
      "1.10.0",
      paste0("EpiNow2::", name, "()"),
      paste0("distspec::", name, "()"),
      user_env = rlang::caller_env(2L)
    )
  }
}

#' Distribution functions re-exported from distspec
#'
#' `r lifecycle::badge("deprecated")`
#'
#' The probability distribution interface has moved to the \pkg{distspec}
#' package. These functions are re-exported so that the `EpiNow2::` form keeps
#' resolving, and will be removed in a future release; use \pkg{distspec}
#' directly (the bare names are attached when EpiNow2 is loaded).
#'
#' @param ... Passed to the \pkg{distspec} function of the same name.
#' @name distspec-reexports
#' @keywords internal
NULL

#' @rdname distspec-reexports
#' @export
Gamma <- function(...) {
  reexport_deprecate("Gamma")
  distspec::Gamma(...)
}

#' @rdname distspec-reexports
#' @export
LogNormal <- function(...) {
  reexport_deprecate("LogNormal")
  distspec::LogNormal(...)
}

#' @rdname distspec-reexports
#' @export
Normal <- function(...) {
  reexport_deprecate("Normal")
  distspec::Normal(...)
}

#' @rdname distspec-reexports
#' @export
Fixed <- function(...) {
  reexport_deprecate("Fixed")
  distspec::Fixed(...)
}

#' @rdname distspec-reexports
#' @export
Exp <- function(...) {
  reexport_deprecate("Exp")
  distspec::Exp(...)
}

#' @rdname distspec-reexports
#' @export
Weibull <- function(...) {
  reexport_deprecate("Weibull")
  distspec::Weibull(...)
}

#' @rdname distspec-reexports
#' @export
Dirichlet <- function(...) {
  reexport_deprecate("Dirichlet")
  distspec::Dirichlet(...)
}

#' @rdname distspec-reexports
#' @export
NonParametric <- function(...) {
  reexport_deprecate("NonParametric")
  distspec::NonParametric(...)
}

#' @rdname distspec-reexports
#' @export
discretise <- function(...) {
  reexport_deprecate("discretise")
  distspec::discretise(...)
}

#' @rdname distspec-reexports
#' @export
discretize <- function(...) {
  reexport_deprecate("discretize")
  distspec::discretize(...)
}

#' @rdname distspec-reexports
#' @export
get_pmf <- function(...) {
  reexport_deprecate("get_pmf")
  distspec::get_pmf(...)
}

#' @rdname distspec-reexports
#' @export
convert_to_logmean <- function(...) {
  reexport_deprecate("convert_to_logmean")
  distspec::convert_to_logmean(...)
}

#' @rdname distspec-reexports
#' @export
convert_to_logsd <- function(...) {
  reexport_deprecate("convert_to_logsd")
  distspec::convert_to_logsd(...)
}

#' @rdname distspec-reexports
#' @export
fix_parameters <- function(...) {
  reexport_deprecate("fix_parameters")
  distspec::fix_parameters(...)
}

#' @rdname distspec-reexports
#' @export
get_distribution <- function(...) {
  reexport_deprecate("get_distribution")
  distspec::get_distribution(...)
}

#' @rdname distspec-reexports
#' @export
get_parameters <- function(...) {
  reexport_deprecate("get_parameters")
  distspec::get_parameters(...)
}

#' @rdname distspec-reexports
#' @export
is_constrained <- function(...) {
  reexport_deprecate("is_constrained")
  distspec::is_constrained(...)
}

#' @rdname distspec-reexports
#' @export
bound_dist <- function(...) {
  reexport_deprecate("bound_dist")
  distspec::bound_dist(...)
}

#' @rdname distspec-reexports
#' @export
collapse <- function(...) {
  reexport_deprecate("collapse")
  distspec::collapse(...)
}

#' @rdname distspec-reexports
#' @export
new_dist_spec <- function(...) {
  reexport_deprecate("new_dist_spec")
  distspec::new_dist_spec(...)
}
