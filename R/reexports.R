# Distribution interface re-exported from distspec for backwards compatibility.

#' Warn on `EpiNow2::` use of a re-exported distribution function
#'
#' Helper for the deprecated re-exports in this file. When the function was
#' called via the `EpiNow2::` prefix (the usage being deprecated) it issues a
#' deprecation warning pointing to \pkg{distspec}. Bare calls resolve here too,
#' because EpiNow2 sits above distspec on the search path, but are left silent
#' since they behave identically to distspec and were never the target.
#'
#' @param name Name of the re-exported function, used in the message.
#' @return `NULL`, invisibly; called for the deprecation-warning side effect.
#' @keywords internal
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
  distspec::Gamma(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
LogNormal <- function(...) {
  reexport_deprecate("LogNormal")
  distspec::LogNormal(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
Normal <- function(...) {
  reexport_deprecate("Normal")
  distspec::Normal(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
Fixed <- function(...) {
  reexport_deprecate("Fixed")
  distspec::Fixed(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
Exp <- function(...) {
  reexport_deprecate("Exp")
  distspec::Exp(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
Weibull <- function(...) {
  reexport_deprecate("Weibull")
  distspec::Weibull(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
Dirichlet <- function(...) {
  reexport_deprecate("Dirichlet")
  distspec::Dirichlet(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
NonParametric <- function(...) {
  reexport_deprecate("NonParametric")
  distspec::NonParametric(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
discretise <- function(...) {
  reexport_deprecate("discretise")
  distspec::discretise(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
discretize <- function(...) {
  reexport_deprecate("discretize")
  distspec::discretize(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
get_pmf <- function(...) {
  reexport_deprecate("get_pmf")
  distspec::get_pmf(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
convert_to_logmean <- function(...) {
  reexport_deprecate("convert_to_logmean")
  distspec::convert_to_logmean(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
convert_to_logsd <- function(...) {
  reexport_deprecate("convert_to_logsd")
  distspec::convert_to_logsd(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
fix_parameters <- function(...) {
  reexport_deprecate("fix_parameters")
  distspec::fix_parameters(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
get_distribution <- function(...) {
  reexport_deprecate("get_distribution")
  distspec::get_distribution(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
get_parameters <- function(...) {
  reexport_deprecate("get_parameters")
  distspec::get_parameters(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
is_constrained <- function(...) {
  reexport_deprecate("is_constrained")
  distspec::is_constrained(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
bound_dist <- function(...) {
  reexport_deprecate("bound_dist")
  distspec::bound_dist(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
collapse <- function(...) {
  reexport_deprecate("collapse")
  distspec::collapse(...) # nolint: namespace_linter.
}

#' @rdname distspec-reexports
#' @export
new_dist_spec <- function(...) {
  reexport_deprecate("new_dist_spec")
  distspec::new_dist_spec(...) # nolint: namespace_linter.
}
