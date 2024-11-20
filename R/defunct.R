#' Defunct functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions are not supported anymore. If there's a known replacement,
#' calling the function will tell you about it.
#'
#' @keywords internal
#' @name defunct
NULL

#' @usage # Deprecated in 1.0.0 -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
pred <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred()", "filter_deployments()")
}

#' @export
#' @rdname defunct
pred_not <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_not()", "filter_deployments()")
}

#' @export
#' @rdname defunct
pred_gt <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_gt()", "filter_deployments()")
}

#' @export
#' @rdname defunct
pred_gte <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_gte()", "filter_deployments()")
}

#' @export
#' @rdname defunct
pred_lt <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_lt()", "filter_deployments()")
}

#' @export
#' @rdname defunct
pred_lte <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_lte()", "filter_deployments()")
}


#' @export
#' @rdname defunct
pred_in <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_in()", "filter_deployments()")
}

#' @export
#' @rdname defunct
pred_notin <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_notin()", "filter_deployments()")
}

#' @export
#' @rdname defunct
pred_na <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_na()", "filter_deployments()")
}

#' @export
#' @rdname defunct
pred_notna <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_notna()", "filter_deployments()")
}

#' @export
#' @rdname defunct
pred_and <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_and()", "filter_deployments()")
}

#' @export
#' @rdname defunct
pred_or <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0", "pred_or()", "filter_deployments()")
}

#' @export
#' @rdname defunct
apply_filter_predicate <- function(arg, value) {
  lifecycle::deprecate_stop("1.0.0",
                            "apply_filter_predicate()",
                            "filter_deployments()")
}

#' @export
#' @rdname defunct
check_species <- function(x) {
  lifecycle::deprecate_stop("1.0.0", "check_species()", "taxa()")
}

#' @export
#' @rdname defunct
get_scientific_name <- function(x) {
  lifecycle::deprecate_stop("1.0.0", "get_scientific_name()", "taxa()")
}