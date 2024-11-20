#' Get species
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Please use [taxa()] instead.
#'
#' @inheritParams n_species
#' @export
get_species <- function(x) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "get_species()",
    with = "taxa()"
  )
  taxa(x)
}
