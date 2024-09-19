#' Get number of identified species for each deployment
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Please use [n_species()] instead.
#' 
#' @inherit n_species
#' @export
get_n_species <- function(x) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "get_n_species()",
    with = "n_species()"
  )
  get_n_species(x)
}
