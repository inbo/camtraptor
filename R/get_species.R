#' Get species
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' It is deprecated as of camtraptor 1.0.0. Please use [taxa()] instead.
#'
#' @inheritParams summarize_deployments
#' @return A tibble data frame with taxonomic information.
#' @export
#' @examples
#' x <- example_dataset()
#' get_species(x)
get_species <- function(x) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "get_species()",
    with = "taxa()"
  )
  taxa(x)
}
