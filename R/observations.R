#' Retrieve observations data from a Camera Trap Data Package
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#'   
#' @return A tibble with the observations of the `package`
#' @export
#'
#' @examples observations(mica)
observations <- function(package) {
  purrr::chuck(package,"data","observations")
}
