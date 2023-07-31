#' Retrieve observations data from a Camera Trap Data Package
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#'   
#' @return A tibble data frame with the observations of the `package`.
#' @export
#'
#' @examples observations(mica)
observations <- function(package) {
  # Trigger any errors for a bad package
  check_package(package)
  # Retrieve the right data, but fail explicitly using purrr
  purrr::chuck(package, "data","observations")
}
