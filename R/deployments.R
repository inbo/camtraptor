#' Retrieve deployment data from a Camera Trap Data Package
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#'
#' @return A tibble with the deployments of the `package`
#' @export
#'
#' @examples deployments(mica)
deployments <- function(package) {
  purrr::chuck(package,"data","deployments")
}