#' Retrieve deployment data from a Camera Trap Data Package
#'
#' @param package 
#'
#' @return A tibble with the deployments of the `package`
#' @export
#'
#' @examples deployments(mica)
deployments <- function(package) {
  purrr::chuck(package,"data","deployments")
}