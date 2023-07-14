#' Retrieve deployment data from a Camera Trap Data Package
#'
#' @param package 
#'
#' @return
#' @export
#'
#' @examples
deployments <- function(package) {
  purrr::chuck(package,"data","deployments")
}