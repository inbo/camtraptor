#' Read a Camera Trap Data Package
#'
#' `r lifecycle::badge("deprecated")`
#' 
#' Reads files from a [Camera Trap Data Package (Camtrap DP)](
#' https://camtrap-dp.tdwg.org) into memory.
#' 
#' This function is deprecated. Please use [read_camtrapdp()] instead.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @return Camera Trap Data Package object.
#' @export
read_camtrap_dp <- function(file) {
  .Deprecated(new = "read_camtrapdp", package = "camtraptor")
  read_camtrapdp(file)
}
