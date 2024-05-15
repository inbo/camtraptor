#' Read a Camera Trap Data Package
#'
#' Reads files from a [Camera Trap Data Package (Camtrap DP)](
#' https://camtrap-dp.tdwg.org) into memory.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @return Camera Trap Data Package object.
#' @export
read_camtrap_dp <- function(file) {
  .Deprecated(new = "read_camtrapdp", package = "camtraptor")
  read_camtrapdp(file)
}
