#' Read a Camera Trap Data Package
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Use [read_camtrapdp()] instead.
#' 
#' Reads files from a [Camera Trap Data Package (Camtrap DP)](
#' https://camtrap-dp.tdwg.org) into memory.
#'
#' @inherit camtrapdp::read_camtrapdp
#' @returns Camera Trap Data Package object.
#' @family deprecated read functions
#' @export
read_camtrap_dp <- function(file) {
  lifecycle::deprecate_warn(when = "1.0.0", what = "read_camtrap_dp()", with = "read_camtrapdp()")
  read_camtrapdp(file)
}
