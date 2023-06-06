#' Helpers to output file paths
#'
#' `testthat::expect_snapshot_file()` needs a function to output the path of the
#' file it needs to snapshot. This function is a wrapper of `write_dwc()` that
#' outputs the path of either `dwc_occurrence.csv` or `dwc_audubon.csv`
#'
#' @param package A Camtrap DP, as read by `read_camtrap_dp()`.
#' @param directory Character. The directory `write_dwc()` will write to.
#' @param which Character. On of either `occurrence` or `audubon` to select
#'   which of the paths of the two output files of `write_dwc()` to return
#'
#' @noRd
#' @return the path of either `dwc_occurrence.csv` or `dwc_audubon.csv`
#' @examples write_dwc_snapshot(mica, tempdir(), "occurrence")
write_dwc_snapshot <- function(package, directory, which){
  suppressMessages(write_dwc(package, directory))
  switch(
    which,
    occurrence = file.path(directory, "dwc_occurrence.csv"),
    audubon = file.path(directory, "dwc_audubon.csv")
  )
}