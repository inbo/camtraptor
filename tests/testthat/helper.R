#' Get snapshot for write_dwc()
#' 
#' Wrapper of `write_dwc()` that returns path of selected output file.
#' Needed for `testthat::expect_snapshot_file()` which expects the path of a 
#' single file to compare against snapshot.
#' @inheritParams write_dwc
#' @param file Either `occurrence` or `audubon` to select which output file of 
#'   `write_dwc()` to return.
#' @return Path of selected output file.
#' @noRd
#' @examples write_dwc_snapshot(mica, tempdir(), "occurrence")
write_dwc_snapshot <- function(package, directory, file){
  suppressMessages(write_dwc(package, directory))
  switch(
    file,
    occurrence = file.path(directory, "dwc_occurrence.csv"),
    audubon = file.path(directory, "dwc_audubon.csv")
  )
}
