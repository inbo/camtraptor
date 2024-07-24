#' Get record table
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Please use [camtrapR_recordTable()] instead.
#' 
#' @inherit camtrapR_recordTable
#' @export
get_record_table <- function(x,
                             stationCol = "locationName",
                             exclude = NULL,
                             minDeltaTime = 0,
                             deltaTimeComparedTo = NULL,
                             removeDuplicateRecords = TRUE) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "get_record_table()",
    with = "camtrapR_recordTable()"
  )
  camtrapR_recordTable(x,
                       stationCol = stationCol,
                       exclude = exclude,
                       minDeltaTime = minDeltaTime,
                       deltaTimeComparedTo = deltaTimeComparedTo,
                       removeDuplicateRecords = removeDuplicateRecords)
}
