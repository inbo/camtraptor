#' Get camera operation matrix
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Please use [camtrapR_cameraOperation()] instead.
#' 
#' @inherit camtrapR_cameraOperation
#' @export
get_cam_op <- function(x,
                       station_col = "locationName",
                       camera_col = NULL,
                       session_col = NULL,
                       use_prefix = FALSE) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "get_cam_op()",
    with = "camtrapR_cameraOperation()"
  )
  camtrapR_cameraOperation(x,
                           station_col = station_col,
                           camera_col = camera_col,
                           session_col = session_col,
                           use_prefix = use_prefix)
}
