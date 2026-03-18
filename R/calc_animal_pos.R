#' Calculate animal position
#' 
#' `r lifecycle::badge("deprecated")`
#' 
#' It is deprecated as of camtraptor 1.0.0. Please use
#' [calculate_individual_radius_angle()] instead.
#' 
#' Calculates the position of animal relative to a camera based on image pixel
#' positions and site calibration models.
#' 
#' @param animal_pos Data frame (tibble) of animal position digitization
#'   data. It must contain (at least) the columns defined in arguments
#'   `deployment_id`, `sequence_id`, `x`, `y`, `image_width` and `image_height`.
#' @param calib_models Named list of deployment calibration models or site
#'   calibration models (`calibs` objects), produced using `cal.site()` (not yet
#'   included in this package). The deployment names are used as names.
#' @param dep_tag Column in `animal_pos` against which names of the
#'   elements can be matched to apply the right deployment calibration models.
#'   Default: `"deploymentID"`.
#' @param sequence_id Column in `animal_pos` containing the sequence ID the
#'   images belong to. Default: `"sequenceID"`.
#' @param x Column in `animal_pos` containing x pixel positions for each
#'   digitised point. Default: `"x"`.
#' @param y Column in `animal_pos` containing y pixel positions for each
#'   digitised point. Default: `"y"`.
#' @param image_width Column in `animal_pos` containing the pixel x
#'   dimension of each image. Default: `"imageWidth"`. Notice that the pixel x
#'   dimension must be consistent for each deployment.
#' @param image_height Column in `animal_pos` containing the pixel y
#'   dimension of each image. Default: `"imageHeight"`. Notice that the pixel y
#'   dimension must be consistent for each deployment.
#' @inherit calculate_individual_radius_angle return
#' @export
#' @examples
#' calc_animal_pos(animal_positions, calibration_models)
calc_animal_pos <- function(
    animal_pos,
    calib_models,
    dep_tag = "deploymentID",
    sequence_id = "eventID",
    x = "x",
    y = "y",
    image_width = "imageWidth",
    image_height = "imageHeight") {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "calc_animal_pos()",
    with = "calculate_individual_radius_angle()"
  )
  calculate_individual_radius_angle(
    animal_positions = animal_pos,
    calibration_models = calib_models,
    deployment_id = dep_tag,
    event_id = sequence_id,
    x = x,
    y = y,
    image_width = image_width,
    image_height = image_height
  )
}