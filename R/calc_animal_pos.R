#' This function is deprecated. Please use [calculate_individual_radius_angle()]
#' instead.
#' 
#' @inherit camtrapR_cameraOperation
#' @export
calc_animal_pos <- function(animal_pos,
                            calib_models,
                            dep_tag = "deploymentID",
                            sequence_id = "sequenceID",
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