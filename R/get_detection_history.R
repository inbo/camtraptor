#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Please use [camtrapR_detectionHistory()]
#' instead.
#' 
#' @inherit camtrapR_detectionHistory
#' @export
get_detection_history <- function(recordTable,
                                  camOp,
                                  species,
                                  output,
                                  occasionLength = 1,
                                  minActiveDaysPerOccasion = NULL,
                                  maxNumberDays = NULL,
                                  day1 = "station",
                                  buffer = NULL,
                                  unmarkedMultFrameInput = FALSE) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "get_detection_history()",
    with = "camtrapR_detectionHistory()"
  )
  camtrapR_detectionHistory(recordTable = recordTable,
                            camOp = camOp,
                            species = species,
                            output = output,
                            occasionLength = occasionLength,
                            minActiveDaysPerOccasion = minActiveDaysPerOccasion,
                            maxNumberDays = maxNumberDays,
                            day1 = day1,
                            buffer = buffer,
                            unmarkedMultFrameInput = unmarkedMultFrameInput)
}
