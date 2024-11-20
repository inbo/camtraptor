#' Get effort
#'
#' Gets the effort (deployment duration) per deployment.
#'
#' @param unit Time unit to use while returning deployment effort (duration).
#'   One of:
#'   - `second`
#'   - `minute`
#'   - `hour`
#'   - `day`
#'   - `month`
#'   - `year`
#' @inheritParams n_species
#' @return A tibble data frame with following columns:
#'   - `deploymentID`: Deployment unique identifier.
#'   - `effort`: Effort expressed in the unit passed by parameter `unit`.
#'   - `unit`: The unit used to express the effort.
#'   One of the values available for parameter `unit`.
#'   - `effort_duration`: A duration object (duration is a class from lubridate
#'   package).
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' 
#' # Efforts expressed in hours
#' get_effort(x)
#'
#' # Effort expressed as days
#' get_effort(x, unit = "day")
get_effort <- function(x,
                       unit = "hour") {
  # Define possible unit values
  units <- c("second", "minute", "hour", "day", "month", "year")

  # Check unit
  check_value(unit, units, "unit", null_allowed = FALSE)

  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)

  # Get deployments
  deployments <- deployments(x)

  # Calculate effort of deployments
  effort_df <-
    deployments %>%
    dplyr::mutate(
      effort_duration = 
        lubridate::as.duration(.data$deploymentEnd - .data$deploymentStart)
      ) %>%
    dplyr::select("deploymentID", "effort_duration")
  # Convert effort duration in specified effort time units (arg units)
  effort_df$effort <- transform_effort_to_common_units(
    effort = effort_df$effort_duration,
    unit = unit
  )
  effort_df$unit <- unit
  effort_df %>%
    dplyr::relocate(
      "deploymentID",
      "effort",
      "unit",
      "effort_duration"
    )
}

#' Transform efforts to common units.
#'
#' This function is useful for visualization and communication purposes.
#' Efforts are duration objects and so they are always expressed in seconds.
#' Although they are also returned on screen in common units, e.g. "2594308s
#' (~4.29 weeks)", the values in seconds are used in colour scales, not very
#' handy to be interpreted.
#' Converting them in the most suitable time unit is also useful for
#' communication purposes (reports, research articles, ...).
#' Obviously the conversion can be not always precise, e.g. a month is not
#' always 30 days long.
#'
#' @param effort A vector of duration objects.
#' @param unit Common unit to express duration objects.
#'   One of:
#'   - `second`
#'   - `minute`
#'   - `hour`
#'   - `day`
#'   - `month`
#'   - `year`
#' @return A numeric vector.
#' @noRd
#' @examples
#' # Create efforts (durations) to transform
#' efforts <- c(
#'   lubridate::duration("2hours 2minutes 1second"),
#'   lubridate::duration("3days 2hours")
#' )
#'
#' # Transform effort to hours
#' transform_effort_to_common_units(efforts, "hour")
#'
#' # Transform effort to days
#' transform_effort_to_common_units(efforts, "day")
transform_effort_to_common_units <- function(effort, unit) {
  # only one unit allowed
  assertthat::assert_that(length(unit) == 1,
    msg = "unit must have length 1"
  )

  # define possible unit values
  units <- c("second", "minute", "hour", "day", "week", "month", "year")

  # check unit
  check_value(unit, units, "unit", null_allowed = FALSE)

  if (unit == "second") {
    effort / lubridate::dseconds()
  } else if (unit == "minute") {
    effort / lubridate::dminutes()
  } else if (unit == "hour") {
    effort / lubridate::dhours()
  } else if (unit == "day") {
    effort / lubridate::ddays()
  } else if (unit == "week") {
    effort / lubridate::dweeks()
  } else if (unit == "month") {
    effort / lubridate::dmonths()
  } else if (unit == "year") {
    effort / lubridate::dyears()
  }
}
