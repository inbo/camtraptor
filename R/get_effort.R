#' Get effort
#'
#' Function to get the effort (deployment duration) per deployment.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param unit time unit to use while returning deployment effort
#'   (duration). One of:
#'
#' - `second`
#' - `minute`
#' - `hour`
#' - `day`
#' - `month`
#' - `year`
#' - `NULL` (default) duration objects (e.g. 2594308s (~4.29 weeks))
#' @param ... filter predicates
#' @importFrom dplyr .data %>% mutate %>% select mutate
#' @importFrom lubridate as.duration
#' @export

#' @return a tibble (data.frame) with the following columns:
#' - `deploymentID` deployment unique identifier
#' - `effort`: a duration object (duration is a class from lubridate package)
#' @family get_functions
#' @examples
#' # efforts expressed as Durations
#' get_effort(mica)
#'
#' # effort expressed as days
#' get_effort(mica, unit = "day")
#'
get_effort <- function(datapkg, ..., unit = NULL) {

  # define possible unit values
  units <- c("second",
             "minute",
             "hour",
             "day",
             "month",
             "year")

  # check unit
  check_value(unit, units, "unit", null_allowed = TRUE)

  # check datapackage
  check_datapkg(datapkg)
  # get deployments
  deployments <- datapkg$deployments

  # apply filtering
  deployments <- apply_filter_predicate(df = deployments, verbose = TRUE, ...)

  # calculate effort of deployments
  effort_df <-
    deployments %>%
    mutate(effort = as.duration(.data$end - .data$start)) %>%
    select(.data$deploymentID, .data$effort)
  # convert effort in specified effort time units (arg units)
  if (!is.null(unit)) {
    effort_df$effort <- transform_effort_to_common_units(
      effort = effort_df$effort,
      unit = unit)
    effort_df$effort_unit <- unit
  } else {
    effort_df$effort_unit <- "Duration"
  }
  return(effort_df)
}

#' Transform efforts to common units.
#'
#' This function is useful for visualization and communication purposes. Efforts
#' are duration objects and so they are always expressed in seconds. Although
#' they are also returned on screen in common units, e.g. "2594308s (~4.29
#' weeks)", the values in seconds are used in color scales, not very handy to be
#' interpreted. Converting them in the most suitable time unit is also useful
#' for communication purposes (reports, research articles, ...). Obviously the
#' conversion can be not always precise, e.g. month is not always 30 days long.
#'
#' @importFrom lubridate dseconds dminutes dhours ddays dweeks dmonths dyears
#'
#' @param effort a vector of duration objects
#' @param unit common unit to express duration objects. One of:
#'
#' - `second`
#' - `minute`
#' - `hour`
#' - `day`
#' - `month`
#' - `year`
#'
#' @export
#'
#' @return a numeric vector
#'
#' @examples
#' # create efforts (durations) to transform
#' efforts <- c(lubridate::duration("2hours 2minutes 1second"),
#'             lubridate::duration("3days 2hours"))
#' # transform effort to hours
#' transform_effort_to_common_units(efforts, "hour")
#' # transform effort to days
#' transform_effort_to_common_units(efforts, "day")
transform_effort_to_common_units <- function(effort, unit) {

  # only one unit allowed
  assert_that(length(unit) == 1,
              msg = "unit must have length 1")

  # define possible effort_unit values
  effort_units <- c("second", "minute", "hour", "day", "week", "month", "year")

  # check effort_unit
  check_value(unit, effort_units, "effort_unit", null_allowed = FALSE)

  if (unit == "second") {
    effort / dseconds()
  } else if (unit == "minute") {
    effort / dminutes()
  } else if (unit == "hour") {
    effort / dhours()
  } else if (unit == "day") {
    effort / ddays()
  } else if (unit == "week") {
    effort / dweeks()
  } else if (unit == "month") {
    effort / dmonths()
  } else if (unit == "year") {
    effort / dyears()
  }
}
