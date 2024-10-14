#' Get custom effort
#'
#' Gets the effort for each deployment and a specific time interval such as day,
#' week, month or year. A custom time window can also be set up. This function
#' calls `get_cam_op()` internally.
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param ... Filter predicates
#' @param start Start date.
#'   Default: `NULL`.
#'   If `NULL` the earliest start date among all deployments is used.
#'   If `group_by` unit is not `NULL`, the lowest start value allowed is one
#'   group by unit before the start date of the earliest deployment.
#'   If this condition doesn't hold true, a warning is returned and the earliest
#'   start date among all deployments is used.
#'   If `group_by` unit is `NULL` the start must be later than or equal to the
#'   start date among all deployments.
#' @param end End date.
#'   Default: `NULL`.
#'   If `NULL` the latest end date among all deployments is used.
#'   If `group_by` unit is not `NULL`, the latest end value allowed is one group
#'   by unit after the end date of the latest deployment.
#'   If this condition doesn't hold true, a warning is returned and the latest
#'   end date among all deployments is used.
#'   If `group_by` unit is `NULL` the end must be earlier than or equal to the
#'   end date among all deployments.
#' @param group_by Character, one of `"day"`, `"week"`, `"month"`, `"year"`.
#'   The effort is calculated at the interval rate defined in `group_by`.
#'   Default: `NULL`: no grouping, i.e. the entire interval from `start` to
#'   `end` is taken into account as a whole. Calendar values are used, i.e.
#'   grouping by year will calculate the effort from Jan 1st up to Dec 31st for
#'   each year.
#' @param unit Character, the time unit to use while returning custom effort.
#'   One of: `hour` (default), `day`.
#' @param datapkg Deprecated.
#'   Use `package` instead.
#' @param ... filter predicates
#' @return A tibble data frame with following columns:
#'   - `deploymentID`: Deployment unique identifier.
#'   - `locationName`: Location name of the deployments.
#'   - `begin`: Begin date of the interval the effort is calculated over.
#'   - `effort`: The effort as number.
#'   - `unit`: Character specifying the effort unit.
#' @family exploration functions
#' @importFrom dplyr .data %>%
#' @export
#' @examples
#' # Effort for each deployment over the entire duration of the project
#' # (datapackage) measured in hours (default)
#' get_custom_effort(mica)
#'
#' # Effort for each deployment expressed in days
#' get_custom_effort(mica, unit = "day")
#'
#' # Effort for each deployment from a specific start to a specific end
#' get_custom_effort(
#'   mica,
#'   start = as.Date("2019-12-15"), # or lubridate::as_date("2019-12-15")
#'   end = as.Date("2021-01-10")
#' )
#'
#' # Effort for each deployment at daily interval
#' get_custom_effort(
#'   mica,
#'   group_by = "day"
#' )
#'
#' # Effort for each deployment at weekly interval
#' get_custom_effort(
#'   mica,
#'   group_by = "week"
#' )
#'
#' # Effort for each deployment at monthly interval
#' get_custom_effort(
#'   mica,
#'   group_by = "month"
#' )
#'
#' # Effort for each deployment at yearly interval
#' get_custom_effort(
#'   mica,
#'   group_by = "year"
#' )
#'
#' # Applying filter(s), e.g. deployments with latitude >= 51.18, can be
#' # combined with other arguments
#' get_custom_effort(mica, pred_gte("latitude", 51.18), group_by = "month")
#' 
#' # You can afterwards calculate the total effort over all deployments
#' library(dplyr)
#' get_custom_effort(mica, group_by = "year", unit = "day") %>%
#'   dplyr::filter(effort > 0) %>%
#'   dplyr::group_by(begin) %>% 
#'   dplyr::summarise(
#'     deploymentIDs = list(deploymentID),
#'     locationNames = list(locationName),
#'     ndep = length(unique(deploymentID)),
#'     nloc = length(unique(locationName)),
#'     effort = sum(effort),
#'     unit = unique(unit)
#'   )
get_custom_effort <- function(package = NULL,
                              ...,
                              start = NULL,
                              end = NULL,
                              group_by = NULL,
                              unit = "hour",
                              datapkg = lifecycle::deprecated()) {
  # Check start earlier than end
  if (!is.null(start) & !is.null(end)) {
    assertthat::assert_that(start <= end,
                            msg = "`start` must be earlier than `end`."
    )
  }
  
  # Check start and end are both dates
  assertthat::assert_that(
    is.null(start) | all(class(start) == "Date"),
    msg = glue::glue(
      "`start` must be `NULL` or an object of class Date. ",
      "Did you forget to convert a string to Date with `as.Date()`?"
    )
  )
  assertthat::assert_that(
    is.null(end) | all(class(end) == "Date"),
    msg = glue::glue(
      "`end` must be `NULL` or an object of class Date. ",
      "Did you forget to convert a string to Date with `as.Date()`?"
    )
  )
  
  # Define possible unit values
  units <- c("hour", "day")

  # Check unit
  check_value(unit, units, "unit", null_allowed = FALSE)

  # Define possible group_by values
  group_bys <- c(
    "day",
    "week",
    "month",
    "year"
  )
  durations <- c(
    lubridate::ddays(x = 1),
    lubridate::dweeks(x = 1),
    lubridate::dmonths(x = 1),
    lubridate::dyears(x = 1)
  )

  # Check group_by
  check_value(group_by, group_bys, "group_by", null_allowed = TRUE)

  # Check camera trap data package
  check_package(package, datapkg, "get_custom_effort")
  if (is.null(package) & !is.name(datapkg)) {
    package <- datapkg
  }
  
  # Get deployments
  deployments <- package$data$deployments

  # Stop function and inform user about deployments with missing `start` date
  no_start_deployments <- deployments[is.na(deployments$start),]$deploymentID
  if (length(no_start_deployments) > 0) {
    stop(
      glue::glue(
        "The deployments with the following deploymentID ",
        "have missing `start` value: ",
        glue::glue_collapse(no_start_deployments, sep = ", ", last = " and "),
        "."
      )
    )
  }
  
  # Stop function and inform user about deployments with missing `end` date
  no_end_deployments <- deployments[is.na(deployments$end),]$deploymentID
  if (length(no_end_deployments) > 0) {
    stop(
      glue::glue(
        "The deployments with the following deploymentID ",
        "have missing `end` value: ",
        glue::glue_collapse(no_end_deployments, sep = ", ", last = " and "),
        "."
      )
    )
  }
  
  # Camera operation matrix with filter(s) on deployments
  cam_op <- get_cam_op(package, ..., station_col = "deploymentID")

  # Transform camera operation matrix to df with effort per deployment
  dep_effort <- cam_op %>%
    dplyr::as_tibble(rownames = "deploymentID") %>%
    tidyr::pivot_longer(cols = -"deploymentID",
                        names_to = "date",
                        values_to = "effort") %>%
    dplyr::mutate(date = lubridate::as_date(.data$date))
  
  # Check start is earlier than end of the latest deployment
  if (!is.null(start)) {
    assertthat::assert_that(
      start <= max(dep_effort$date),
      msg = glue::glue(
        "`start` value is set too late. ",
        "`start` value must be not later than the end of the ", 
        "latest deployment: {max(dep_effort$date)}."
      )
    )
  }
  
  # Check end is later than begin of the earliest deployment
  if (!is.null(end)) {
    assertthat::assert_that(
      end >= min(dep_effort$date),
      msg = glue::glue(
        "`end` value is set too early. ",
        "`end` value must be not earlier than the start of the ", 
        "earliest deployment: {min(dep_effort$date)}."
      )
    )
  }
  
  # Check start is not earlier than start first deployment date.
  # Return a warning and set start to first day deployment otherwise.
  if (!is.null(start)) {
    if (lubridate::as_date(start) < min(dep_effort$date)) {
      start <- min(dep_effort$date)
      warning(
        glue::glue(
          "`start` value is set too early. ",
          "`start` authomatically set to start date of earliest ",
          "deployment: {start}."
        )
      )
    }
  } else {
    # Set start to date of the earliest deployment
    start <- min(dep_effort$date)
  }
  # Check end is not later than end last deployment date.
  # Return a warning and set end to last day deployment otherwise.
  if (!is.null(end)) {
    if (lubridate::as_date(end) > max(dep_effort$date)) {
      end <- max(dep_effort$date)
      warning(
        glue::glue(
          "`end` value is set too late. ",
          "`end` authomatically set to end date of latest deployment: {end}."
        )
      )
    }
  } else {
    # Set end to date of the latest deployment
    end <- max(dep_effort$date)
  }
  
  # Create df with all dates from start to end
  dates_df <- dplyr::tibble(date = seq(start, end, by = "days"))

  # Join dates_df to dep_effort
  dep_effort <-
    dates_df %>%
    dplyr::left_join(dep_effort, by = "date")

  # Filter by start and end date
  dep_effort <-
    dep_effort %>%
    dplyr::filter(.data$date >= start & .data$date <= end)

  if (is.null(group_by)) {
    # Calculate total effort (days) per deployment
    sum_effort <-
      dep_effort %>%
      dplyr::group_by(.data$deploymentID) %>%
      dplyr::summarise(
        begin = start,
        effort = sum(.data$effort, na.rm = TRUE)
      )
  } else {
    # Calculate total effort (days) per deployment and given temporal grouping
    sum_effort <-
      dep_effort %>%
      dplyr::mutate(
        begin = lubridate::floor_date(.data$date, unit = group_by)) %>%
      dplyr::group_by(.data$deploymentID, .data$begin) %>%
      dplyr::summarise(effort = sum(.data$effort, na.rm = TRUE))
  }

  # Transform effort to hours if needed
  if (unit == "hour") {
    sum_effort <-
      sum_effort %>%
      dplyr::ungroup() %>%
      dplyr::mutate(effort = .data$effort * 24)
  }

  # Add locations (`locationName`)
  sum_effort <- dplyr::left_join(
    sum_effort,
    dplyr::select(deployments, "deploymentID", "locationName"),
    by = "deploymentID"
  )
  
  # Add unit column and adjust column order
  sum_effort %>%
    dplyr::mutate(unit = unit) %>%
    dplyr::select(
      "deploymentID",
      "locationName",
      "begin",
      "effort",
      "unit"
    )
}
