#' Get custom effort
#'
#' Gets the custom effort (deployment duration) for a custom time window and a
#' specific time interval such as day, week, month or year. The custom effort is also
#' calculated over all deployments, although filtering predicates can be applied
#' as well. This function calls `get_cam_op()` internally.
#'
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
#' @param ... filter predicates
#' @inheritParams get_species
#' @return A tibble data frame with following columns:
#'   - `begin`: Begin date of the interval the effort is calculated over.
#'   - `effort`: The effort as number.
#'   - `unit`: Character specifying the effort unit.
#' @family exploration functions
#' @importFrom dplyr .data %>%
#' @export
#' @examples
#' # A global effort over the entire duration of the project (camera trap data
#' package) measured in hours
#' get_custom_effort(mica)
#'
#' # Global effort expressed in days
#' get_custom_effort(mica, unit = "day")
#'
#' # Total effort from a specific start to a specific end
#' get_custom_effort(
#'   mica,
#'   start = as.Date("2019-12-15"), # or lubridate::as_date("2019-12-15")
#'   end = as.Date("2021-01-10")
#' )
#'
#' # Effort at daily interval
#' get_custom_effort(
#'   mica,
#'   group_by = "day"
#' )
#'
#' # Effort at weekly interval
#' get_custom_effort(
#'   mica,
#'   group_by = "week"
#' )
#'
#' # Effort at monthly interval
#' get_custom_effort(
#'   mica,
#'   group_by = "month"
#' )
#'
#' # Effort at yearly interval
#' get_custom_effort(
#'   mica,
#'   group_by = "year"
#' )
#'
#' # Applying filter(s), e.g. deployments with latitude >= 51.18
#' get_custom_effort(mica, pred_gte("latitude", 51.18))
get_custom_effort <- function(package,
                              ...,
                              start = NULL,
                              end = NULL,
                              group_by = NULL,
                              unit = "hour") {
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
  camtrapdp::check_camtrapdp(package)
  
  # Get deployments
  deployments <- camtrapdp::deployments(package)

  # Camera operation matrix with filter(s) on deployments
  cam_op <- get_cam_op(package, ..., station_col = "deploymentID")

  # Sum effort over all deployments for each day  (in day units)
  sum_effort <- colSums(cam_op, na.rm = TRUE, dims = 1)

  sum_effort <- dplyr::tibble(
    date = lubridate::as_date(names(sum_effort)),
    sum_effort = sum_effort
  )

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
  
  # Check start is earlier than end of the latest deployment
  if (!is.null(start)) {
    assertthat::assert_that(
      start <= sum_effort$date[nrow(sum_effort)],
      msg = glue::glue(
        "`start` value is set too late. ",
        "`start` value must be not later than the end of the ", 
        "latest deployment: {sum_effort$date[nrow(sum_effort)]}."
      )
    )
  }
  
  # Check end is later than begin of the earliest deployment
  if (!is.null(end)) {
    assertthat::assert_that(
      end >= sum_effort$date[1],
      msg = glue::glue(
        "`end` value is set too early. ",
        "`end` value must be not earlier than the start of the ", 
        "earliest deployment: {sum_effort$date[1]}."
      )
    )
  }
  
  
  # Check start is not earlier than start first deployment date.
  # Return a warning and set start to first day deployment otherwise.
  if (!is.null(start)) {
    if (lubridate::as_date(start) < sum_effort$date[1]) {
      start <- sum_effort$date[1]
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
    start <- sum_effort$date[1]
  }
  # Check end is not later than end last deployment date.
  # Return a warning and set end to last day deployment otherwise.
  if (!is.null(end)) {
    if (lubridate::as_date(end) > sum_effort$date[nrow(sum_effort)]) {
      end <- sum_effort$date[nrow(sum_effort)]
      warning(
        glue::glue(
          "`end` value is set too late. ",
          "`end` authomatically set to end date of latest deployment: {end}."
        )
      )
    }
  } else {
    # Set end to date of the latest deployment
    end <- sum_effort$date[nrow(sum_effort)]
  }
  
  # Check start earlier than end
  assertthat::assert_that(start <= end,
    msg = "`start` must be earlier than `end`."
  )

  # Create df with all dates from start to end
  dates_df <- dplyr::tibble(date = seq(start, end, by = "days"))

  # Join dates_df to sum_effort
  sum_effort <-
    dates_df %>%
    dplyr::left_join(sum_effort, by = "date")

  # Filter by start and end date
  sum_effort <-
    sum_effort %>%
    dplyr::filter(.data$date >= start & .data$date <= end)

  if (is.null(group_by)) {
    # Calculate total effort (days) over all deployments
    sum_effort <-
      sum_effort %>%
      dplyr::summarise(
        begin = start,
        effort = sum(.data$sum_effort, na.rm = TRUE)
      )
  } else {
    sum_effort <-
      sum_effort %>%
      dplyr::mutate(
        begin = lubridate::floor_date(.data$date, unit = group_by)) %>%
      dplyr::group_by(.data$begin) %>%
      dplyr::summarise(effort = sum(.data$sum_effort, na.rm = TRUE))
  }

  # Transform effort to hours if needed
  if (unit == "hour") {
    sum_effort <-
      sum_effort %>%
      dplyr::mutate(effort = .data$effort * 24)
  }

  # Add unit column and adjust column order
  sum_effort %>%
    dplyr::mutate(unit = unit) %>%
    dplyr::select(
      "begin",
      "effort",
      "unit"
    )
}
