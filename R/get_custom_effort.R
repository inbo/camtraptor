#' Get custom effort
#'
#' Function to get custom effort (deployment duration) for a custom time windows
#' and a specific time interval such as day or month. The custom effort is also
#' calculated over all deployments, although filtering predicates can be applied
#' as well.
#'
#' @param datapkg Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param ... Filter predicates
#' @param start Start date. Default: `NULL`. If `NULL` the earliest start date
#'   among all deployments is used. If  `group_by` unit is not `NULL`, the
#'   lowest start value allowed is one group by unit before the start date of
#'   the earliest deployment. If this condition doesn't hold true, a warning is
#'   returned and the earliest start date among all deployments is used. If
#'   `group_by` unit is `NULL` the start must be later than or equal to the
#'   start date among all deployments.
#' @param end End date. Default: `NULL`. If `NULL` the latest end date among all
#'   deployments is used.  If  `group_by` unit is not `NULL`, the
#'   latest end value allowed is one group by unit after the end date of
#'   the latest deployment. If this condition doesn't hold true, a warning is
#'   returned and the latest end date among all deployments is used. If
#'   `group_by` unit is `NULL` the end must be earlier than or equal to the
#'   end date among all deployments.
#' @param group_by Character, one of `"day"`, `"week"`, `"month"`, `"year"`. The
#'   effort is calculated at the interval rate defined in `group_by`. Default:
#'   `NULL`: no grouping, i.e. the entire interval from `start` to `end` is
#'   taken into account as a whole. A week is defined as a period of 7 days, a
#'   month as a period of 30 days, a year as a period of 365 days.
#' @param unit Character, the time unit to use while returning custom effort.
#'   One of: `hour` (default), `day`.
#'
#' @param ... filter predicates
#' @importFrom dplyr .data %>%
#' @export
#' @return A tibble (data.frame) with following columns:
#'
#'   - `begin`: Date: begin of the interval the effort is calculated over.
#'   - `effort`: The effort as number.
#'   - `unit`: Character specifying the effort unit.
#' @family get_functions
#' @examples
#' # a global effort over the entire duration of the project (datapackage)
#' # measured in hours
#' get_custom_effort(mica)
#'
#' # global effort expressed in days
#' get_custom_effort(mica, unit = "day")
#'
#' # total effort from a specific start to a specific end
#' get_custom_effort(
#'   mica,
#'   start = as.Date("2019-12-15"), # or lubridate::as_date("2019-12-15")
#'   end = as.Date("2021-01-10")
#' )
#'
#' # effort at daily interval
#' get_custom_effort(
#'   mica,
#'   group_by = "day"
#' )
#' # effort at weekly interval
#' get_custom_effort(
#'   mica,
#'   group_by = "week"
#' )
#' # effort at monthly interval
#' get_custom_effort(
#'   mica,
#'   group_by = "month"
#' )
#' # effort at yearly interval
#' get_custom_effort(
#'   mica,
#'   group_by = "year"
#' )
#' # applying filter(s), e.g. deployments with latitude >= 51.18
#' get_custom_effort(mica, pred_gte("latitude", 51.18))
get_custom_effort <- function(datapkg,
                              ...,
                              start = NULL,
                              end = NULL,
                              group_by = NULL,
                              unit = "hour") {

  # define possible unit values
  units <- c("hour", "day")

  # check unit
  check_value(unit, units, "unit", null_allowed = FALSE)

  # define possible group_by values
  group_bys <- c("day",
                 "week",
                 "month",
                 "year")
  durations <- c(lubridate::ddays(x =1),
                 lubridate::dweeks(x= 1),
                 lubridate::dmonths(x = 1),
                 lubridate::dyears(x = 1))

  # check group_by
  check_value(group_by, group_bys, "group_by", null_allowed = TRUE)

  # check datapackage
  check_datapkg(datapkg)

  # get deployments
  deployments <- datapkg$deployments

  # camera operation matrix with filter(s) on deployments
  cam_op <- get_cam_op(datapkg, ..., use_decimal = TRUE)

  # Sum effort over all deployments for each day  (in day units)
  sum_effort <- colSums(cam_op, na.rm = TRUE, dims = 1)

  sum_effort <- dplyr::tibble(date = lubridate::as_date(names(sum_effort)),
                              sum_effort = sum_effort)

  # check start and end are both dates
  assertthat::assert_that(
    is.null(start) | all(class(start) == "Date"),
    msg = glue::glue(
      "start must be NULL or an object of class Date. ",
      "Did you maybe forget to convert a string to Date with as.Date()?"
    )
  )
  assertthat::assert_that(
    is.null(end) | all(class(end) == "Date"),
    msg = glue::glue(
      "end must be NULL or an object of class Date. ",
      "Did you maybe forget to convert a string to Date with as.Date()?"
    )
  )

  # check start is not earlier than start first deployment - 1 group_by unit
  # duration. Return a warning and set start to first day deployment otherwise.
  if (!is.null(start)) {
    days_diff <- sum_effort$date[1] - lubridate::as_date(start)
    if (!is.null(group_by)) {
      d <- durations[which(group_bys == group_by)]
      earliest_start <- sum_effort$date[1] - d + lubridate::ddays(1)
    } else{
      d <- lubridate::as.duration(0)
      earliest_start <- sum_effort$date[1]
    }
    if (days_diff >= d) {
      start <- sum_effort$date[1]
      warning(
        glue::glue("start argument set too early. ",
                   "Earliest deployment start date: {sum_effort$date[1]}. ",
                   "With the given group_by value ",
                   "the earliest start possible is: {earliest_start}.",
                   "
                   start argument set to start date of earliest deployment: {start}.
                   ")
      )
    }
  }

  # check end is not later than end last deployment + 1 group_by unit
  # duration. Return a warning and set end to last day deployment otherwise.
  if (!is.null(end)) {
    days_diff <- lubridate::as_date(end) - sum_effort$date[nrow(sum_effort)]
    if (!is.null(group_by)) {
      d <- durations[which(group_bys == group_by)]
      latest_end <- sum_effort$date[nrow(sum_effort)] + d - lubridate::ddays(1)
    } else{
      d <- lubridate::as.duration(0)
      latest_end <- sum_effort$date[nrow(sum_effort)]
    }
    if (days_diff >= d) {
      end <- sum_effort$date[nrow(sum_effort)]
      warning(
        glue::glue("end argument set too late. ",
                   "Latest deployment end date: {sum_effort$date[nrow(sum_effort)]}. ",
                   "With the given group_by value ",
                   "the latest end possible is: {latest_end}.",
                   "
                   end argument set to end date of latest deployment: {end}.
                   ")
      )
    }
  }
  # set start to date of the earliest deployment if NULL
  if (is.null(start)) start <- sum_effort$date[1]

  # set end to date of the latest deployment if NULL
  if (is.null(end)) end <- sum_effort$date[nrow(sum_effort)]

  # check start earlier than end
  assertthat::assert_that(start < end,
                          msg = "start must be earlier than end.")

  # create df with all dates from start to end
  dates_df <- dplyr::tibble(date = seq(start, end, by ="days"))

  # join dates_df to sum_effort
  sum_effort <- dates_df %>%
    left_join(sum_effort, by ="date")

  # filter by start and end date
  sum_effort <- sum_effort %>%
    filter(.data$date >= start & .data$date <= end)

  if (is.null(group_by)) {
    # total effort (days) over all deployments
    sum_effort <-
      sum_effort %>%
        dplyr::summarise(begin = start,
                         effort = sum(.data$sum_effort, na.rm = TRUE))
  } else {

    if (group_by == "day") {
      period <- 1 # ndays within a group by unit
    }

    if (group_by == "week") {
      period <- 7 # ndays within a group by unit
    }
    if (group_by == "month") {
      period <- 30 # ndays within a group by unit
    }
    if (group_by == "year") {
      period <- 365 # ndays within a group by unit
    }
    # add period column and group by it
    sum_effort <- sum_effort %>%
      mutate(period = as.numeric(.data$date- .data$date[1]) %/% period) %>%
      group_by(.data$period)

    # sum total effort over each interval
    sum_effort <-
      sum_effort %>%
      dplyr::summarise(begin = min(.data$date, na.rm = TRUE),
                       effort = sum(.data$sum_effort, na.rm = TRUE)) %>%
      dplyr::ungroup()
  }

  # transform effort to hours if needed
  if (unit == "hour") {
    sum_effort <-
      sum_effort %>%
      dplyr::mutate(effort = .data$effort * 24)
  }

  # add unit column and adjust column order
  sum_effort %>%
    dplyr::mutate(unit = unit) %>%
    dplyr::select(.data$begin,
                  .data$effort,
                  .data$unit)
}
