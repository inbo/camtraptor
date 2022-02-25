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
#'   among all deployments is used.
#' @param end End date. Default: `NULL`. If `NULL` the latest end date among all
#'   deployments is used.
#' @param group_by Character, one of `"day"`, `"week"`, `"month"`, `"year"`. The
#'   effort is calculated at the interval rate defined in `group_by`. Default:
#'   `NULL`: no grouping, i.e. the entire interval from `start` to `end` is
#'   taken into account as a whole.
#' @param unit Character, the time unit to use while returning custom effort.
#'   One of: `hour` (default), `day`.
#'
#' @param ... filter predicates
#' @importFrom dplyr .data %>%
#' @export
#' @return A tibble (data.frame) with following columns:
#'
#' - `begin`: Date: begin of the interval the effort is calculated over.
#' - `effort`: The effort as number.
#' - `effort_unit`: Character specifying the effort_unit.
#' - `year`: Numeric, the year `begin` belongs to. This column is not present if
#' `group_by` is `NULL`.
#' - `month`: Numeric, the month `begin` belongs to. This column is present if
#' `group_by` is `"month"` or `"day"`.
#' - `week`: Numeric, the week `begin` belongs to. This column is present if
#' `group_by` is `"week"`.
#' - `day`: Numeric, the day `begin` belongs to. This column is present if
#' `group_by` is `"day"`.
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

  # set start to date of the earliest deployment
  if (is.null(start)) start <- sum_effort$date[1]
  if (is.null(end)) end <- sum_effort$date[nrow(sum_effort)]
  # check start earlier than end
  assertthat::assert_that(start < end,
                          msg = "start must be earlier than end.")

  # filter by start and end date
  sum_effort <- sum_effort %>%
    filter(.data$date >= start & .data$date <= end)

  if (is.null(group_by)) {
    # total effort (days) over all deployments
    sum_effort <-
      sum_effort %>%
        dplyr::summarise(begin = min(.data$date, na.rm = TRUE),
                         effort = sum(.data$sum_effort))
  } else {
    # add year column
    sum_effort <- sum_effort %>%
      dplyr::mutate(year = lubridate::isoyear(.data$date))

    if (group_by == "year") {
      sum_effort <- sum_effort %>% dplyr::group_by(.data$year)
    }

    if (group_by == "month") {
      # add month column
      sum_effort <- sum_effort %>%
        dplyr::mutate(month = lubridate::month(.data$date)) %>%
        dplyr::group_by(.data$year, .data$month)
    }

    if (group_by == "week") {
      # add week column
      sum_effort <- sum_effort %>%
        mutate(week = lubridate::isoweek(.data$date)) %>%
        group_by(.data$year, .data$week)
    }

    if (group_by == "day") {
      # add day column
      sum_effort <- sum_effort %>%
        mutate(month = lubridate::month(.data$date),
               day = lubridate::day(.data$date)) %>%
        group_by(.data$year, .data$month, .data$day)
    }

    # sum total effort over each interval
    sum_effort <-
      sum_effort %>%
      dplyr::summarise(begin = min(.data$date, na.rm = TRUE),
                       effort = sum(.data$sum_effort)) %>%
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
                  .data$unit,
                  dplyr::everything())
}
