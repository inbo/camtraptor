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
#' @param unit Time unit to use while returning custom effort
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
#' @importFrom dplyr .data %>%
#' @importFrom rlang !!
#' @export
#' @return A tibble (data.frame) with one column:
#' - `effort`: duration object (duration is a class from lubridate package) if `unit` is `NULL`, or a number if `unit` is not `NULL`.
#' - `effort_unit`: Character specifying the effort_unit.
#' @family get_functions
#' @examples
#' # a global effort over the entire duration of the project (datapackage)
#' get_custom_effort(mica)
#'
#' # effort expressed as days
#' get_custom_effort(mica, unit = "day")
#'
#' # effort from a specific start to a specific end
#' get_custom_effort(
#'   mica,
#'   start = as.Date("2019-12-15"), # or lubridate::as_date("2019-12-15")
#'   end = as.Date("2021-01-10")
#' )
#'
#' # effort at weekly interval
#' get_custom_effort(
#'   mica,
#'   start = as.Date("2019-12-15"),
#'   end = as.Date("2021-01-10"),
#'   group_by = "week"
#' )
get_custom_effort <- function(datapkg,
                              ...,
                              start = NULL,
                              end = NULL,
                              group_by = NULL,
                              unit = NULL) {

  # define possible unit values
  units <- c("second",
             "minute",
             "hour",
             "day",
             "month",
             "year")

  # check unit
  check_value(unit, units, "unit", null_allowed = TRUE)

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
  cam_op <- get_cam_op(datapkg, ...)

  # Sum effort over all deployments for each day
  n_deploys <- colSums(cam_op, na.rm = TRUE, dims = 1)

  n_deploys <- dplyr::tibble(date = lubridate::as_date(names(n_deploys)),
                            n_deployments = n_deploys)

  # check start and end are both dates
  assertthat::assert_that(
    is.null(start) | class(start) == "Date",
    msg = glue::glue(
      "start must be NULL or an object of class Date.",
      "Did you maybe forget to convert a string to Date with as.Date() ?"
    )
  )
  assertthat::assert_that(
    is.null(end) | class(end) == "Date",
    msg = glue::glue(
      "end must be NULL or an object of class Date.",
      "Did you maybe forget to convert a string to Date with as.Date() ?"
    )
  )

  # set start to date of the earliest deployment
  if (is.null(start)) start <- n_deploys$date[1]
  if (is.null(end)) end <- n_deploys$date[nrow(n_deploys)]
  # check start earlier than end
  assertthat::assert_that(start < end,
                          msg = "start must be earlier than end.")

  # filter by start and end date
  n_deploys <- n_deploys %>%
    filter(.data$date >= start & .data$date <= end)

  if (is.null(group_by)) {
    n_deploys <-
      n_deploys %>%
        dplyr::summarise(n_deployments = sum(.data$n_deployments))
  } else {
    # add year column
    n_deploys <- n_deploys %>%
      mutate(year = lubridate::year(date))

    if (group_by == "week") {
      # add week column
      n_deploys <- n_deploys %>%
        mutate(week = lubridate::week(date)) %>%
        group_by(year, week)
    }

    if (group_by == "month") {
      # add month column
      n_deploys <- n_deploys %>%
        mutate(m = lubridate::month(date)) %>%
        group_by(year, month)
    }

    # count number of deployments per group
    n_deploys <-
      n_deploys %>%
      dplyr::count(n_deployments, name="effort") %>%
      dplyr::ungroup()
  }

  # transform to unit
  n_deploys
}
