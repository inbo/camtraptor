#' Summarize deployments information
#' 
#' Summarizes deployments information, more specifically the duration effort.
#' 
#' `summarize_deployments()` and `summarise_deployments()` are synonyms.
#' 
#' @param x Camera trap data package object, as returned by
#'   [camtrapdp::read_camtrapdp()].
#' @param group_by Character vector with the names of the columns in
#'   deployments. At the moment you can choose one or many columns among:
#'   `c("deploymentID", "locationID", "locationName", "deploymentTags")`.
#'   Default: `deploymentID`.
#' @param group_time_by Character, one of `"day"`, `"week"`, `"month"`,
#'   `"year"`. The effort is calculated at the interval rate defined in
#'   `group_time_by`. Default: `NULL`, no grouping, i.e. the entire duration of
#'   the deployment is taken into account as a whole.
#' @return A tibble data frame with the following columns:
#'   - `group_by` names, e.g. `deploymentID` and `locationName`.
#'   - `group_time_by` name if provided, e.g. `month`. It contains the first
#'   date of the time interval, e.g. the first day of the month.
#'   - `effort_duration`: A duration object (duration is a class from lubridate
#'   package). Duration is always recorded as a fixed number of seconds. See
#'   [lubridate::duration()].
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Return effort per `deploymentID` and `locationName`, by day
#' summarize_deployments(
#'   x,
#'   group_by = c("deploymentID", "locationName"),
#'   group_time_by = "day"
#' )
#' 
#' # Return effort per `deploymentID` and `locationName`. No grouping by time.
#' summarize_deployments(
#'   x,
#'   group_by = c("deploymentID", "locationName")
#' )
#' 
#' # Afterwards, you can calculate the total effort over all deployments. You
#' can also show other information, e.g. the (number of) deployments and
#' locations.
#' library(dplyr)
#' summarize_deployments(
#'   x,
#'   group_by = c("deploymentID", "locationName"),
#'   group_time_by = "month"
#'   ) %>%
#' dplyr::group_by(month) %>%
#' dplyr::summarise(
#'   deploymentIDs = list(deploymentID),
#'   ndep = length(unique(deploymentID)),
#'   nloc = length(unique(locationName)),
#'   effort_duration = sum(effort_duration)
#' )
summarize_deployments <- function(x,
                                  group_by = "deploymentID",
                                  group_time_by = NULL) {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)

  # Check `group_by`
  group_bys <- c("deploymentID", "locationID", "locationName", "deploymentTags")
  check_value(group_by, group_bys, "group_by", null_allowed = FALSE)
  
  # Check `group_time_by`
  group_time_bys <- c("day", "week", "month", "year")
  check_group_time_by(group_time_by, group_time_bys)
  
  deployments <- deployments(x)
  deployment_ids <- purrr::pluck(deployments, "deploymentID")

  # Effort per `deploymentID` and `group_time_by`. It contains also the
  # deployment columns in `group_by`
  effort_df <- deployment_ids %>%
    purrr::map(~ summarize_deployment(
      deployment = .,
      deployments = deployments,
      group_by = group_by,
      group_time_by = group_time_by
    ))
  effort_df <- purrr::list_rbind(effort_df)

  # Calculate the sum of effort over all deployments grouped by `group_by` and
  # `group_time_by`
  effort_df %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_by, group_time_by)))
    ) %>%
    dplyr::summarise(
      effort_duration = sum(.data$effort_duration, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      effort_duration = lubridate::as.duration(.data$effort_duration)
    )
}
#' @rdname summarize_deployments
#' @export
summarise_deployments <- summarize_deployments

#' Summarize deployment information
#' 
#' Summarizes the deployments information for a given deployment.
#' It is the core function used in `summarize_deployments()`.
#' 
#' @param deployment_id The deployment ID to summarize.
#' @param deployments A data frame containing deployment information, including
#'   `deploymentID`, `deploymentStart`, and `deploymentEnd`.
#' @param group_by A character vector of deployment column names to group the
#'   results by. It is only used to pass the columns needed for the
#'   summary afterwards. The grouping occurs at `deploymentID` level and by the
#'   `group_time_by` argument.
#' @param group_time_by A character string indicating the time unit to group the
#'   results by. If `NULL`, no time grouping is applied.
#' @return A tibble data frame with the following columns:
#'   - `deploymentID`: The ID of the deployment.
#'   - `group_by` names, e.g. `locationName`.
#'   - `group_time_by` name if provided, e.g. `month`. It contains the first
#'   date of the time interval, e.g. the first day of the month.
#'   - `effort_duration`: A duration object (duration is a class from lubridate
#'   package). Duration is always recorded as a fixed number of seconds. See
#'   [lubridate::duration()].
#' @noRd
summarize_deployment <- function(deployment_id,
                                 deployments,
                                 group_by,
                                 group_time_by) {
  start_date <- deployments %>% 
    dplyr::filter(.data$deploymentID == deployment_id) %>%
    dplyr::pull("deploymentStart")
  # Find date of the start and end of the given calendar period
  start_floor_date <- calendar_floor_date(start_date, group_time_by)
  start_ceiling_date <- calendar_ceiling_date(start_date, group_time_by)
  end_date <- deployments %>% 
    dplyr::filter(.data$deploymentID == deployment_id) %>%
    dplyr::pull("deploymentEnd")
  end_floor_date <- calendar_floor_date(end_date, group_time_by)
  end_ceiling_date <- calendar_ceiling_date(end_date, group_time_by)
  
  # Create a vector with all datetimes the time groups start and end
  if (is.null(group_time_by)) {
    start_date_series <- start_date
    end_date_series <- end_date
  } else {
    start_date_series <- lubridate::as_datetime(
      seq.Date(from = lubridate::date(start_floor_date),
               to = lubridate::date(end_floor_date),
               by = group_time_by
      )
    )
    end_date_series <- lubridate::as_datetime(
      seq.Date(from = lubridate::date(start_ceiling_date),
               to = lubridate::date(end_ceiling_date),
               by = group_time_by
      )
    )
  }
  
  effort_per_deploy_df <-
  dplyr::tibble(
    start = start_date_series,
    end = end_date_series
  ) %>%
  dplyr::mutate(deploymentID = deployment_id) %>%
  dplyr::left_join(
    deployments,
    by = "deploymentID",
    relationship = "many-to-one",
    unmatched = "drop"
  ) %>%
  # Calculate effort duration for each time group
  dplyr::mutate(effort_duration = lubridate::as.duration(
    pmin(.data$end, .data$deploymentEnd) -
      pmax(.data$start, .data$deploymentStart)
  )) %>%
  dplyr::select(
    dplyr::any_of(c(group_by, "start", "effort_duration"))
  )
if (!is.null(group_time_by)) {
  effort_per_deploy_df %>%
    dplyr::rename(!!group_time_by := start)
} else {
  effort_per_deploy_df
}
}
