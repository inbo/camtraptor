#' Find date of the begin of the given calendar period
#' 
#' This function calculates the date of the calendar start based on the
#' time period defined in `group_time_by`.
#' 
#' @param my_date A datetime object.
#' @param period `NULL` or character, one of:
#'  - `day`
#'  - `week`
#'  - `month`
#'  - `year`
#' @return A date object with the calendar start of the time group.
#' @noRd
calendar_floor_date <- function(my_date, period) {
  if (is.null(period)) {
    my_date
  } else if (period == "day") {
    lubridate::floor_date(my_date, unit = "day")
  } else if (period == "week") {
    lubridate::floor_date(my_date, unit = "week")
  } else if (period == "month") {
    lubridate::floor_date(my_date, unit = "month")
  } else if (period == "year") {
    lubridate::floor_date(my_date, unit = "year")
  }
  else {
    stop(glue::glue("Unknown period value: {period}."))
  }
}

#' Find date of the end of the given calendar period
#' 
#' This function calculates the date of the calendar end based on the
#' time period defined in `group_time_by`.
#' 
#' @param my_date A datetime object.
#' @param period `NULL` or character, one of:
#'  - `day`
#'  - `week`
#'  - `month`
#'  - `year`
#' @return A date object with the calendar end of the time period.
#' @noRd
calendar_ceiling_date <- function(my_date, period) {
  if (is.null(period)) {
    my_date
  } else if (period == "day") {
    lubridate::ceiling_date(my_date, unit = "day")
  } else if (period == "week") {
    lubridate::ceiling_date(my_date, unit = "week")
  } else if (period == "month") {
    lubridate::ceiling_date(my_date, unit = "month")
  } else if (period == "year") {
    lubridate::ceiling_date(my_date, unit = "year")
  }
  else {
    stop(glue::glue("Unknown period value: {period}"))
  }
}

#' Create date series based on a deployment start/end and the time grouping
#' 
#' This function creates start/end date series for a given deployment, based on
#' the `deploymentStart`, `deploymentEnd` and the given time grouping.
#' 
#' @param deployment_id Character, deployment ID.
#' @param deployments A tibble data frame with deployments.
#' @param group_time_by `NULL` or character, one of:
#'  - `NULL`: No grouping, return the deployment start/end dates.
#'  - `day`: Group by day.
#'  - `week`: Group by week.
#'  - `month`: Group by month.
#'  - `year`: Group by year.
#' @return A tibble data frame with three columns:
#' - `start`: start dates.
#' - `end`: end dates.
#' - `deploymentID`: the ID of the deployment.
#' @noRd
create_date_series <- function(deployment_id, deployments, group_time_by) {
  # Select the deployment by ID
  deployment <- deployments %>%
    dplyr::filter(.data$deploymentID == deployment_id)
  # Get start datetimes of the deployment
  start_date <- deployment %>%
    dplyr::pull("deploymentStart")
  # Calculate floor/ceiling start dates, based on the time grouping
  start_floor_date <- calendar_floor_date(start_date, group_time_by)
  start_ceiling_date <- calendar_ceiling_date(start_date, group_time_by)
  # Get end datetimes of the deployment
  end_date <- deployments %>%
    dplyr::filter(.data$deploymentID == deployment_id) %>%
    dplyr::pull("deploymentEnd")
  # Calculate floor/ceiling end dates, based on the time grouping
  end_floor_date <- calendar_floor_date(end_date, group_time_by)
  end_ceiling_date <- calendar_ceiling_date(end_date, group_time_by)
  # Create a vector with all datetimes the time groups start and end
  if (is.null(group_time_by)) {
    start_date_series <- start_date
    end_date_series <- end_date
  } else {
    start_date_series <- lubridate::as_datetime(
      seq.Date(
        from = lubridate::date(start_floor_date),
        to = lubridate::date(end_floor_date),
        by = group_time_by
      )
    )
    end_date_series <- lubridate::as_datetime(
      seq.Date(
        from = lubridate::date(start_ceiling_date),
        to = lubridate::date(end_ceiling_date),
        by = group_time_by
      )
    )
  }
  # Return a tibble dataframe with the deployment ID
  # and the start/end date series
  dplyr::tibble(
    start = start_date_series,
    end = end_date_series,
    deploymentID = deployment_id
  )
}

#' Enrich deployment information with date series
#'
#' This function enriches the information about one deployment with date series
#' based on its `deploymentStart`, `deploymentEnd` and the given time grouping.
#' This function uses `create_date_series()` for creating the date series.
#' 
#' @param group_by A character vector of deployment column names to add to
#'   the date series.
#' @inheritParams create_date_series
#' @return A tibble data frame with start/end dates, the deployment ID and the
#'   deployments columns in `group_by`.
#' @noRd
enrich_deployment <- function(deployment_id,
                              deployments,
                              group_by,
                              group_time_by) {
  create_date_series(
    deployments = deployments,
    deployment_id = deployment_id,
    group_time_by = group_time_by
  ) %>%
    # Add needed deployments columns
    dplyr::left_join(
      deployments %>%
        dplyr::select(
          "deploymentID",
          "deploymentStart",
          "deploymentEnd",
          dplyr::any_of(group_by)
        ),
      by = "deploymentID"
    )
}

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
#'   `c("deploymentID", "latitude", "longitude", "locationID", "locationName",
#'   "deploymentStart", "deploymentEnd", "deploymentTags")`. Default:
#'   `c("deploymentID", "latitude", "longitude")`.
#' @param group_time_by Character, one of `"day"`, `"week"`, `"month"`,
#'   `"year"`. The effort is calculated at the interval rate defined in
#'   `group_time_by`. Default: `NULL`, no grouping, i.e. the entire duration of
#'   the deployment is taken into account as a whole.
#' @return A grouped tibble data frame with the following columns:
#'   - `group_by` names, e.g. `deploymentID`, `latitude`, `longitude` and 
#'   `locationName`.
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
#' # Return effort using default `group_by` and no time grouping
#' summarize_deployments(x)
#' 
#' # Return effort using default `group_by` and grouping by year
#' summarize_deployments(x, group_time_by = "year")
#' 
#' # Return effort specifying grouping columns, e.g. `deploymentID` and
#' # `locationName` and grouping by day
#' summarize_deployments(
#'   x,
#'   group_by = c("deploymentID", "locationName"),
#'   group_time_by = "day"
#' )
#' 
#' # Afterwards, you can calculate the total effort over all deployments. You 
#' # can also show other information, e.g. the (number of) deployments and
#' # locations.
#' library(dplyr)
#' summarize_deployments(
#'   x,
#'   group_by = c("deploymentID", "locationName"),
#'   group_time_by = "month"
#' ) %>%
#'   group_by(month) %>%
#'   summarise(
#'     deploymentIDs = list(deploymentID),
#'     ndep = length(unique(deploymentID)),
#'     nloc = length(unique(locationName)),
#'     effort_duration = sum(effort_duration)
#'   )
summarize_deployments <- function(
    x,
    group_by = c("deploymentID", "latitude", "longitude"),
    group_time_by = NULL
) {
  # Check Camera Trap Data Package
  camtrapdp::check_camtrapdp(x)

  # Check `group_by`
  check_value(
    group_by,
    .group_bys_deployments,
    "group_by",
    null_allowed = FALSE
  )
  
  # Check `group_time_by`
  check_group_time_by(group_time_by, .group_time_bys)
  
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
      effort_duration = sum(.data$effort_duration, na.rm = TRUE),
      .groups = "keep"
    ) %>%
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
  effort_per_deploy_df <- enrich_deployment(
    deployment_id = deployment_id,
    deployments = deployments,
    group_by = group_by,
    group_time_by = group_time_by) %>%
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
