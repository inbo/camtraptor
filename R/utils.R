#' Check input value against list of provided values
#'
#' Will return error message if an input value cannot be found in list of
#' provided values. NULL values can be allowed (default) or not by setting
#' parameter `null_allowed` equal to `TRUE` or `FALSE`.
#'
#' @param arg Character containing the input parameter provided by the user.
#' @param options Character vector of valid inputs for the parameter.
#' @param arg_name Character with the name of the parameter used in the function
#'   to test.
#' @param null_allowed Logical (`TRUE`, the default, or `FALSE`).
#'   Are NULL values allowed?
#' @return If no error, `TRUE`.
#' @noRd
#' @examples
#' \dontrun{
#' # Valid inputs for species
#' check_value("Canis lupus", c("Canis lupus", "Corvus monedula"), "species")
#'
#' # Invalid inputs for species
#' values <- c(
#'   "Ans streperi", # wrong
#'   "Anas strepera",
#'   "wld end", # wrong
#'   "wilde eend"
#' )
#' check_value(values, c("Anas strepera", "wilde eend"), "species")
#' }
check_value <- function(arg, options = NULL, arg_name, null_allowed = TRUE) {
  max_print <- 20

  # Drop NA
  options <- options[!is.na(options)]

  # Wrong values
  wrong_values <- arg[!(arg %in% options)]

  # Suppress long messages with valid options
  if (length(options) > max_print) {
    options_to_print <- c(options[1:max_print], "others...")
  } else {
    options_to_print <- options
  }
  
  # Include NULL
  if (null_allowed) {
    options_to_print <- append(options_to_print, "NULL")
  } else if (is.null(wrong_values)) {
    wrong_values <- "NULL"
  }
  
  # Compose error message
  msg_to_print <- glue::glue(
    "Invalid value for {arg_name} parameter: ",
    glue::glue_collapse(wrong_values, sep = ", ", last = " and "),
    ".\nValid inputs are: ",
    glue::glue_collapse(options_to_print, sep = ", ", last = " and ")
  )

  # Provide user message
  if (!is.null(arg)) {
    assertthat::assert_that(
      all(arg %in% options),
      msg = msg_to_print
    )
  } else {
    assertthat::assert_that(null_allowed == TRUE,
      msg = msg_to_print
    )
  }
}

check_group_time_by <- function(group_time_by, group_time_bys) {
  assertthat::assert_that(
    is.null(group_time_by) | length(group_time_by) == 1,
    msg = "`group_time_by` must have length 1 or NULL."
  )
  check_value(
    group_time_by,
    group_time_bys,
    "group_time_by",
    null_allowed = TRUE
  )
}

#' Get version from data package profile
#'
#' This helper functions returns the version of a Camera Trap Data Package by
#' applying a regex to its `profile`.
#' 
#' The regex rule extracts the version by detecting a sequence of digits and dots.
#' If no dots are detected or `"camtrap-dp-profile.json"` is not part of
#' `profile`, the entire `profile` is returned.
#' 
#' @param profile Character containing the profile of the data package.
#' @return Character containing the data package version.
#' @noRd
get_version <- function(profile) {
  pattern_regex <- "\\d+(\\.\\d+){1,2}" # a sequence of digits and dots (max 2)
  extracted_version <- stringr::str_extract(
    string = profile, 
    pattern = pattern_regex
  )
  if (stringr::str_detect(string = profile, 
                          pattern = stringr::fixed("camtrap-dp-profile.json")) & 
      !is.na(extracted_version)) {
    extracted_version
  } else {
    profile
  }
}

#' Check reading issues
#' 
#' This helper function throws a warning if issues while reading datapackage
#' resources (data.frames) are detected. The issues are also returned as
#' data.frame.
#' 
#' @param df Data.frame.
#' @param df_name Character with name of the data.frame passed to `df`.
#' @return Data.frame containing the issues as returned by `readr::problems()`.
#' @noRd
check_reading_issues <- function(df, df_name) {
  # get problems
  issues_df <- readr::problems(df)
  if (nrow(issues_df) > 0) {
    warning(glue::glue(
      "One or more parsing issues occurred while reading `{df_name}`. ",
      "Check `?read_camtrap_dp()` for examples on how to use ",
      "`readr::problems()`."
    ))
  }
  return(issues_df)
}

#' Custom label format function
#'
#' Add "+" to last label of legend while using absolute scale. At the moment
#' only numeric scale is needed and therefore implemented.
#'
#' @source based on leaflet's
#'   [labelFormat()](https://github.com/rstudio/leaflet/commit/bb3ab964486b357ddc160a7032cfdce6cd8fbe35)
#'    function
#'
#' @param max_scale a number indicating the maximum value of the absolute scale
#'   (`NULL` if relative scale is used, default)
#' @param prefix a prefix of legend labels
#' @param suffix a suffix of legend labels
#' @param digits the number of digits of numeric values in labels
#' @param big.mark the thousand separator
#' @param transform a function to transform the label value
#' @noRd
labelFormat_scale <- function(max_scale = NULL,
                              prefix = "",
                              suffix = "",
                              digits = 3,
                              big.mark = ",",
                              transform = identity) {
  formatNum <- function(x, max_scale) {
    cuts_chrs <- format(round(transform(x), digits),
      trim = TRUE,
      scientific = FALSE,
      big.mark = big.mark
    )
    if (!is.null(max_scale)) {
      n <- length(x)
      if (x[n] == max_scale) {
        cuts_chrs[n] <- paste0(cuts_chrs[n], "+")
      }
    }
    return(cuts_chrs)
  }

  function(type, ...) {
    switch(type,
      numeric = (function(cuts) {
        paste0(prefix, formatNum(cuts, max_scale), suffix)
      })(...)
    )
  }
}

#' Get deployments without observations
#'
#' Return subset of deployments without observations. A message is also returned
#' to list the ID of such deployments.
#'
#' @inheritParams n_species
#' @return A tibble data frame with deployments not linked to any observations.
#' @family exploration functions
#' @noRd
#' @examples
#' x <- example_dataset()
#' get_dep_no_obs(x)
get_dep_no_obs <- function(x) {
  
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  
  # Extract observations and deployments
  observations <- observations(x)
  deployments <- deployments(x)
  
  # Deployment with no observations
  dep_no_obs <-
    deployments %>%
    dplyr::anti_join(observations %>%
      dplyr::distinct(.data$deploymentID),
    by = "deploymentID"
    )

  dep_no_obs_ids <- dep_no_obs$deploymentID
  n_dep_no_obs <- length(dep_no_obs_ids)

  if (n_dep_no_obs > 0) {
    max_print <- 20
    # Suppress long messages
    if (length(dep_no_obs_ids) > max_print) {
      options_to_print <- c(dep_no_obs_ids[1:max_print], "others..")
    } else {
      options_to_print <- dep_no_obs_ids
    }
    message(glue::glue(
      "There are {n_dep_no_obs} deployments without observations: ",
      glue::glue_collapse(options_to_print, sep = ", ", last = " and ")
    ))
  }
  return(dep_no_obs)
}

#' Calculate daily effort for start or end day
#'
#' While assessing the camera operation matrix, start and end day are edge
#' case. The daily effort is a real number between 0 and 1 as and is defined as
#' the fraction of the day the camera was on.
#'
#' @noRd
calc_daily_effort <- function(deploy_df, calc_start = NULL, calc_end = NULL) {
  # check calc_start or calc_end are passed
  assertthat::assert_that(
    (is.null(calc_start) & !is.null(calc_end)) |
      (!is.null(calc_start) & is.null(calc_end)),
    msg = "Either calc_start or calc_end must be defined."
  )
  deploy_df <-
    deploy_df %>%
    dplyr::mutate(
      edge = dplyr::if_else(
        !is.null(calc_start), 
        .data$deploymentStart, .data$deploymentEnd
        ),
      edge_day = dplyr::if_else(!is.null(calc_start), .data$start_day, .data$end_day)
    )
  deploy_df %>%
    # calculate the duration of the start/end day (edge day)
    dplyr::mutate(
      edge_day_duration =
        lubridate::as.duration(lubridate::as_datetime(.data$edge_day) +
          lubridate::ddays(1) -
          lubridate::as_datetime(.data$edge_day))
    ) %>%
    # calculate the duration of the active part of the start/end day
    dplyr::mutate(active_edge_day_duration = dplyr::if_else(
      !is.null(calc_start),
      # start day
      .data$edge_day_duration - lubridate::as.duration(.data$edge - lubridate::as_datetime(.data$edge_day)),
      # end day
      .data$edge_day_duration - lubridate::as.duration(lubridate::as_datetime(.data$edge_day) + lubridate::ddays(1) - .data$edge)
    )) %>%
    # calculate the fraction of the duration of the active part
    dplyr::mutate(daily_effort = .data$active_edge_day_duration / .data$edge_day_duration) %>%
    dplyr::pull(.data$daily_effort)
}

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

#' Predict radial distance
#'
#' Predict radial distance from camera given pixel positions.
#'
#' @param mod Site calibration model (`depcal` object), produced using
#'   `cal.site()`).
#' @param relx x Pixel position relative to the centre line.
#' @param rely y Pixel position relative to the top edge.
#' @return Vector numeric radii.
#' @noRd
#' @note Units depend on the units of pole height above ground used to calibrate
#'   the site model.
predict_r <- function(mod, rel_x, rel_y) {
  new_data <- data.frame(relx = rel_x, rely = rel_y)
  res <- stats::predict(mod, newdata = new_data)
  res[res < 0] <- Inf
  return(res)
}

#' Map legend title table
#'
#' Store legend titles for deployment visualizations: RAI, effort, number of
#' observations, etc.
#' Returns a data frame of all titles with the following columns:
#' - `feature`: Deployment feature to visualize.
#' - `legend_title`: Legend title.
#'
#' @noRd
#' @usage map_legend_title()
map_legend_title <- function() dplyr::as_tibble(mapdep_legend_titles)

mapdep_legend_titles <- structure(list(
  feature = c(
    "n_species",
    "n_obs",
    "n_individuals",
    "rai",
    "rai_individuals",
    "effort"
  ),
  legend_title = c(
    "Number of detected species",
    "Number of observations",
    "Number of individuals",
    "RAI",
    "RAI (individuals)",
    "Effort"
  )
))

#' Get legend title for deployment visualizations
#'
#' @param feature Character, one of:
#'   - `n_species`
#'   - `n_obs`
#'   - `rai`
#'   - `effort`
#' @noRd
get_legend_title <- function(feat) {
  # get all legend titles
  titles <- map_legend_title()
  # return the legend title we need
  titles %>%
    dplyr::filter(.data$feature == feat) %>%
    dplyr::pull(.data$legend_title)
}

#' Add unit to legend title
#'
#' This function is useful when a unit (e.g. temporal unit) should be added to
#' legend title.
#'
#' @param title A character with legend title.
#' @param unit Character with unit to add to `title`.
#' @param use_brackets Logical.
#'   If `TRUE` (default) `unit` is wrapped between brackets, e.g. `(days)`.
#' @noRd
#' @usage map_legend_title("My title", unit = "day", use_bracket = TRUE)
add_unit_to_legend_title <- function(title, unit = NULL, use_brackets = TRUE) {
  if (is.null(unit)) {
    title
  } else {
    if (use_brackets == TRUE) {
      unit <- paste0("(", unit, ")")
    }
    paste(title, unit)
  }
}

#' Create columns, but only if they don't exist yet!
#'
#' Using dplyr::mutate(), add a new column, but only if it's missing
#'
#' @inherit dplyr::mutate
#' @noRd
#' @examples 
#' \dontrun{
#' # doesn't add a column when it already exists
#' mutate_when_missing(cars, speed = "warp 9")
#' # but does add a column when it doesn't exist yet
#' mutate_when_missing(cars, space = "The final frontier")
#' }
mutate_when_missing <- function(.data,...){
  dots <- substitute(list(...))[-1]
  cols_to_check <- names(sapply(dots, deparse))
  columns_to_add <- cols_to_check[!cols_to_check %in% colnames(.data)]
  if(!rlang::is_empty(columns_to_add)){.data <- dplyr::mutate(.data,...)}
  return(.data)
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
  # Return a tibble dataframe with the deployment ID and the start/end date series
  dplyr::tibble(start = start_date_series,
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
    group_time_by = group_time_by) %>%
    # Add needed deployments columns
    dplyr::left_join(
      deployments %>%
        dplyr::select("deploymentID",
                      "deploymentStart",
                      "deploymentEnd",
                      dplyr::any_of(group_by)),
      by = "deploymentID")
}

#' Enrich observations information of a deployment with date series
#' 
#' This function enriches the information about the observations of a deployment
#' with date series based on its `deploymentStart`, `deploymentEnd` and the
#' given time grouping. This function is an extension of `enrich_deployment()`.
#' 
#' @param observations A data frame containing observation information of a
#'  given deployment.
#' @param group_by_deployments A character vector of deployment column names to
#'  add to the date series.
#' @param group_by_observations A character vector of observation column names
#'  to add to the date series.
#' @param col_obs_for_feature A character vector of observation columns to add
#'  to the date series.
#' @inheritParams enrich_deployment
#' @return A tibble data frame with start/end dates, the deployment ID, the
#'   columns in `group_by` and the observations columns. If `deployment_id` not
#'   in `observations`, `NULL` is returned.
#' @noRd
enrich_observations <- function(deployment_id,
                                deployments,
                                observations,
                                group_by_deployments,
                                group_by_observations,
                                group_time_by,
                                col_obs_for_feature) {
  if (deployment_id %in% observations$deploymentID) {
    observations %>%
      dplyr::select("deploymentID",
                    "eventStart",
                    dplyr::any_of(group_by_observations),
                    dplyr::any_of(col_obs_for_feature)) %>%
      dplyr::left_join(enrich_deployment(deployment_id = deployment_id,
                                         deployments = deployments,
                                         group_by = group_by_deployments,
                                         group_time_by = group_time_by),
                       by = dplyr::join_by(deploymentID,
                                           dplyr::between(
                                             x = x$eventStart,
                                             y$start,
                                             y$end
                                           )
                       )
      )
  } else {
    NULL
  }
}