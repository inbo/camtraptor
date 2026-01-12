#' Define internal vectors with possible `group_by` values, to be used in
#' `summarize_observations()`, `summarize_deployments()` and check functions.
.group_bys_deployments <- c(
  "deploymentID",
  "latitude",
  "longitude",
  "locationID",
  "locationName",
  "deploymentTags"
)
.group_bys_observations <- c(
  "scientificName",
  "lifeStage",
  "sex",
  "behavior"
)
.group_time_bys <- c("day", "week", "month", "year")

#' Features returned by `summarize_observations()`. Useful to check output of
#' the function and other check functions.
.features_observations <- c(
  "n_scientificName",
  "n_events",
  "n_observations",
  "sum_count",
  "rai_observations",
  "rai_count"
)
#' Features returned by `summarize_deployments()`. Useful to check output of
#' the function and other check functions.
.features_deployments <- c(
  "effort_duration"
)

#' Prefix table for hover info over deployments
#'
#' Stores prefixes for info shown in leaflet map while hovering over a
#' deployment with the mouse.
#'
#' Returns a data.frame of all prefixes with the following columns:
#' - `info`: all valid grouping columns (deployments or observations) and features.
#' - `prefix`: Prefix to use.
.prefixes_for_hover_info <- dplyr::tibble(
  info = c(.group_bys_deployments,
           .group_bys_observations,
           .features_deployments,
           .features_observations
  ),
  prefix = snakecase::to_sentence_case(info) %>%
    stringr::str_c(": ") %>%
    # Add `"s"` to `"N scientific name"`
    stringr::str_replace(pattern = "N scientific name", replacement = "N scientific names") %>%
    # Replace `"count"` with `individual counts"`
    stringr::str_replace(pattern = "count", replacement = "individual counts")
)
