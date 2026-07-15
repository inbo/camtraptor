# Define global variables to avoid R CMD check notes `no visible binding for
# global variable`. Notice that we try to avoid using `utils::globalVariables()`
# where possible, but in some cases it is necessary, e.g. when using
# `dplyr::join_by()`. Reason: `join_by()` uses a domain-specific language where
# column names are treated as symbols rather than evaluated through data-masking
# via `.data$`. For packages the only solution at the moment is to declare those
# variables with `utils::globalVariables()`.
utils::globalVariables(c("deploymentID", "x", "y"))


#' Define internal vectors with possible `group_by` values, to be used in
#' `summarize_observations()`, `summarize_deployments()` and check functions.
#' @keywords internal
.group_bys_deployments <- c(
  "deploymentID",
  "latitude",
  "longitude",
  "locationID",
  "locationName",
  "deploymentStart",
  "deploymentEnd",
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
#' @keywords internal
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
#' @keywords internal
.features_deployments <- c(
  "effort_duration"
)

#' Prefix table for hover info over deployments
#'
#' Stores prefixes for info shown in leaflet map while hovering over a
#' deployment with the mouse.
#'
#' A data.frame of all prefixes with the following columns:
#' - `info`: all valid grouping columns (deployments or observations) 
#' and features.
#' - `prefix`: Prefix to use.
#' @keywords internal
#' @importFrom snakecase to_sentence_case
.prefixes_for_hover_info <- dplyr::tibble(
  info = c(.group_bys_deployments,
           .group_bys_observations,
           .features_deployments,
           .features_observations,
           .group_time_bys
  ),
  prefix = snakecase::to_sentence_case(info) %>%
    stringr::str_c(": ") %>%
    # Add `"s"` to `"N scientific name"`
    stringr::str_replace(
      pattern = "N scientific name",
      replacement = "N scientific names"
    ) %>%
    # Replace `"count"` with `individual counts"`
    stringr::str_replace(pattern = "count", replacement = "individual counts")
)

#' Map legend title table
#'
#' Store legend titles for deployment visualizations: RAI, effort, number of
#' observations, etc.
#' A data frame of all titles with the following columns:
#' - `feature`: Deployment feature to visualize.
#' - `legend_title`: Legend title.
#' @keywords internal
.map_summary_legend_titles <- dplyr::tibble(
  feature = c(
    "n_scientificName",
    "n_events",
    "n_observations",
    "sum_count",
    "rai_observations",
    "rai_count",
    "effort_duration"
  ),
  legend_title = c(
    "Number of detected species",
    "Number of events",
    "Number of observations",
    "Sum of individual counts",
    "RAI",
    "RAI (individual counts)",
    "Effort"
  )
)
