#' Summarize observations information
#'
#' Summarizes event-based observations by calculating:
#' - Number of observations.
#' - Sum of individual counts.
#' - Number of species.
#' - Relative Abundance Index (RAI) based on number of observations.
#' - Relative Abundance Index (RAI) based on individual counts.
#'
#' `summarize_observations()` and `summarise_observations()` are synonyms.
#'
#' @param group_by Character vector with names of columns in deployments and
#'   observations. At the moment you can choose one or many columns among:
#'   `c("deploymentID", "locationID", "locationName", "deploymentTags",
#'   "scientificName", "lifeStage", "sex", "behavior")`. Default:
#'   `c("deploymentID", "scientificName")`.
#' @param group_time_by Character, one of `"day"`, `"week"`, `"month"`,
#'   `"year"`. The features are calculated at the interval rate defined in
#'   `group_time_by`. Default: `NULL`, no grouping, i.e. the entire duration of
#'   the deployment is taken into account as a whole.
#' @inheritParams summarize_deployments
#' @return A tibble data frame with the following columns:
#'   - `group_by` names, e.g. `deploymentID` and `locationName`.
#'   - `group_time_by` name if provided, e.g. `month`. It contains the first
#'   date of the time interval, e.g. the first day of the month.
#'   - `n_observations`: Number of observations.
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' # Summarize observations by `deploymentID` (default)
#' summarize_observations(x)
#'
#' # Summarize observations by `deploymentID` and month
#' summarize_observations(x, group_time_by = "month")
#'
#' # Summarize observations by `locationId`, and `locationName`
#' #' summarize_observations(x, group_by = "locationName")
summarize_observations <- function(x,
                                   group_by = c("deploymentID"),
                                   group_time_by = NULL) {
   # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  
  # Check `group_by`
  # Some `group_by` values are deployment related, others obs related
  group_bys_deployments <- c("deploymentID",
                             "locationID",
                             "locationName",
                             "deploymentTags"
  )
  group_bys_observations <- c("scientificName",
                              "lifeStage",
                              "sex",
                              "behavior"
  )
  group_bys <- c(group_bys_deployments, group_bys_observations)
  check_value(group_by, group_bys, "group_by", null_allowed = FALSE)
  
  group_by_deployments <- group_by[group_by %in% group_bys_deployments]
  group_by_observations <- group_by[group_by %in% group_bys_observations]
  
  # Check `group_time_by`
  group_time_bys <- c( "day", "week", "month", "year")
  check_group_time_by(group_time_by, group_time_bys)
  
  # Use event-based observations only
  x <- x %>%
    filter_observations(.data$observationLevel == "event")
  
  # Extract observations and deployments
  observations <- observations(x)
  deployments <- deployments(x)
  deployment_ids <- purrr::pluck(deployments, "deploymentID")
  
  # Create summary
  n_observations(deployment_ids = deployment_ids,
                 deployments = deployments,
                 observations = observations,
                 group_by_deployments = group_by_deployments,
                 group_by_observations = group_by_observations,
                 group_time_by = group_time_by)

}
#' @rdname summarize_deployments
#' @export
summarise_observations <- summarize_observations
