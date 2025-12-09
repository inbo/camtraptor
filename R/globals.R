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
