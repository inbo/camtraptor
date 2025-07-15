#' Get custom effort
#'
#' Gets the effort for each deployment and a specific time interval such as day,
#' week, month or year. A custom time window can also be set up. This function
#' calls `camtrapR_cameraOperation()` internally.
#'
#' @param start Deprecated and not supported anymore. Use `filter_deployments()` to filter out deployments.
#' @param end Deprecated and not supported anymore.  Use `filter_deployments()` to filter out deployments.
#' @param group_by Character, one of `"day"`, `"week"`, `"month"`, `"year"`.
#'   Default: `NULL`. See `group_time_by` argument in
#'   `[summarize_deployments()]`.
#' @param unit Deprecated and not supported anymore.
#' @inheritParams n_species
#' @inherit summarize_deployments return
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' 
#' # Effort for each deployment over the entire duration of the project
#' get_custom_effort(x)
#'
#' # Effort at monthly interval
#' get_custom_effort(x, group_by = "month")
#'
#' # You can afterwards calculate the total effort over all deployments
#' library(dplyr)
#' get_custom_effort(x, group_by = "year", unit = "day") %>%
#'   dplyr::filter(effort > 0) %>%
#'   dplyr::group_by(begin) %>% 
#'   dplyr::summarise(
#'     deploymentIDs = list(deploymentID),
#'     locationNames = list(locationName),
#'     ndep = length(unique(deploymentID)),
#'     nloc = length(unique(locationName)),
#'     effort = sum(effort),
#'     unit = unique(unit)
#'   )
get_custom_effort <- function(x,
                              start = NULL,
                              end = NULL,
                              group_by = NULL,
                              unit = "hour") {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "get_custom_effort()",
    details = glue::glue(
      "Please use `summarize_deployments(x, group_by = \"deploymentID\", group_time_by = {group_by})` ",
      "instead. Notice also that the effort is only returned as a lubridate ",
      "duration object in column `effort_duration`. The columns `effort` and ",
      "`unit` are not returned anymore. If temporal grouping is applied, a column named as the allowed value is "
    )
  )
  if (!is.null(unit)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "get_custom_effort(unit)",
      details = glue::glue(
        "The effort is now only returned as a lubridate duration object in ",
        "column `effort_duration`. To suppress this warning, set ",
        "`unit = NULL`."
      )
    )
  }
  # Check start earlier than end
  if (!is.null(start) & !is.null(end)) {
    assertthat::assert_that(start <= end,
                            msg = "`start` must be earlier than `end`."
    )
  }
  
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  
  summarize_deployments(x,
                        group_by = "deploymentID",
                        group_time_by = group_by
  )
}
