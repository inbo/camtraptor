#' Get custom effort
#'
#' `r lifecycle::badge("deprecated")`
#' 
#' Gets the effort for each deployment and a specific time interval such as day,
#' week, month or year. 
#' 
#' It is deprecated as of camtraptor 1.0.0. Please use [summarize_deployments()]
#' instead.
#'
#' @param start `r lifecycle::badge("deprecated")` if not `NULL `. Not supported
#'   anymore. Use `filter_deployments()` to filter out deployments.
#' @param end `r lifecycle::badge("deprecated")` if not `NULL `. Not supported
#'   anymore. Use `filter_deployments()` to filter out deployments.
#' @param group_by Character, one of `"day"`, `"week"`, `"month"`, `"year"`.
#'   Default: `NULL`. See `group_time_by` argument in
#'   `[summarize_deployments()]`.
#' @inheritParams get_effort
#' @inherit summarize_deployments return
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
                              ...,
                              start = NULL,
                              end = NULL,
                              group_by = NULL,
                              unit = "hour") {
  summarize_deployments_for_deprecated_functions(
    x,
    ...,
    start = start,
    end = end,
    group_by = group_by,
    unit = unit,
    function_name = deparse(sys.call()[[1]])
  )
}
