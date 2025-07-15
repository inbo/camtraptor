#' Get effort
#'
#' Gets the effort (deployment duration) per deployment.
#'
#' @param unit Deprecated and not supported anymore. 
#' @inheritParams n_species
#' @return A tibble data frame with following columns:
#'   - `deploymentID`: Deployment unique identifier.
#'   - `effort_duration`: A duration object (duration is a class from lubridate
#'   package).
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' get_effort(x)
get_effort <- function(x,
                       unit = "hour") {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "get_effort()",
    details = glue::glue(
      "Please use `summarize_deployments(x, group_by = \"deploymentID\")` ",
      "instead. Notice also that the effort is only returned as a lubridate ",
      "duration object in column `effort_duration`. The columns `effort` and ",
      "`unit` are not returned anymore."
    )
  )
  if (!is.null(unit)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "get_effort(unit)",
      details = glue::glue(
        "The effort is now only returned as a lubridate duration object in ",
        "column `effort_duration`. To suppress this warning, set ",
        "`unit = NULL`."
      )
    )
  }
  
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)

  summarize_deployments(x, group_by = "deploymentID")
}
