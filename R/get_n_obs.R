#' Get number of observations for each deployment
#'
#' `r lifecycle::badge("deprecated")`
#' 
#' Gets the number of observations per deployment.
#' 
#' It is deprecated as of camtraptor 1.0.0. Please use [summarize_observations()]
#' instead.
#'
#' @inheritParams summarize_deployments
#' @return A tibble data frame with following columns:
#'   - `deploymentID`: deployment unique identifier.
#'   - `n_observations`: intger vector with the number of observations.
#'   - `sum_count`: integer vector with the sum of the count of individuals.
#'   - `rai`: numeric vector with the relative abundance index (RAI) based on
#'    number of observations.
#'   - `rai_individuals`: numeric vector with the RAI based on the count of
#'    individuals.
#' @export
get_n_obs <- function(x) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "get_effort()",
    details = glue::glue(
      "Please use `summarize_observations(x, group_by = \"deploymentID\")` ",
      "instead."
    )
  )
}
