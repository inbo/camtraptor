#' Get effort
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Use [summarize_deployments()] instead.
#' 
#' Gets the effort (deployment duration) per deployment.
#'
#' @param unit `r lifecycle::badge("deprecated")` The unit used to quantify the
#'   effort. Ignored as the effort is returned only as a duration object.
#' @inheritParams summarize_deployments
#' @inheritParams get_n_obs
#' @inherit summarize_deployments return
#' @family deprecated exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' get_effort(x)
get_effort <- function(x,
                       ...,
                       unit = "hour") {
  summarize_deployments_for_deprecated_functions(
    x,
    ...,
    unit = unit,
    function_name = deparse(sys.call()[[1]])
  )
}
