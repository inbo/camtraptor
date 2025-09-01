#' Get effort
#'
#' `r lifecycle::badge("deprecated")`
#' 
#' Gets the effort (deployment duration) per deployment.
#' 
#' It is deprecated as of camtraptor 1.0.0. Please use [summarize_deployments()]
#' instead.
#'
#' @param unit `r lifecycle::badge("deprecated")` The unit used to quantify the
#'   effort. Ignored as the effort is returned only as a duration object.
#' @inheritParams summarize_deployments
#' @inheritParams get_n_obs
#' @inherit summarize_deployments return
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
