#' Filter out timelapse observations
#'
#' Subsets observations in a Camera Trap Data Package object, removing timelapse
#' observations, i.e. observations where `captureMethod` = `timeLapse`. This
#' function is a shortcut for `filter_observations(x, captureMethod !=
#' "timelapse")`.
#' 
#' @inheritParams get_species
#'
#' @return `x` filtered.
#' @family filter functions
#' @export
#'
#' @examples
#' x <- example_dataset()
#' 
#' # `x` doesn't contain timelapse observations, returned as is
#' filter_out_timelapse(x)
#' 
#' # Create a data package with timelapse observations
#' obs <- observations(x)
#' obs$captureMethod <- rep("timelapse", nrow(obs) - 1)
#' observations(x) <- obs
#' # Filter out timelapse observations
#' filter_out_timelapse(x)
filter_out_timelapse <- function(x) {
  if ("captureMethod" %in% names(observations(x))) {
    x %>%
      filter_observations(captureMethod != "timelapse")
  } else {
    x
  }
}
