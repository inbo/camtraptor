#' Add deployment coordinates to observations
#'
#' This function adds deployment coordinates to `observations` based on
#' `deploymentID`.
#'
#' When the `latitude` and `longitude` columns are already present in
#' `observations`, a warning is issued and the original object is returned
#' unchanged.
#'
#' @inheritParams summarize_deployments
#' @returns Camera trap data package object, where `observations` is updated by
#'   appending two new columns: `latitude` and `longitude`
#' @family transformation functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Add coordinates to observations
#' add_coordinates(x) %>% observations()
add_coordinates <- function(x) {
  
  # Check Camera Trap Data Package
  camtrapdp::check_camtrapdp(x)
  
  # If coordinates are already present, warn and return x
  if (all(c("latitude", "longitude") %in% colnames(observations(x)))) {
    warning("Coordinates already present in observations. Returning x.")
    return(x)
  }
  
  # Add coordinates to observations
  observations(x) <- observations(x) %>%
    dplyr::left_join(deployments(x) %>% 
                       dplyr::select("deploymentID", "latitude", "longitude"),
                     by = "deploymentID")
  
  return(x)
}
