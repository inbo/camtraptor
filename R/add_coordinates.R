#' Add deployment coordinates to observations
#' 
#' This function adds deployment coordinates to `observations` based on
#' `deploymentID`.
#' 
#' @return Camera trap data package object, where `observations` is updated by appending two new columns: `latitude` and `longitude`.
#' @inheritParams n_species
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' 
#' # Add coordinates to observations
#' add_coordinates(x) %>% observations()
add_coordinates <- function(x) {
  
  # add coordinates to observations
  observations(x) <- observations(x) %>%
    dplyr::left_join(deployments(x) %>% 
                       dplyr::select("deploymentID", "latitude", "longitude"),
                     by = "deploymentID")
  
  return(x)
}
