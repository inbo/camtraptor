#' Get species
#'
#' Function to get all identified species
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#' 1. `observations`
#' 2. `deployments`
#' 3. `multimedia`
#'
#' and a list with metadata: `datapackage`
#'
#'
#' @export

#' @return a vector with the scientific names of all identified species
#'
#' @examples
#' get_species(camtrapdp)
#'
get_species <- function(datapkg) {

  # check input data package
  check_datapkg(datapkg)

  # extract observations and deployments
  observations <- datapkg$observations

  # extract species
  species <- unique(observations$scientific_name)
  # remove possible NA from vector
  species[!is.na(species)]

}
