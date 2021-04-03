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

#' @return a data.frame with all scientific names and vernacular names of
#'   identified species
#'
#' @examples
#' get_species(camtrapdp)
#'
get_species <- function(datapkg) {

  # check input data package
  check_datapkg(datapkg)

  # extract observations and deployments
  observations <- datapkg$observations

  # extract scientific name and vernacular names
  species <-
    observations %>%
    distinct(.data$scientific_name, .data$vernacular_name)

  # remove possible NA from vector
  species %>%
    filter(!is.na(.data$scientific_name),
           !is.na(.data$vernacular_name))

}
