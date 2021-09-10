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
#' @importFrom dplyr %>% .data distinct left_join select starts_with tibble
#' @importFrom purrr map_dfr
#' @importFrom tidyr drop_na
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
  
  # get vernacular names and scientific names from datapackage (taxonomic
  # slot)
  if (!"taxonomic" %in% names(datapkg$datapackage)) return(NULL)
  map_dfr(
    datapkg$datapackage$taxonomic,
    function(x) x %>% as.data.frame()) %>% 
    tibble()
}
