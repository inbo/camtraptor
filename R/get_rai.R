#' Get Relative Abundance Index (RAI)
#'
#' Function to get the RAI (Relative Abundance Index) per deployment.
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
#' @param species a character with scientific names or common names (case
#'   insensitive). If "all" (default), all scientific names are automatically
#'   selected
#' @param ... filter predicates for filtering on deployments
#'
#' @importFrom dplyr .data %>% group_by left_join select summarise ungroup
#'
#' @export

#' @return a tibble (data.frame) with the following columns:
#' - `deployment_id` deployment unique identifier
#' - `scientific_name` scientific name
#' - `rai`: relative abundance index
#'
#' @examples
#' # all species
#' get_rai(camtrapdp, species = "all")
#'
#' # selected species
#' get_rai(camtrapdp, species = c("Anas platyrhynchos", "Rattus norvegicus"))
#'
#' # with common names
#' get_rai(camtrapdp, species = c("Mallard", "norway rat"))
#'
#' # mixed scientific and vernacular names
#' get_rai(camtrapdp, species = c("Anas platyrhynchos", "norway rat"))
#'
#' # species argument is case insensitive
#' get_rai(camtrapdp, species = c("ANAS plAtyRhynChOS"))
#'
#' # apply filtering: deployments with latitude >= 51.28
#' get_rai(camtrapdp, species = "all", pred_gte("latitude", 51.28))
#'
get_rai <- function(datapkg, species, ...) {

  # check input data package
  check_datapkg(datapkg)

  # get all identified species if species arg is equal to "all"
  if ("all" %in% species) {
    species <- get_species(datapkg)$scientific_name
  }
  # check species
  species <- check_species(datapkg, species)

  # get number of observations
  n_obs_df <- get_n_obs(datapkg, species = species, ...)

  # extract deployments
  deployments <- datapkg$deployments

  # get deployment duration (effort) in seconds (standard duration in lubridate)
  dep_effort <- get_effort(datapkg, unit = NULL, ...)

  # calculate RAI
  n_obs_df %>%
    left_join(dep_effort,
              by = "deployment_id") %>%
    group_by(.data$deployment_id,
             .data$scientific_name) %>%
    summarise(rai = .data$n * 100 / (as.numeric(.data$effort)/24/60/60)) %>%
    ungroup()
}
