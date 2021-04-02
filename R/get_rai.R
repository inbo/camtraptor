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
#' @param species a character with scientific name
#'
#' @importFrom dplyr .data %>% group_by left_join select summarise
#' @export

#' @return a tibble (data.frame) with the following columns: - `deployment_id`
#'   deployment unique identifier - `rai`: relative abundance index
#'
get_rai <- function(datapkg, species) {

  # check input data package
  check_datapkg(datapkg)


  # check species
  valid_species <- get_species(datapkg)
  check_value(species, valid_species, "species", null_allowed = FALSE)

  # get number of observations
  n_obs_df <- get_n_obs(datapkg, species = species)

  # extract deployments
  deployments <- datapkg$deployments

  # get deployment_duration
  dep_effort <- get_effort(datapkg)

  # calculate RAI
  n_obs_df %>%
    left_join(dep_effort,
              by = "deployment_id") %>%
    group_by(.data$deployment_id) %>%
    summarise(rai = n * 100 / (as.numeric(.data$effort)/24/60/60))

}
