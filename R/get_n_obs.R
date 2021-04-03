#' Get number of observations for each deployment
#'
#' Function to get the number of observations (of a subset of species) per
#' deployment.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#'   1. `observations` 2. `deployments` 3. `multimedia`
#'
#'   and a list with metadata: `datapackage`
#'
#' @param species a character with scientific name. If `NULL` (default) all
#'   observations of all species are taken into account
#'
#' @importFrom dplyr .data %>% bind_rows count group_by mutate select ungroup
#'
#' @export

#' @return a tibble (data.frame) with the following columns: - `deployment_id`
#'   deployment unique identifier - `n`: (integer) number of observations
#'
#' @examples
#' # get number of observations of all species (not identified individuals as well)
#' get_n_obs(camtrapdp)
#'
#' # get number of observations of Gallinula chloropus
#' get_n_obs(camtrapdp, species = "Gallinula chloropus")
#'
get_n_obs <- function(datapkg, species = NULL) {

  # check input data package
  check_datapkg(datapkg)

  # check species
  valid_species <- get_species(datapkg)
  check_value(tolower(species), tolower(valid_species), "species")

  # select observations with the selected species
  if (!is.null(species)) {
    datapkg$observations <-
      datapkg$observations %>%
      filter(tolower(.data$scientific_name) == tolower(species))
  }

  # extract observations and deployments
  observations <- datapkg$observations
  deployments <- datapkg$deployments

  # get deployments without observations
  deployments_no_obs <- get_dep_no_obs(datapkg)

  # get number of observations collected by each deployment
  n_obs <-
    observations %>%
    group_by(.data$deployment_id) %>%
    count() %>%
    ungroup()

  # set up number of observations to 0 for deployments without observations
  deployments_no_obs <-
    deployments_no_obs %>%
    select(.data$deployment_id) %>%
    mutate(n = 0)

  # add them to n and return df with column n as integer
  n_obs %>%
    bind_rows(deployments_no_obs) %>%
    mutate(n = as.integer(.data$n))
}
