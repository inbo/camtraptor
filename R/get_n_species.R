#' Get number of identified species for each deployment
#'
#' Function to get the number of identified species per deployment.
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
#' @importFrom dplyr .data %>% bind_rows count distinct filter group_by mutate
#'   pull select
#'
#' @export

#' @return a tibble (data.frame) with the following columns: - `deployment_id`
#'   deployment unique identifier - `n`: (integer) number of observed and
#'   identified species
#'
get_n_species <- function(datapkg) {

  # check input data package
  check_datapkg(datapkg)

  # extract observations and deployments
  observations <- datapkg$observations
  deployments <- datapkg$deployments

  # get deployments without observations
  deployments_no_obs <- get_dep_no_obs(datapkg)

  # get species detected by each deployment
  species <-
    observations %>%
    distinct(.data$deployment_id, .data$scientific_name)

  # get deployments with unidentified observations
  unidentified_obs <-
    species %>%
    filter(is.na(.data$scientific_name)) %>%
    pull(.data$deployment_id)

  # get amount of species detected by each deployment
  n_species <-
    species %>%
    group_by(.data$deployment_id) %>%
    count()

  # remove the count of NA as species and set n as integer
  n_species <- n_species %>%
    mutate(n = ifelse(.data$deployment_id %in% unidentified_obs,
                      as.integer(.data$n - 1),
                      as.integer(.data$n)
    ))

  # set up n = NA (number of species) for deployments without observations
  deployments_no_obs <-
    deployments_no_obs %>%
    select(.data$deployment_id) %>%
    mutate(n = NA_integer_)

  # add them to n_species and return
  n_species %>% bind_rows(deployments_no_obs)
}
