#' Get number of observations for each deployment
#'
#' Function to get the number of observations per deployment.
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
#' @importFrom dplyr .data %>% bind_rows count group_by mutate select
#' @importFrom dplyr .data %>% bind_rows count group_by mutate select ungroup
#'
#' @export

#' @return a tibble (data.frame) with the following columns: - `deployment_id`
#'   deployment unique identifier - `n`: (integer) number of observations
#'
get_n_obs <- function(datapkg) {

  # check input data package
  check_datapkg(datapkg)

  # extract observations and deployments
  observations <- datapkg$observations
  deployments <- datapkg$deployments

  # get deployments without observations
  deployments_no_obs <- get_dep_no_obs(datapkg)

  # get number of observations collected by each deployment
  n_obs <-
    observations %>%
    group_by(.data$deployment_id) %>%
    count()
    count() %>%
    ungroup()

  # set up number of observations to 0 for deployments without observations
  deployments_no_obs <-
    deployments_no_obs %>%
    select(.data$deployment_id) %>%
    mutate(n = 0)

  # add them to n_species and return df with column n as integer
  n_obs %>%
    bind_rows(deployments_no_obs) %>%
    mutate(n = as.integer(.data$n))
}
