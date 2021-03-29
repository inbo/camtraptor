#' Get number of observations for each deployment
#'
#' Function to get the number of observations per deployment.
#'
#' @param deployments a tibble (data.frame) containing deployments
#' @param observations a tibble (data.frame) containing observations
#'
#' @importFrom dplyr .data %>% bind_rows count group_by mutate select
#'
#' @export

#' @return a tibble (data.frame) with the following columns: - `deployment_id`
#'   deployment unique identifier - `n`: (integer) number of observations
#'
get_n_obs <- function(deployments, observations) {

  # get deployments without observations
  deployments_no_obs <- get_dep_no_obs(deployments, observations)

  # get number of observations collected by each deployment
  n_obs <-
    observations %>%
    group_by(.data$deployment_id) %>%
    count()

  # set up number of observations to 0 for deployments without observations
  deployments_no_obs <-
    deployments_no_obs %>%
    select(.data$deployment_id) %>%
    mutate(n = 0)

  # add them to n_species and return df with column n as integer
  n_obs %>%
    bind_rows(deployments_no_obs) %>%
    mutate(n = as.integer(n))
}
