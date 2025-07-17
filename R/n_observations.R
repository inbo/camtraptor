#' Calculate the number of observations
#'
#' Temporal grouping is also possible. This function is used internally in
#' `summarize_observations()`.
#'
#' @param deployment_ids Character. The deployment IDs.
#' @param deployments Tibble data frame with deployments information, e.g. via
#'   `deployments(x)`.
#' @param observations Tibble data frame with observations information, e.g. via
#'   `observations(x)`.
#' @param group_by_deployments Character. Vector with the name of the columns of
#'   `deployments`.
#' @param group_by_observations Character. Vector with the name of the columns
#'   of `observations`.
#' @param group_time_by Character, one of `"day"`, `"week"`, `"month"`,
#'   `"year"`. The effort is calculated at the interval rate defined in
#'   `group_time_by`. Default: `NULL`, no grouping, i.e. the entire duration of
#'   the deployment is taken into account as a whole.
#' @noRd
n_observations <- function(deployment_ids,
                           deployments,
                           observations,
                           group_by_deployments = group_by_deployments,
                           group_by_observations = group_by_observations,
                           group_time_by) {
  # Effort per `deploymentID` and `group_time_by`. It contains also the
  # deployment columns in `group_by_deployments` and the observation columns in
  # `group_by_observations`
  n_obs_per_deploy <- deployment_ids %>%
    purrr::map(~ n_observations_per_deployment(
      deployment_id = .,
      deployments = deployments,
      observations = observations,
      group_by_deployments = group_by_deployments,
      group_by_observations = group_by_observations,
      group_time_by = group_time_by
    )
  )
  n_obs_per_deploy <- purrr::list_rbind(n_obs_per_deploy)
  
  # Calculate the number of observations over all deployments grouped by
  # `group_by_deployments`, `group_by_observations` and `group_time_by`
  n_obs_per_deploy %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_by_deployments,
                                    group_by_observations,
                                    group_time_by)))
    ) %>%
    dplyr::summarise(
      n_observations = sum(.data$n_observations, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}

#' Number of observations per deployment
#' 
#' This function calculates the number of observations for a given deployment.
#' It is used internally in `n_observations()`.
#' @noRd
n_observations_per_deployment <- function(
    deployment_id,
    deployments,
    observations,
    group_by_deployments = group_by_deployments,
    group_by_observations = group_by_observations,
    group_time_by) {
  
  observations <- observations %>%
    dplyr::filter(.data$deploymentID == deployment_id)
  if (nrow(observations) > 0) {
    n_obs_one_deploy_df <- enrich_observations(
      deployment_id = deployment_id,
      deployments = deployments,
      observations = observations,
      group_by = group_by_deployments,
      group_time_by = group_time_by
    ) %>%
    # Group by deploymentID and any additional grouping variables given in
    # `group_by_deployments` and `group_by_observations`
    dplyr::group_by(
      dplyr::across(c("start",
                      dplyr::all_of(group_by_deployments),
                      dplyr::all_of(group_by_observations)))
    ) %>%
    # Calculate number of observations (`observationID`) per group
    dplyr::summarise(
      n_observations = dplyr::n_distinct(.data$observationID)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(c(group_by_deployments,
                                  group_by_observations,
                                  "start",
                                  "n_observations")))
  } else {
    # Empty tibble if no observations for the deployment
    n_obs_one_deploy_df <- dplyr::tibble(
      !!!purrr::map(
        purrr::set_names(
          x = c(group_by_deployments, group_by_observations),
          nm = c(group_by_deployments, group_by_observations)
        ), ~ character(0)
      )) %>%
      dplyr::mutate(start = lubridate::as_datetime(character(0))) %>%
      dplyr::mutate(n_observations = integer(0))
  }
  if (!is.null(group_time_by)) {
    n_obs_one_deploy_df %>%
      dplyr::rename(!!group_time_by := start)
  } else {
    n_obs_one_deploy_df
  }
}
