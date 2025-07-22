#' Calculate the observations related feature
#'
#' Calculates the desired feature. This function uses the passed formulas;
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
#' @param col_obs_for_feature Character. The column in `observations` to use for
#'  the feature calculation.
#' @param formula_per_deployment A formula to calculate the feature per
#'   deployment.
#' @param formula_total A formula to calculate the feature over all deployments.
#' @return A tibble data frame.
#' @noRd
calc_obs_feature <- function(deployment_ids,
                             deployments,
                             observations,
                             group_by_deployments,
                             group_by_observations,
                             group_time_by,
                             col_obs_for_feature,
                             formula_per_deployment,
                             formula_total) {
  # Check that `col_obs_for_feature` is of length 1
  assertthat::assert_that(
    length(col_obs_for_feature) == 1,
    msg = "col_obs_for_feature must have length 1"
  )
  # Check that `col_obs_for_feature` is a valid column in `observations`
  check_value(
    col_obs_for_feature,
    colnames(observations),
    "col_obs_for_feature",
    null_allowed = FALSE
  )
  # Number of observations per `deploymentID` and `group_time_by`. It contains
  # also the deployment columns in `group_by_deployments` and the observation
  # columns in `group_by_observations`
  feat_per_deploy <- deployment_ids %>%
    purrr::map(~ calc_obs_feature_per_deployment(
      deployment_id = .,
      deployments = deployments,
      observations = observations,
      group_by_deployments = group_by_deployments,
      group_by_observations = group_by_observations,
      group_time_by = group_time_by,
      col_obs_for_feature = col_obs_for_feature,
      formula = formula_per_deployment
    ))
  feat_per_deploy <- purrr::list_rbind(feat_per_deploy)
  
  # Calculate the number of observations over all deployments grouped by
  # `group_by_deployments`, `group_by_observations` and `group_time_by`
  feat_per_deploy %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_by_deployments,
                                    group_by_observations,
                                    group_time_by)))
    ) %>%
    dplyr::summarise(
      !!rlang::f_lhs(formula_total) := !!rlang::f_rhs(formula_total)
    ) %>%
    dplyr::ungroup()
}

#' Calculate the observations related feature per deployment
#' 
#' Calculates the desired feature for a given deployment.
#' It is used internally in `calc_obs_feature()`.
#' @noRd
calc_obs_feature_per_deployment <- function(deployment_id,
                                            deployments,
                                            observations,
                                            group_by_deployments,
                                            group_by_observations,
                                            group_time_by,
                                            col_obs_for_feature,
                                            formula) {
  feature_col_name <- rlang::f_lhs(formula)
  feature_calc <- rlang::f_rhs(formula)
  observations <- observations %>%
    dplyr::filter(.data$deploymentID == deployment_id)
  if (nrow(observations) > 0) {
    feat_one_deploy_df <- enrich_observations(
      deployment_id = deployment_id,
      deployments = deployments,
      observations = observations,
      group_by_deployments = group_by_deployments,
      group_by_observations = group_by_observations,
      group_time_by = group_time_by,
      col_obs_for_feature = col_obs_for_feature
    ) %>%
      # Group by deploymentID and any additional grouping variables given in
      # `group_by_deployments` and `group_by_observations`
      dplyr::group_by(
        dplyr::across(c("start",
                        dplyr::all_of(group_by_deployments),
                        dplyr::all_of(group_by_observations)))
      ) %>%
      # Calculate number of observations (`observationID`) per group. Use function
      # in `func` in `dplyr::summarise()` with arg `col_to_use` to specify the
      # column. The new column is named based on `col_to_create` value.
      dplyr::summarise(!!feature_col_name := !!feature_calc) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::any_of(c(group_by_deployments,
                                    group_by_observations,
                                    "start",
                                    rlang::as_string(feature_col_name)))
      )
  } else {
    # Empty tibble if feature is not present in the deployment, e.g. no
    # observations
    feat_one_deploy_df <- dplyr::tibble(
      !!!purrr::map(
        purrr::set_names(
          x = c(group_by_deployments, group_by_observations),
          nm = c(group_by_deployments, group_by_observations)
        ), ~ character(0)
      )) %>%
      dplyr::mutate(start = lubridate::as_datetime(character(0))) %>%
      dplyr::mutate(!!feature_col_name := integer(0))
  }
  if (!is.null(group_time_by)) {
    feat_one_deploy_df %>%
      dplyr::rename(!!group_time_by := start)
  } else {
    feat_one_deploy_df
  }
}
