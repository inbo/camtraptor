#' Extend observations summary with the groups left out by summarize functions
#'
#' The function `summarize_observations()` doesn't include all possible groups.
#' This is also the normal behavior of dplyr's `summarize()` function. However,
#' for exploration and visualization purposes, it is sometimes useful to know
#' which groups are missing. This function extends the summary with the missing
#' groups. More details are provided in the details section.
#'
#' @details
#' The function works by getting all possible combinations of the grouping
#' columns of the summary from `observations` and all existent
#' combinations in `deployments`.
#' Then, it performs a full join between the provided summary and the complete
#' set of groups. This ensures that all possible groups are represented in the
#' final output.
#' 
#' @param summary A grouped tibble data frame as returned by
#'   `summarize_observations()`.
#' @param x Camera Trap Data Package object.
#' @return A grouped tibble data frame with the missing groups added. The values
#'   of the features are set to `0` or `NA`, depending on the feature. See the
#'   details section for more information.
#' @noRd
extend_summary <- function(summary, x) {
  
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  # Check `summary` is a valid summary
  check_summary(summary)
  
  # Get observations
  obs <- observations(x)
  
  # Get grouping columns from the summary
  grouping_cols <- dplyr::group_vars(summary)
  
  # Grouping cols in observations
  grouping_cols_obs <- grouping_cols[
    grouping_cols %in% .group_bys_observations
  ]
  
  # Grouping cols in deployments if any
  grouping_cols_dep <- grouping_cols[
    grouping_cols %in% .group_bys_deployments
  ]
  
  # Time grouping column if any
  time_group_col <- grouping_cols[
    grouping_cols %in% .group_time_bys
  ]
  if (length(time_group_col) == 0) {
    # Set time_group_col to NULL if not present: it makes easier to handle
    # later: one if-else statement less
    time_group_col <- NULL
  }
  
  # Get all deployment/time groups by running `summarize_deployments()` with
  # the same temporal grouping if any
  if (length(grouping_cols_dep) > 0) {
    dep_time_groups <- summarize_deployments(
      x,
      group_by = grouping_cols_dep,
      group_time_by = time_group_col
    )
  } else {
    dep_time_groups <- summarize_deployments(
      x,
      group_by = "deploymentID", # "dummy" grouping to get time groups
      group_time_by = time_group_col
    )
  }
  dep_time_groups <- dep_time_groups %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(c(grouping_cols_dep, time_group_col))) %>%
    dplyr::distinct()
  
  # Get all possible groups in observations
  all_groups_obs <- obs %>%
    # Select only the grouping columns present in observations (using `any_of()`
    # to avoid error when summary is also grouped temporally
    dplyr::select(dplyr::any_of(grouping_cols_obs)) %>%
    dplyr::distinct() %>%
    # Get all unique values per grouping column. This step is needed because
    # `expand.grid()` considers multiple NAs in a column as separate entities
    # when creating combinations, leading to duplicate rows in the output.
    purrr::map(unique) %>%
    # Create all combinations of grouping columns.
    expand.grid() %>%
    dplyr::as_tibble()
  
  # Extend the deployments/time groups with the observation groups
  all_groups <- tidyr::expand_grid(dep_time_groups, all_groups_obs)
  
  # Extend summary with all possible groups
  extended_summary <- summary %>%
    dplyr::full_join(all_groups, by = grouping_cols) %>%
    # Preserve the order based on grouping columns
    dplyr::arrange(dplyr::across(dplyr::all_of(grouping_cols)))
  
  # NAs must be replaced by `0` except for `n_scientficName`
  features_zero <- .features_observations[
    .features_observations != "n_scientificName"
  ]
  extended_summary <- extended_summary %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(features_zero),
        ~ tidyr::replace_na(., 0)
      )
    )
  
  return(extended_summary)
}

#' Summarize observations information
#'
#' Summarizes event-based observations by calculating:
#' - Number of scientific names.
#' - Number of events.
#' - Number of observations.
#' - Sum of individual counts.
#' - Relative Abundance Index (RAI) based on number of observations.
#' - Relative Abundance Index (RAI) based on individual counts.
#'
#' `summarize_observations()` and `summarise_observations()` are synonyms.
#'
#' @param group_by Character vector with names of columns in deployments and
#'   observations. At the moment you can choose one or many columns among:
#'   `c("deploymentID", "latitude", "longitude", "locationID", "locationName",
#'   "deploymentTags", "scientificName", "lifeStage", "sex", "behavior")`.
#'   Default: `c("deploymentID", "latitude", "longitude", "scientificName")`.
#' @param group_time_by Character, one of `"day"`, `"week"`, `"month"`,
#'   `"year"`. The features are calculated at the interval rate defined in
#'   `group_time_by`. Default: `NULL`, no grouping, i.e. the entire duration of
#'   the deployment is taken into account as a whole.
#' @inheritParams summarize_deployments
#' @return A grouped tibble data frame with the following columns:
#'   - `group_by` names, e.g. `deploymentID`, `latitude`, `longitude`, and `scientificName`.
#'   - `group_time_by` name if provided, e.g. `month`. It is a datetime column
#'   containing the first date of the time interval, e.g. the first day of the
#'   month.
#'   - `n_scientificName`: integer vector with the number of scientific names.
#'   If `scientificName` is in `group_by`, `n_scientificName` is equal to 1 or
#'   0, if `scientificName = NA` (unidentified animals).
#'   - `n_events`: integer vector with the number of events.
#'   - `n_observations`: integer vector with the number of observations.
#'   - `sum_count`: integer vector with the sum of individual counts.
#'   - `rai_observations`: numeric vector with the Relative Abundance Index
#'   (RAI), defined as `100 * (n_observations/effort)` where `n_observations` is
#'   the number of observations and `effort` is the `effort_duration` as
#'   returned by `summarize_deployments()` expressed in days.
#'   - `rai_count`: numeric vector with the Relative Abundance Index (RAI),
#'   defined as `100 * (sum_count/effort)` where `sum_count` is the sum of
#'   individual counts and `effort` is the `effort_duration` as returned by
#'   `summarize_deployments()` expressed in days.
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' # Summarize observations by `deploymentID`, `latitude`, `longitude` and
#' # `scientificName` (default)
#' summarize_observations(x)
#'
#' # Summarize observations by `deploymentID`, and month
#' summarize_observations(x, group_by = "deploymentID", group_time_by = "month")
#'
#' # Summarize observations by `locationId`, and `locationName`
#' summarize_observations(x, group_by = "locationName")
summarize_observations <- function(
    x,
    group_by = c("deploymentID", "latitude", "longitude", "scientificName"),
    group_time_by = NULL) {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)

  # Check `group_by`
  group_bys <- c(.group_bys_deployments, .group_bys_observations)
  check_value(group_by, group_bys, "group_by", null_allowed = FALSE)
  group_by_deployments <- group_by[group_by %in% .group_bys_deployments]
  group_by_observations <- group_by[group_by %in% .group_bys_observations]
  # Check `group_time_by`
  check_group_time_by(group_time_by, .group_time_bys)
  # Use event-based observations only
  x <- x %>%
    filter_observations(.data$observationLevel == "event")
  # Extract observations and deployments
  observations <- observations(x)
  deployments <- deployments(x)
  deployment_ids <- purrr::pluck(deployments, "deploymentID")
  # Define the formula for the number of scientific names per deployment
  formula_n_species_per_dep <- rlang::expr(
    n_scientificName := dplyr::n_distinct(.data$scientificName, na.rm = TRUE)
  )
  # Define the formula for the total number of scientific names
  formula_n_species <- rlang::expr(
    n_scientificName := sum(.data$n_scientificName, na.rm = TRUE)
  )
  # Calculate n_scientificName
  n_species_df <- calc_obs_feature(
    deployment_ids = deployment_ids,
    deployments = deployments,
    observations = observations,
    group_by_deployments = group_by_deployments,
    group_by_observations = group_by_observations,
    group_time_by = group_time_by,
    col_obs_for_feature = "scientificName",
    formula_per_deployment = formula_n_species_per_dep,
    formula_total = formula_n_species
  )
  # Define the formula for the number of events per deployment
  formula_n_events_per_dep <- rlang::expr(
    n_events := dplyr::n_distinct(.data$eventID, na.rm = TRUE)
  )
  # Define the formula for the total number of events
  formula_n_events <- rlang::expr(
    n_events := sum(.data$n_events, na.rm = TRUE)
  )
  # Calculate n_events
  n_events_df <- calc_obs_feature(
    deployment_ids = deployment_ids,
    deployments = deployments,
    observations = observations,
    group_by_deployments = group_by_deployments,
    group_by_observations = group_by_observations,
    group_time_by = group_time_by,
    col_obs_for_feature = "eventID",
    formula_per_deployment = formula_n_events_per_dep,
    formula_total = formula_n_events
  )
  # Define the formula for the number of observations per deployment
  formula_n_obs_per_dep <- rlang::expr(
    n_observations := dplyr::n_distinct(.data$observationID)
  )
  # Define the formula for the total number of observations
  formula_n_obs <- rlang::expr(
    n_observations := sum(.data$n_observations, na.rm = TRUE)
  )
  # Calculate n_observations
  n_obs_df <- calc_obs_feature(
    deployment_ids = deployment_ids,
    deployments = deployments,
    observations = observations,
    group_by_deployments = group_by_deployments,
    group_by_observations = group_by_observations,
    group_time_by = group_time_by,
    col_obs_for_feature = "observationID",
    formula_per_deployment = formula_n_obs_per_dep,
    formula_total = formula_n_obs
  )

  # Define the formula for the sum of individual counts per deployment
  formula_sum_count_per_dep <- rlang::expr(
    sum_count := as.integer(sum(.data$count, na.rm = TRUE))
  )
  # Calculate sum_count (sum of individual counts)
  formula_sum_count <- rlang::expr(
    sum_count := sum(.data$sum_count, na.rm = TRUE)
  )
  sum_count_df <- calc_obs_feature(
    deployment_ids = deployment_ids,
    deployments = deployments,
    observations = observations,
    group_by_deployments = group_by_deployments,
    group_by_observations = group_by_observations,
    group_time_by = group_time_by,
    col_obs_for_feature = "count",
    formula_per_deployment = formula_sum_count_per_dep,
    formula_total = formula_sum_count
  )
  # Join the features
  summary <- purrr::reduce(
    list(n_species_df, n_events_df, n_obs_df, sum_count_df),
    dplyr::left_join,
    by = c(group_by_deployments, group_by_observations, group_time_by)
  )

  # Add RAI based on number of observations () and RAI based on the sum of
  # individual counts. To do so, we need to calculate the effort. If `group_by`
  # doesn't contain deployment columns, the effort cannot be calculated:
  # RAI cannot be calculated either. Message returned and NA values are
  # returned.
  if (length(group_by_deployments) > 0) {
    effort_df <- summarize_deployments(
      x,
      group_by = group_by_deployments,
      group_time_by = group_time_by
    )
    summary %>%
      dplyr::left_join(effort_df,
        by = c(group_by_deployments, group_time_by)
      ) %>%
      dplyr::mutate(
        rai_observations =
          100 * .data$n_observations /
            # Duration in days
            (.data$effort_duration / lubridate::ddays(1))
      ) %>%
      dplyr::mutate(
        rai_count =
          100 * .data$sum_count /
            # Duration in days
            (.data$effort_duration / lubridate::ddays(1))
      ) %>%
      dplyr::select(-"effort_duration")
  } else {
    summary %>%
      dplyr::mutate(
        rai_observations = NA_real_,
        rai_count = NA_real_
      )
  }
}
#' @rdname summarize_observations
#' @export
summarise_observations <- summarize_observations
