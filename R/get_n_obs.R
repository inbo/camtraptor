#' Get number of observations for each deployment
#'
#' Gets the number of observations per deployment.
#' The number of observations is defined as the number of distinct sequences
#' (`sequenceID`).
#'
#' @inheritParams n_species
#' @return A tibble data frame with the following columns:
#' - `deploymentID`: Deployment unique identifier.
#' - `scientificName`: Scientific name of the species.
#' - `n`: Number of observations.
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' 
#' # Get number of individuals for each species
#' get_n_obs(x)
#'
#' # Use `filter_observations()` to filter on  scientific name
#' x %>%
#'   filter_observations(
#'     scientificName %in% c("Anas platyrhynchos", "Vulpes vulpes")
#'   ) %>%
#'   get_n_obs()
#'
#' # Use `filter_observations()` to filter on vernacular name
#' x %>%
#'   filter_observations(taxon.vernacularNames.eng == "mallard") %>%
#'   get_n_obs()
#'
#' # Get number of individuals per deployment
#' x %>%
#'   get_n_obs() %>%
#'   dplyr::group_by(.data$deploymentID) %>%
#'   dplyr::summarise(n = sum(.data$n)) %>%
#'   dplyr::ungroup()
get_n_obs <- function(x) {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  
  # Extract observations and deployments
  observations <- observations(x)
  deployments <- deployments(x)

  deploymentID <- purrr::pluck(deployments, "deploymentID")

  deployments_no_obs <- get_dep_no_obs(x)

  # Get number of observations collected by each deployment for each species
  n_obs <-
    observations %>%
    dplyr::group_by(.data$deploymentID, .data$scientificName) %>%
    dplyr::summarise(n = dplyr::n_distinct(.data$sequenceID)) %>%
    dplyr::ungroup()

  # Get all combinations deployments - scientific name
  combinations_dep_species <-
    expand.grid(
      deploymentID,
      unique(purrr::pluck(observations, "scientificName"))
    ) %>%
    dplyr::rename(deploymentID = "Var1", scientificName = "Var2") %>%
    dplyr::as_tibble()

  # Set 0 to combinations without observations (i.e. n = NA after join)
  n_obs <-
    combinations_dep_species %>%
    dplyr::left_join(n_obs,
      by = c("deploymentID", "scientificName")
    ) %>%
    dplyr::mutate(n = ifelse(is.na(.data$n), 0, .data$n)) %>%
    dplyr::mutate(n = as.integer(.data$n))

  if (is.null(species)) {
    # Sum all observations per deployment
    n_obs <-
      n_obs %>%
      dplyr::group_by(.data$deploymentID) %>%
      dplyr::summarise(n = sum(.data$n)) %>%
      dplyr::ungroup()
  }

  # Order result by deployments and follow same order as in deployments df
  deployments %>%
    dplyr::select("deploymentID") %>%
    dplyr::left_join(n_obs, by = "deploymentID", multiple = "all")
}
