#' Get number of individuals for each deployment
#'
#' Gets the number of individuals per deployment.
#' The number of observed individuals is stored in column `count` of
#' `observations`.
#'
#' @inheritParams n_species
#' @return A tibble data frame with the following columns:
#' - `deploymentID`: Deployment unique identifier.
#' - `scientificName`: Scientific name of the species. `NA` for not identified individuals.
#' - `n`: Number of individuals.
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' 
#' # Get number of individuals for each species
#' get_n_individuals(x)
#'
#' # Use `filter_observations()` to filter on  scientific name
#' x %>%
#'   filter_observations(
#'     scientificName %in% c("Anas platyrhynchos", "Vulpes vulpes")
#'   ) %>%
#'   get_n_individuals()
#'
#' # Use `filter_observations()` to filter on vernacular name
#' x %>%
#'   filter_observations(taxon.vernacularNames.eng == "mallard") %>%
#'   get_n_individuals()
#'
#' # Get number of individuals per deployment
#' x %>%
#'   get_n_individuals() %>%
#'   dplyr::group_by(.data$deploymentID) %>%
#'   dplyr::summarise(n = sum(.data$n)) %>%
#'   dplyr::ungroup()
get_n_individuals <- function(x) {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  
  # Extract observations and deployments
  observations <- observations(x)
  deployments <- deployments(x)

  deploymentID <- purrr::pluck(deployments, "deploymentID")

  deployments_no_obs <- get_dep_no_obs(x)

  # Get number of individuals collected by each deployment for each species
  n_individuals <-
    observations %>%
    dplyr::group_by(
      .data$deploymentID,
      .data$scientificName
    ) %>%
    dplyr::summarise(n = sum(.data$count)) %>%
    dplyr::ungroup()

  # Get all combinations deployments - scientific name
  combinations_dep_species <-
    expand.grid(
      deploymentID,
      unique(purrr::pluck(observations, "scientificName"))
    ) %>%
    dplyr::rename(
      deploymentID = "Var1",
      scientificName = "Var2"
    ) %>%
    dplyr::as_tibble()

  # set 0 to combinations without observed individuals (i.e. n = NA after join)
  n_individuals <-
    combinations_dep_species %>%
    dplyr::left_join(n_individuals,
      by = c("deploymentID", "scientificName")
    ) %>%
    dplyr::mutate(n = ifelse(is.na(.data$n), 0, .data$n)) %>%
    dplyr::mutate(n = as.integer(.data$n))

  # order result by deployments and follow same order as in deployments df
  deployments %>%
    dplyr::select("deploymentID") %>%
    dplyr::left_join(n_individuals, by = "deploymentID", multiple = "all")
}
