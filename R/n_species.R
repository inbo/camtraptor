#' Get number of identified species for each deployment
#'
#' Gets the number of identified species per deployment. A species is identified
#' if it has a scientific name (`scientificName` != `NA`). If a deployment has
#' only unidentified species, the number of identified species is set to 0. If a
#' deployment has no observations, the number of identified species is set to
#' `NA`.
#'
#' @inheritParams get_species
#' @return A tibble data frame with the following columns:
#'   - `deploymentID`: Deployment unique identifier.
#'   - `n`: Number of observed and identified species.
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Get number of species
#' n_species(x)
n_species <- function(x) {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  
  # Extract observations and deployments
  observations <- observations(x)
  deployments <- deployments(x)

  # Get deployments without observations
  deployments_no_obs <- get_dep_no_obs(x)

  # Get species detected by each deployment
  species <-
    observations %>%
    dplyr::filter(.data$deploymentID %in% deployments$deploymentID) %>%
    dplyr::distinct(.data$deploymentID, .data$scientificName)

  # Get deployments with unidentified observations
  unidentified_obs <-
    species %>%
    dplyr::filter(is.na(.data$scientificName)) %>%
    dplyr::pull(.data$deploymentID)

  # Get amount of species detected by each deployment
  n_species <-
    species %>%
    dplyr::group_by(.data$deploymentID) %>%
    dplyr::count() %>%
    dplyr::ungroup()

  # Remove the count of NA as species and set n as integer
  n_species <- n_species %>%
    dplyr::mutate(n = ifelse(.data$deploymentID %in% unidentified_obs,
      as.integer(.data$n - 1),
      as.integer(.data$n)
    ))

  # Set up n = NA (number of species) for deployments without observations
  deployments_no_obs <-
    deployments_no_obs %>%
    dplyr::select("deploymentID") %>%
    dplyr::mutate(n = NA_integer_)

  # Add them to n_species and return
  n_species %>% dplyr::bind_rows(deployments_no_obs)
}
