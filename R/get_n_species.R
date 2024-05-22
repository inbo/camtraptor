#' Get number of identified species for each deployment
#'
#' Gets the number of identified species per deployment.
#'
#' @param package Camera trap data package object, as returned by
#'   [camtrapdp::read_camtrapdp()].
#' @param ... Filter predicates for filtering on deployments.
#' @return A tibble data frame with the following columns:
#'   - `deploymentID`: Deployment unique identifier.
#'   - `n`: Number of observed and identified species.
#' @family exploration functions
#' @importFrom dplyr .data %>%
#' @export
#' @examples
#' # Get number of species
#' get_n_species(mica)
#'
#' # Get number of species for deployments with latitude >= 51.18
#' get_n_species(mica, pred_gte("latitude", 51.18))
get_n_species <- function(package, ...) {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(package)
  
  # Extract observations and deployments
  observations <- package$data$observations
  deployments <- package$data$deployments

  # Apply filtering
  deployments <- apply_filter_predicate(
    df = deployments,
    verbose = TRUE,
    ...
  )

  # Get deployments without observations among the filtered deployments
  deployments_no_obs <- get_dep_no_obs(
    package,
    pred_in("deploymentID", deployments$deploymentID)
  )

  # Get species detected by each deployment after filtering
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
