#' Get number of identified species for each deployment
#'
#' Gets the number of identified species per deployment.
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param ... Filter predicates for filtering on deployments.
#' @param datapkg Deprecated.
#'   Use `package` instead.
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
get_n_species <- function(package = NULL,
                          ...,
                          datapkg = lifecycle::deprecated()) {
  # check input data package
  package <- check_package(package, datapkg, "get_n_species")

  # extract observations and deployments
  observations <- package$data$observations
  deployments <- package$data$deployments

  # apply filtering
  deployments <- apply_filter_predicate(
    df = deployments,
    verbose = TRUE,
    ...
  )

  # get deployments without observations among the filtered deployments
  deployments_no_obs <- get_dep_no_obs(
    package,
    pred_in("deploymentID", deployments$deploymentID)
  )

  # get species detected by each deployment after filtering
  species <-
    observations %>%
    dplyr::filter(.data$deploymentID %in% deployments$deploymentID) %>%
    dplyr::distinct(.data$deploymentID, .data$scientificName)

  # get deployments with unidentified observations
  unidentified_obs <-
    species %>%
    dplyr::filter(is.na(.data$scientificName)) %>%
    dplyr::pull(.data$deploymentID)

  # get amount of species detected by each deployment
  n_species <-
    species %>%
    dplyr::group_by(.data$deploymentID) %>%
    dplyr::count() %>%
    dplyr::ungroup()

  # remove the count of NA as species and set n as integer
  n_species <- n_species %>%
    dplyr::mutate(n = ifelse(.data$deploymentID %in% unidentified_obs,
      as.integer(.data$n - 1),
      as.integer(.data$n)
    ))

  # set up n = NA (number of species) for deployments without observations
  deployments_no_obs <-
    deployments_no_obs %>%
    dplyr::select("deploymentID") %>%
    dplyr::mutate(n = NA_integer_)

  # add them to n_species and return
  n_species %>% dplyr::bind_rows(deployments_no_obs)
}
