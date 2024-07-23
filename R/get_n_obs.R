#' Get number of observations for each deployment
#'
#' Gets the number of observations (of a subset of species) per deployment.
#' The number of observations is defined as the number of distinct sequences
#' (`sequenceID`).
#'
#' @param species Character with scientific names or common names (case
#'   insensitive).
#'   If `"all"` (default) all scientific names are automatically selected.
#'   If `NULL` all observations of all species are taken into account.
#' @inheritParams get_species
#' @return A tibble data frame with the following columns:
#' - `deploymentID`: Deployment unique identifier.
#' - `scientificName`: Scientific name of the species.
#'   This column is omitted if parameter `species = NULL`.
#' - `n`: Number of observations.
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' 
#' # Get number of observations for each species
#' get_n_obs(x)
#'
#' # Get number of obs of all species, not identified individuals as well
#' get_n_obs(x, species = NULL)
#'
#' # Get number of observations of Anas platyrhynchos (scientific name)
#' get_n_obs(x, species = "Anas platyrhynchos")
#'
#' # Get number of observations of eurasian beaver (vernacular names)
#' get_n_obs(x, species = "eurasian beaver")
#'
#' # Case insensitive
#' get_n_obs(x, species = "Anas plaTYrhYnchoS")
#' get_n_obs(x, species = "EUrasian beavER")
#'
#' # Use `filter_observations()` to filter on life stage
#' x %>%
#'   filter_observations(lifeStage == "adult") %>%
#'   get_n_obs()
#'
#' # Use `filter_observations()` to filter on sex
#' x %>%
#'   filter_observations(sex == "female") %>%
#'   get_n_obs()
get_n_obs <- function(x, species = "all") {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  
  # Get observations of the selected species
  if (!is.null(species)) {
    # If species == all retrieve all detected species
    if ("all" %in% species) {
      # If also other values are present, they will be ignored
      if (length(species) > 1) {
        ignored_species <- species[!species == "all"]
        warning(glue::glue(
          "Value `all` found in `species`. All other values are ignored: ",
          glue::glue_collapse(ignored_species, sep = ", ", last = " and ")
        ))
      }
      species <- get_species(x)$scientificName
    }
    # Check species and get scientific names
    species <- check_species(x, species)
    # Filter observations by species
    x <- x %>% filter_observations(scientificName %in% species)
  }

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
      unique(c(unique(purrr::pluck(observations, "scientificName")), species))
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
