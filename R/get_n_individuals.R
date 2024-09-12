#' Get number of individuals for each deployment
#'
#' Gets the number of individuals (of a subset of species) per deployment.
#' The number of observed individuals is stored in field `count` of
#' `observations`.
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
#' - `n`: Number of individuals.
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' 
#' # Get number of observations for each species
#' get_n_individuals(x)
#'
#' # Get number of obs of all species, not identified individuals as well
#' get_n_individuals(x, species = NULL)
#'
#' # Get number of observations of Anas platyrhynchos
#' get_n_individuals(x, species = "Anas platyrhynchos")
#'
#' # Get number of observations of eurasian beaver (vernacular name)
#' get_n_individuals(x, species = "eurasian beaver")
#'
#' # Mix scientific and vernacular names
#' get_n_individuals(x, species = c("Anas platyrhynchos", "eurasian beaver"))
#'
#' # Case insensitive
#' get_n_individuals(x, species = "AnAS PLatyrhyncHOS")
#' get_n_individuals(x, species = "eurasian BEAVER")
#'
#' # Use `filter_observations()` to filter on life stage
#' x %>%
#'   filter_observations(lifeStage == "adult") %>%
#'   get_n_individuals()
#'
#' # Use `filter_observations()` to filter on sex
#' x %>%
#'   filter_observations(sex == "female") %>%
#'   get_n_individuals()
get_n_individuals <- function(x, species = "all") {
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
      species <- taxa(x)$scientificName
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
      unique(c(purrr::pluck(observations, "scientificName"), species))
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

  if (is.null(species)) {
    # sum all observations per deployment
    n_individuals <-
      n_individuals %>%
      dplyr::group_by(.data$deploymentID) %>%
      dplyr::summarise(n = sum(.data$n)) %>%
      dplyr::ungroup()
  }

  # order result by deployments and following same order as in deployments df
  deployments %>%
    dplyr::select("deploymentID") %>%
    dplyr::left_join(n_individuals, by = "deploymentID", multiple = "all")
}
