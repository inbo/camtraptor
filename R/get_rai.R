#' Get Relative Abundance Index (RAI)
#'
#' Gets the RAI (Relative Abundance Index) per deployment. The RAI is normalized
#' using 100 days deployment activity. In other words: 
#' `RAI = 100 * (n/effort)` where `n` is the number of observations as
#' calculated via `get_n_obs()` and `effort` is the effort in days as calculated
#' via `get_effort()`.
#'
#' @param species Character with scientific names or common names (case
#'   insensitive). If `"all"` (default) all scientific names are automatically
#'   selected.
#' @inheritParams n_species
#' @return A tibble data frame with the following columns: - `deploymentID`:
#'   Deployment unique identifier. - `scientificName`: Scientific name. - `rai`:
#'   Relative abundance index.
#' @family exploration functions
#' @export
#' @examples
#' # Calculate RAI for all species
#' get_rai(x) # species = "all" by default, so equivalent of
#' get_rai(x, species = "all")
#'
#' # Selected species
#' get_rai(x, species = c("Anas platyrhynchos", "Martes foina"))
#'
#' # With vernacular names, even mixing languages
#' get_rai(x, species = c("mallard", "steenmarter"))
#'
#' # Mixed scientific and vernacular names
#' get_rai(x, species = c("Anas platyrhynchos", "steenmarter"))
#'
#' # Species parameter is case insensitive
#' get_rai(x, species = c("ANAS plAtyRhynChOS"))
#'
#' # Use `filter_observations()` to filter on life stage
#' x %>%
#'   filter_observations(lifeStage == "adult") %>%
#'   get_rai()
#'
#' # Use `filter_observations()` to filter on sex
#' x %>%
#'   filter_observations(sex == "female") %>%
#'   get_rai()
get_rai <- function(x, species = "all") {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  
  get_rai_primitive(x,
    use = "n_obs",
    species = species
  )
}

#' Get Relative Abundance Index (RAI) based on number of individuals
#'
#' Function to get the RAI (Relative Abundance Index) per deployment based on
#' number of detected individuals instead of the number of observations. The RAI
#' is normalized using 100 days deployment activity. In other words: 
#' `RAI = 100 * (n/effort)` where `n` is the number of individuals as calculated
#' via `get_n_individuals()` and `effort` is the effort in days as calculated
#' via `get_effort()`.
#'
#' @param species Character with scientific names or common names (case
#'   insensitive).
#'   If `"all"` (default) all scientific names are automatically selected.
#' @inheritParams n_species
#' @return A tibble data frame with the following columns:
#'   - `deploymentID`: Deployment unique identifier.
#'   - `scientificName`: Scientific name.
#'   - `rai`: Relative abundance index.
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' 
#' # Calculate RAI based on number of individuals
#' get_rai_individuals(x) # species = "all" by default, so equivalent of
#' get_rai_individuals(x, species = "all")
#'
#' # Selected species
#' get_rai_individuals(
#'   x,
#'   species = c("Anas platyrhynchos", "Martes foina")
#' )
#'
#' # With common names, also mixing up languages
#' get_rai_individuals(x, species = c("mallard", "steenmarter"))
#'
#' # Mixed scientific and vernacular names
#' get_rai_individuals(x, species = c("Anas platyrhynchos", "beech marten"))
#'
#' # Species parameter is case insensitive
#' get_rai_individuals(x, species = c("ANAS plAtyRhynChOS"))
#' 
#' # Use `filter_observations()` to filter on life stage
#' x %>%
#'   filter_observations(lifeStage == "adult") %>%
#'   get_rai_individuals()
#'
#' # Use `filter_observations()` to filter on sex
#' x %>%
#'   filter_observations(sex == "female") %>%
#'   get_rai_individuals()
get_rai_individuals <- function(x, species = "all") {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  
  get_rai_primitive(x, use = "n_individuals", species = species)
}


#' Primitive function for RAI calculation
#'
#' This function is the primitive function behind `get_rai()` and
#' `get_rai_individuals()` to calculate RAI based on number of observations or
#' number of individuals respectively.
#'
#' @param use Character, one of:
#'   - `"n_obs"`: Calculate RAI based on number of observation (standard).
#'   - `"n_individuals"`: Calculate RAI based on number of individuals.
#' @inheritParams n_species
#' @return A tibble data frame.
#' @noRd
get_rai_primitive <- function(x, use, species) {
  # define possible feature values
  uses <- c("n_obs", "n_individuals")

  # check use
  check_value(use, uses, "use", null_allowed = FALSE)
  assertthat::assert_that(
    length(use) == 1,
    msg = "use must have length 1"
  )

  # get all identified species if species arg is equal to "all"
  if ("all" %in% species) {
    species <- get_species(x)$scientificName
  }
  # check species
  species <- check_species(x, species)

  if (use == "n_obs") {
    # get number of observations
    n_df <- get_n_obs(x, species = species)
  } else {
    # get number of individuals
    n_df <- get_n_individuals(x, species = species)
  }

  # extract deployments
  deployments <- deployments(x)

  # get deployment duration (effort) in days
  dep_effort <- get_effort(x, unit = "day")

  # calculate RAI
  n_df %>%
    dplyr::left_join(dep_effort,
      by = "deploymentID"
    ) %>%
    dplyr::group_by(
      .data$deploymentID,
      .data$scientificName
    ) %>%
    dplyr::summarise(rai = .data$n * 100 / .data$effort) %>%
    dplyr::ungroup()
}
