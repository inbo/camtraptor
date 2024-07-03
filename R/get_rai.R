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
#' @param sex Character defining the sex class to filter on, e.g. `"female"` or
#'   `c("male", "unknown")`. If `NULL` (default) all observations of all sex
#'   classes are taken into account.
#' @param life_stage Character vector defining the life stage class to filter
#'   on, e.g. `"adult"` or `c("subadult", "adult")`. If `NULL` (default) all
#'   observations of all life stage classes are taken into account.
#' @param ... Filter predicates for filtering on deployments.
#' @inheritParams get_species
#' @return A tibble data frame with the following columns: - `deploymentID`:
#'   Deployment unique identifier. - `scientificName`: Scientific name. - `rai`:
#'   Relative abundance index.
#' @family exploration functions
#' @export
#' @examples
#' # Calculate RAI for all species
#' get_rai(mica) # species = "all" by default, so equivalent of
#' get_rai(mica, species = "all")
#'
#' # Selected species
#' get_rai(mica, species = c("Anas platyrhynchos", "Martes foina"))
#'
#' # With vernacular names, even mixing languages
#' get_rai(mica, species = c("mallard", "steenmarter"))
#'
#' # Mixed scientific and vernacular names
#' get_rai(mica, species = c("Anas platyrhynchos", "steenmarter"))
#'
#' # Species parameter is case insensitive
#' get_rai(mica, species = c("ANAS plAtyRhynChOS"))
#'
#' # Specify sex
#' get_rai(mica, sex = "female")
#' get_rai(mica, sex = c("female", "unknown"))
#'
#' # Specify life stage
#' get_rai(mica, life_stage = "adult")
#' get_rai(mica, life_stage = c("adult", "subadult"))
#'
#' # Apply filter(s): deployments with latitude >= 51.18
#' get_rai(mica, pred_gte("latitude", 51.18))
get_rai <- function(package,
                    ...,
                    species = "all",
                    sex = NULL,
                    life_stage = NULL) {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(package)
  
  get_rai_primitive(package, ...,
    use = "n_obs",
    species = species,
    sex = sex,
    life_stage = life_stage
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
#' @param sex Character defining the sex class to filter on, e.g. `"female"`
#'   or `c("male", "unknown")`.
#'   If `NULL` (default) all observations of all sex classes are taken into
#'   account.
#' @param life_stage Character vector defining the life stage class to filter
#'   on, e.g. `"adult"` or `c("subadult", "adult")`.
#'   If `NULL` (default) all observations of all life stage classes are taken
#'   into account.
#' @param ... Filter predicates for filtering on deployments.
#' @inheritParams get_species
#' @return A tibble data frame with the following columns:
#'   - `deploymentID`: Deployment unique identifier.
#'   - `scientificName`: Scientific name.
#'   - `rai`: Relative abundance index.
#' @family exploration functions
#' @export
#' @examples
#' # Calculate RAI based on number of individuals
#' get_rai_individuals(mica) # species = "all" by default, so equivalent of
#' get_rai_individuals(mica, species = "all")
#'
#' # Selected species
#' get_rai_individuals(mica,
#'   species = c("Anas platyrhynchos", "Martes foina")
#' )
#'
#' # With common names, also mixing up languages
#' get_rai_individuals(mica, species = c("mallard", "steenmarter"))
#'
#' # Mixed scientific and vernacular names
#' get_rai_individuals(mica, species = c("Anas platyrhynchos", "beech marten"))
#'
#' # Species parameter is case insensitive
#' get_rai_individuals(mica, species = c("ANAS plAtyRhynChOS"))
#'
#' # Specify sex
#' get_rai_individuals(mica, sex = "female")
#' get_rai_individuals(mica, sex = c("female", "unknown"))
#'
#' # Specify life stage
#' get_rai_individuals(mica, life_stage = "adult")
#' get_rai_individuals(mica, life_stage = c("adult", "subadult"))
#'
#' # Apply filter(s): deployments with latitude >= 51.18
#' get_rai_individuals(mica, pred_gte("latitude", 51.18))
get_rai_individuals <- function(package,
                                ...,
                                species = "all",
                                sex = NULL,
                                life_stage = NULL) {
  # Check camera trap data package
  camtrapdp::check_camtrapdp(package)
  
  get_rai_primitive(package, ...,
    use = "n_individuals",
    species = species,
    sex = sex,
    life_stage = life_stage
  )
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
#' @inheritParams get_species
#' @return A tibble data frame.
#' @importFrom dplyr .data %>%
#' @noRd
get_rai_primitive <- function(package, use, species, sex, life_stage, ...) {
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
    species <- get_species(package)$scientificName
  }
  # check species
  species <- check_species(package, species)

  if (use == "n_obs") {
    # get number of observations
    n_df <- get_n_obs(package, species = species, sex = sex, life_stage = life_stage, ...)
  } else {
    # get number of individuals
    n_df <- get_n_individuals(package,
      species = species,
      sex = sex,
      life_stage = life_stage,
      ...
    )
  }

  # extract deployments
  deployments <- package$data$deployments

  # get deployment duration (effort) in days
  dep_effort <- get_effort(package, unit = "day", ...)

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
