#' Get Relative Abundance Index (RAI)
#'
#' `r lifecycle::badge("deprecated")`
#' 
#' Gets the RAI (Relative Abundance Index) per deployment. The RAI is normalized
#' using 100 days deployment activity. In other words: 
#' `RAI = 100 * (n/effort)` where `n` is the number of event-based observations and `effort` is the effort duration in days.
#' 
#' It is deprecated as of camtraptor 1.0.0. Please use [summarize_observations()]
#' instead.
#'
#' @inheritParams get_n_obs
#' @return A tibble data frame with the following columns: 
#' - `deploymentID`: character vector with the deployment unique identifiers.
#' - `scientificName`: character vector with the scientific name.
#' - `rai`: numeric vector with the relative abundance index.
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
get_rai <- function(x,
                    ...,
                    species = "all",
                    sex = NULL,
                    life_stage = NULL) {
  summarize_observations_for_deprecated_functions(
    x,
    ...,
    species = species,
    sex = sex,
    life_stage = life_stage,
    function_name = deparse(sys.call()[[1]])
  ) %>%
    dplyr::rename("rai" = "rai_observations") %>%
    # `any_of()` instead of `all_of()` to avoid error if column `scientificName`
    # is not present (species = NULL)
    dplyr::select(dplyr::any_of(c("deploymentID", "scientificName", "rai")))
}

#' Get Relative Abundance Index (RAI) based on the sum of individual counts
#'
#' Gets the RAI (Relative Abundance Index) per deployment. The RAI is normalized
#' using 100 days deployment activity. In other words: `RAI = 100 * (n/effort)`
#' where `n` is the sum of individual counts related to event-based observations
#' and `effort` is the effort duration in days.
#' 
#' It is deprecated as of camtraptor 1.0.0. Please use [summarize_observations()]
#' instead.
#'
#' @inheritParams get_n_obs
#' @return A tibble data frame with the following columns:
#'   - `deploymentID`: Deployment unique identifier.
#'   - `scientificName`: Scientific name.
#'   - `rai`: Relative abundance index.
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
get_rai_individuals <- function(x,
                                ...,
                                species = "all",
                                sex = NULL,
                                life_stage = NULL) {
  summarize_observations_for_deprecated_functions(
    x,
    ...,
    species = species,
    sex = sex,
    life_stage = life_stage,
    function_name = deparse(sys.call()[[1]])
  ) %>%
    dplyr::rename("rai" = "rai_count") %>%
    # `any_of()` instead of `all_of()` to avoid error if column `scientificName`
    # is not present (species = NULL)
    dplyr::select(dplyr::any_of(c("deploymentID", "scientificName", "rai")))
}
