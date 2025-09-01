#' Get number of identified species for each deployment
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' It is deprecated as of camtraptor 1.0.0. Please use [summarize_observations()]
#' instead.
#'
#' @inheritParams get_n_obs
#' @return A tibble data frame with the following columns:
#'   - `deploymentID`: Deployment unique identifier.
#'   - `n`: Number of observed and identified species.
#' @export
#' @examples
#' x <- example_dataset()
#' # Get number of species
#' get_n_species(x)
get_n_species <- function(x, ...) {
  # Return deprecation warning for function and filtering predicates in ellipses
  summarize_observations_for_deprecated_functions(
    x,
    ...,
    species = NULL,
    sex = NULL,
    life_stage = NULL,
    function_name = deparse(sys.call()[[1]])
  )
  summarize_observations(x, group_by = "deploymentID") %>%
    dplyr::rename("n" = "n_scientificName") %>%
    dplyr::select("deploymentID", "n")
}
