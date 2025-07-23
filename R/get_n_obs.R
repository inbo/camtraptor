#' Get number of observations for each deployment
#'
#' `r lifecycle::badge("deprecated")`
#' 
#' Gets the number of event-based observations per deployment.
#' 
#' It is deprecated as of camtraptor 1.0.0. Please use [summarize_observations()]
#' instead.
#'
#' @param species `r lifecycle::badge("deprecated")` Character with scientific names.
#' Common names are not supported anymore as of camtraptor 1.0.0. Please, check
#' `filter_observations()` to know how to filter by `scientificName`.
#'   If `"all"` (default) all scientific names are automatically selected.
#'   If `NULL` all observations of all species are taken into account.
#' @param sex `r lifecycle::badge("deprecated")` Character defining the sex class to filter on, e.g. `"female"` or
#'   `c("male", "unknown")`.
#'   If `NULL` (default) all observations of all sex classes are taken into
#'   account. Please, check
#' `filter_observations()` to know how to filter by `sex`.
#' @param life_stage `r lifecycle::badge("deprecated")` Character vector defining the life stage class to filter
#'   on, e.g. `"adult"` or `c("subadult", "adult")`.
#'   If `NULL` (default) all observations of all life stage classes are taken
#'   into account. Please, check `filter_observations()` to know how to filter
#'   by `lifeStage`.
#' @param ... `r lifecycle::badge("deprecated")` filter predicates for filtering
#'   on deployments are not supported anymore and will be ignored. Please use
#'   `filter_deployments()` to filter on deployments.
#' @inheritParams summarize_deployments
#' @return A tibble data frame with the following columns:
#' - `deploymentID`: Deployment unique identifier.
#' - `scientificName`: Scientific name of the species.
#'   This column is omitted if parameter `species = NULL`.
#' - `n`: Number of observations.
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
#' # Get number of observations of Anas platyrhynchos
#' get_n_obs(x, species = "Anas platyrhynchos")
#' 
#' # Specify sex
#' get_n_obs(x, sex = "female")
#' 
#' # Specify life stage
#' get_n_obs(x, life_stage = c("subadult", "adult"))
get_n_obs <- function(x,
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
    dplyr::rename("n" = "n_observations") %>%
    # `any_of()` instead of `all_of()` to avoid error if column `scientificName`
    # is not present (species = NULL)
    dplyr::select(dplyr::any_of(c("deploymentID", "scientificName", "n")))
}
