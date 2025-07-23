#' Get number of individuals for each deployment
#'
#' `r lifecycle::badge("deprecated")`
#' 
#' Gets the number of individuals per deployment.
#' 
#' It is deprecated as of camtraptor 1.0.0. Please use [summarize_observations()]
#' instead.
#'
#' @inheritParams get_n_obs
#' @inherit get_n_obs return
#' @export
#' @examples
#' x <- example_dataset()
#' 
#' # Get sum of `individualCount` for each species
#' get_n_individuals(x)
#' 
#' # Get sum of `individualCount` of all species (unidentified included)
#' get_n_individuals(x, species = NULL)
#'  
#' # Get number of observations of some species only, e.g. Anas platyrhynchos
#' get_n_individuals(x, species = "Anas platyrhynchos")
#' 
#' # Specify sex
#' get_n_individuals(x, sex = "female")
#' 
#' # Specify life stage
#' get_n_individuals(x, life_stage = c("subadult", "adult"))
get_n_individuals <- function(x,
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
    dplyr::rename("n" = "sum_count") %>%
    # `any_of()` instead of `all_of()` to avoid error if column `scientificName`
    # is not present (species = NULL)
    dplyr::select(dplyr::any_of(c("deploymentID", "scientificName", "n")))
}
