#' Get number of identified species for each deployment
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Use [summarize_observations()] instead.
#'
#' @inheritParams get_n_obs
#' @returns A tibble data frame with the following columns:
#'   - `deploymentID`: Deployment unique identifier.
#'   - `n`: Number of observed and identified species.
#' @family deprecated exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' # Get number of species
#' get_n_species(x)
get_n_species <- function(x, ...) {
  # Return error if species, life_stage or sex are in the ellipses as 
  # `get_n_species()` has never supported these arguments.
  assertthat::assert_that(
    !any(c("species", "sex", "life_stage") %in% names(list(...))),
    msg = glue::glue(
      "Arguments `species`, `sex` and `life_stage` are not supported by ",
      "`get_n_species()`. Please use `filter_observations()` to filter on ",
      "these variables."
    )
  )
  # Return deprecation warning for function and filtering predicates in ellipses
  summarize_observations_for_deprecated_functions(
    x,
    ...,
    species = NULL,
    sex = NULL,
    life_stage = NULL,
    function_name = deparse(sys.call()[[1]])
  ) %>%
    dplyr::rename("n" = "n_scientificName") %>%
    dplyr::select("deploymentID", "n")
}
