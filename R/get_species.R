#' Get species
#'
#' Gets all identified species.
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @return A tibble data frame with all scientific names and vernacular names of
#'   the identified species.
#' @family exploration functions
#' @export
#' @examples
#' get_species(mica)
get_species <- function(package = NULL) {
  # Check camera trap data package
  check_package(package)
  # Get taxonomic information from package metadata
  if (!"taxonomic" %in% names(package)) {
    return(NULL)
  } else {
    taxonomy <- package$taxonomic
    if ("vernacularNames" %in% names(taxonomy[[1]])) {
      # Get all languages used in vernacularNames
      langs <- purrr::map(taxonomy, function(x) {
        vernacular_languages <- NULL
        if ("vernacularNames" %in% names(x)) {
          vernacular_languages <- names(x$vernacularNames)
        }
      })
      langs <- unique(unlist(langs))

      # Fill empty vernacular names with NA
      taxonomy <- purrr::map(taxonomy, function(x) {
        missing_langs <- langs[!langs %in% names(x$vernacularNames)]
        for (i in missing_langs) {
          x$vernacularNames[[i]] <- NA_character_
        }
        x
      })
    }
    # flatten list and transform to data.frame
    purrr::map_dfr(taxonomy, function(x) {
      purrr::list_flatten(x, name_spec = "{outer}.{inner}")
      }
    )
  }
}
