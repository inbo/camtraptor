#' Get species
#'
#' Gets all identified species.
#'
#' @param x Camera trap data package object, as returned by
#'   [camtrapdp::read_camtrapdp()].
#' @return A tibble data frame with all scientific names and vernacular names of
#'   the identified species.
#' @family exploration functions
#' @export
#' @examples
#' x <- example_dataset()
#' get_species(x)
get_species <- function(x) {
  # Get taxonomic information from metadata
  if (!"taxonomic" %in% names(x)) {
    return(NULL)
  } else {
    taxonomy <- x$taxonomic
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
