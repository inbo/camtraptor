#' Get species
#'
#' Function to get all identified species
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @return a data.frame with all scientific names and vernacular names of the
#'   identified species.
#'
#' @examples
#' get_species(mica)
get_species <- function(datapkg) {
  # Check input data package
  check_datapkg(datapkg)

  # Get taxonomic information from package metadata
  if (!"taxonomic" %in% names(datapkg)) {
    return(NULL)
  } else {
    taxonomy <- datapkg$taxonomic
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
    purrr::map_dfr(taxonomy, function(x) {
      dplyr::tibble(as.data.frame(x))
    })
  }
}
