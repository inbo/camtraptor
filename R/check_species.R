#' Check scientific or vernacular name(s)
#'
#' Checks if a given scientific or vernacular name(s) can be found in the
#' metadata (`x$taxonomic`) and returns error if not.
#'
#' @param species Character vector with scientific or vernacular names.
#' @param arg_name Character with argument name to return in error message
#'   Default: "species".
#' @inheritParams get_species
#' @return A character vector with the correspondent scientific names.
#' @family validation functions
#' @export
#' @examples
#' x <- example_dataset()
#' 
#' # Species is a scientific name
#' check_species(x, "Martes foina")
#'
#' # Species is a vector of vernacular names
#' check_species(x, c("beech marten", "european polecat"))
#'
#' # Vernacular names can be specified in any language available
#' check_species(x, c("vos", "blauwe reiger"))
#'
#' # Vernacular names and scientific names can be mixed up
#' check_species(x, c("beech marten", "blauwe reiger", "Anas strepera"))
#'
#' # Case insensitive
#' check_species(x, "AnaS StrePeRa")
#' check_species(x, "bEEch mARteN")
#'
#' \dontrun{
#' check_species(x, "bad name")
#' }
check_species <- function(x,
                          species,
                          arg_name = "species") {
  # Check camera trap Data Package
  camtrapdp::check_camtrapdp(x)
  
  assertthat::assert_that(
    !is.null(species) & length(species) > 0,
    msg = "`species` parameter must be specified"
  )

  all_species <-
    get_species(x) %>%
    dplyr::select(-dplyr::any_of(c("taxonID", "taxonIDReference")))
  
  check_value(
    tolower(species),
    unlist(all_species) %>% tolower(),
    arg_name,
    null_allowed = FALSE
  )

  purrr::map_chr(species, function(s) {
    # get scientific name in case a vernacular names is given
    if (!tolower(s) %in% tolower(all_species$scientificName)) {
      sn <- get_scientific_name(x, s)
      message(glue::glue("Scientific name of {s}: {sn}"))
      sn
    } else {
      stringr::str_to_sentence(s) # in case the scientific name is not capitalized
    }
  })
}
