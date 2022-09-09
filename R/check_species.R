#' Check scientific or vernacular name(s)
#'
#' Checks if a given scientific or vernacular name(s) can be found in the
#' metadata (`package$taxonomic`) and returns error if not.
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param species Character vector with scientific or vernacular names.
#' @param arg_name Character with argument name to return in error message
#'   Default: "species".
#' @param datapkg Deprecated. Use `package` instead.
#' @return A character vector with the correspondent scientific names.
#' @family validation functions
#' @importFrom dplyr %>% .data
#' @export
#' @examples
#' # Species is a scientific name
#' check_species(mica, "Martes foina")
#'
#' # Species is a vector of vernacular names
#' check_species(mica, c("beech marten", "european polecat"))
#'
#' # Vernacular names can be specified in any language available
#' check_species(mica, c("vos", "blauwe reiger"))
#'
#' # Vernacular names and scientific names can be mixed up
#' check_species(mica, c("beech marten", "blauwe reiger", "Anas strepera"))
#'
#' # Case insensitive
#' check_species(mica, "AnaS StrePeRa")
#' check_species(mica, "bEEch mARteN")
#'
#' \dontrun{
#' check_species(mica, "bad name")
#' }
check_species <- function(package = NULL,
                          species,
                          arg_name = "species",
                          datapkg = lifecycle::deprecated()) {
  # Check camera trap data package
  package <- check_package(package, datapkg, "check_species")
  assertthat::assert_that(
    !is.null(species) & length(species) > 0,
    msg = "species argument must be specified"
  )

  all_species <-
    get_species(package) %>%
    dplyr::select(-c(.data$taxonID, .data$taxonIDReference))
  check_value(
    tolower(species),
    unlist(all_species) %>% tolower(),
    arg_name,
    null_allowed = FALSE
  )

  purrr::map_chr(species, function(x) {
    # get scientific name in case a vernacular names is given
    if (!tolower(x) %in% tolower(all_species$scientificName)) {
      sn <- get_scientific_name(package, x)
      message(glue::glue("Scientific name of {x}: {sn}"))
      sn
    } else {
      stringr::str_to_sentence(x) # in case the scientific name is not capitalized
    }
  })
}
