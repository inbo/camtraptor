#' Check validity of the given scientific or vernacular name(s)
#'
#' Check names against all scientific and vernacular names contained in a camera
#' trap data package.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param species a character vector with scientific or vernacular names
#' @param arg_name character with argument name to return in error message
#'   Default: "species".
#'
#' @importFrom dplyr %>% .data select
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom assertthat assert_that
#' @importFrom stringr str_to_sentence
#'
#' @export
#'
#' @return a character vector with the correspondent scientific names
#'
#' @examples
#' # species is a scientific name
#' check_species(mica, "Martes foina")
#' # species is a vector of vernacular names
#'  check_species(mica, c("beech marten", "european polecat"))
#' # vernacular names can be specified in any language available
#' check_species(mica, c("vos", "blauwe reiger"))
#' # vernacular names and scientific names can be mixed up
#' check_species(mica, c("beech marten", "blauwe reiger", "Anas strepera"))
#' # case insensitive
#' check_species(mica, "AnaS StrePeRa")
#' check_species(mica, "bEEch mARteN")
#' \dontrun{
#' check_species(mica, "bad name")
#' }
check_species <- function(datapkg, species, arg_name = "species") {

  assert_that(!is.null(species) & length(species) > 0,
              msg = "species argument must be specified")

  all_species <- get_species(datapkg) %>%
    select(-c(.data$taxonID, .data$taxonIDReference))
  check_value(tolower(species),
              unlist(all_species) %>% tolower(),
              arg_name,
              null_allowed = FALSE)

  map_chr(species, function(x) {
    # get scientific name in case a vernacular names is given
    if (!tolower(x) %in% tolower(all_species$scientificName)) {
      sn <- get_scientific_name(datapkg, x)
      message(glue("Scientific name of {x}: {sn}"))
      sn
    } else {
      str_to_sentence(x) # in case the scientific name is not capitalized
    }
  })
}
