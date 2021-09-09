#' Check validity of the given scientific or vernacular name(s)
#'
#' Check names against all scientific and vernacular names contained in a camera
#' trap data package.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#'   1. `observations`
#'   2. `deployments`
#'   3. `multimedia`
#'
#'   and a list with metadata: `datapackage`
#'
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
#' check_species(camtrapdp, "Gallinula chloropus")
#' # species is a vector of vernacular names
#'  check_species(camtrapdp, c("mallard", "norway rat"))
#' # vernacular names can be specified in any language available
#' check_species(camtrapdp, c("wilde eend", "bruine rat"))
#' # vernacular names and scientific names can be mixed up
#' check_species(camtrapdp, c("mallard", "bruine rat", "Ondatra zibethicus"))
#' # case insensitive
#' check_species(camtrapdp, "galliNULa CHloropUs")
#' check_species(camtrapdp, "MalLARD")
#' \dontrun{
#' check_species(camtrapdp, "bad name")
#' }
check_species <- function(datapkg, species, arg_name = "species") {

  assert_that(!is.null(species) & length(species) > 0,
              msg = "species argument must be specified")

  all_species <- get_species(datapkg) %>%
    select(-.data$taxon_id)
  check_value(tolower(species),
              unlist(all_species) %>% tolower(),
              arg_name,
              null_allowed = FALSE)

  map_chr(species, function(x) {
    # get scientific name in case a vernacular names is given
    if (!tolower(x) %in% tolower(all_species$scientific_name)) {
      sn <- get_scientific_name(datapkg, x)
      message(glue("Scientific name of {x}: {sn}"))
      sn
    } else {
      str_to_sentence(x) # in case the scientific name is not capitalized
    }
  })
}
