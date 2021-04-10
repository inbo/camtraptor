#' Check validity of the given scientific or vernacular name(s)
#'
#' Check names against all scientific and vernacular names contained in a camera
#' trap data package
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#'   1. `observations` 2. `deployments` 3. `multimedia`
#'
#'   and a list with metadata: `datapackage`
#'
#' @param species a character vector with scientific or vernacular names.
#'
#' @importFrom purrr map_chr
#' @importFrom assertthat assert_that
#'
#' @export
#'
#' @return a character vector with the correspondent scientific names
#'
#' @examples
#' check_species(camtrapdp, c("Mallard", "Norway Rat"))
#' # case insensitive
#' check_species(camtrapdp, "MalLARD")
#'
check_species <- function(datapkg, species) {

  assert_that(!is.null(species) & length(species) > 0,
              msg = "species argument must be specified")

  all_species <- get_species(datapkg)
  check_value(tolower(species),
              c(tolower(all_species$scientific_name),
                tolower(all_species$vernacular_name)),
              "species")

  map_chr(species, function(x) {
    # get scientific name in case a vernacular names is given
    if (tolower(x) %in% tolower(all_species$vernacular_name)) {
      sn <- get_scientific_name(datapkg, x)
      message(glue("Scientific name of {x}: {sn}"))
      sn
    } else {
      x
    }
  })
}
