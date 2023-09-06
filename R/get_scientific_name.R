#' Get scientific name for vernacular name
#'
#' Gets the scientific name for one or more vernacular names based on the
#' taxonomic information found in the metadata (`package$taxonomic`).
#' The match is performed case insensitively.
#' If a vernacular name is not valid, an error is returned
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param vernacular_name Character vector with input vernacular name(s).
#' @family exploration functions
#' @return Character vector of scientific name(s).
#' @importFrom dplyr .data %>%
#' @export
#' @examples
#' # One or more vernacular names
#' get_scientific_name(mica, "beech marten")
#' get_scientific_name(mica, c("beech marten", "mallard"))
#'
#' # Vernacular names can be passed in different languages
#' get_scientific_name(mica, c("beech marten", "wilde eend"))
#'
#' # Search is performed case insensitively
#' get_scientific_name(mica, c("MaLLarD"))
#'
#' \dontrun{
#' # An error is returned if at least one invalid vernacular name is passed
#' get_scientific_name(mica, "this is a bad vernacular name")
#'
#' # A scientific name is an invalid vernacular name of course
#' get_scientific_name(mica, c("Vulpes vulpes", "wilde eend"))
#' }
get_scientific_name <- function(package = NULL,
                                vernacular_name) {
  # Check camera trap data package
  check_package(package)
  
  # Get scientific names for check
  all_sn_vn <- get_species(package)

  # Get vernacular names for check
  all_vn <-
    all_sn_vn %>%
    dplyr::select(dplyr::starts_with("vernacularName"))
  # Check validity vernacular_name param
  check_value(
    arg = tolower(vernacular_name),
    options = unlist(all_vn) %>% tolower(),
    arg_name = "vernacular_name", null_allowed = FALSE
  )

  input_vernacular <- vernacular_name

  all_sn_vn <-
    all_sn_vn %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("vernacularName"), ~ tolower(.))
    )

  purrr::map_chr(
    input_vernacular,
    function(v) {
      # search within the columns with vernacular names
      sc_n <-
        all_sn_vn %>%
        dplyr::filter(dplyr::if_any(
          dplyr::starts_with("vernacularName"),
          ~ tolower(.) %in% tolower(v)
        )) %>%
        dplyr::pull(.data$scientificName)
      if (length(sc_n) == 0) {
        message(glue::glue("`{v}` is not a valid vernacular name."))
        sc_n <- NA_character_
      }
      sc_n
    }
  )
}
