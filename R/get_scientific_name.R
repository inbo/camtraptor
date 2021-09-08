#' Get scientific name based on its input_vernacular name
#'
#' This function returns the scientific name(s) of a vector of input_vernacular names
#' based on the observations table of the given camera trap data package.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#'   1. `observations` 2. `deployments` 3. `multimedia`
#'
#'   and a list with metadata: `datapackage`
#' @param vernacular_name a character vector with input vernacular name(s)
#'
#' @importFrom dplyr .data %>% across filter if_any mutate pull starts_with
#' @importFrom glue glue
#' @importFrom purrr map_chr
#'
#' @export
#'
#' @return a character vector of scientific name(s)
#'
#' @examples
#' get_scientific_name(camtrapdp, c("brown rat", "mallard"))
#' # vernacular names can be passed in different languages 
#' get_scientific_name(camtrapdp, c("brown rat", "wilde eend"))
#' # the search is performed case insensitively
#' get_scientific_name(camtrapdp, c("BrOwN RaT"))
#' # if scientific names are passed, they are returned in the case correct form
#' get_scientific_name(camtrapdp, c("Rattus norvegicus", "MyOcAsToR coYPuS"))
get_scientific_name <- function(datapkg, vernacular_name) {

  all_sn_vn <- get_species(datapkg) # datapkg check happens in get_species

  
  input_vernacular <- vernacular_name

  all_sn_vn <-
    all_sn_vn %>%
    mutate(across(starts_with("vernacular_name"), ~ tolower(.)))
  
  map_chr(
    input_vernacular, 
    function(v) {
      # if a scientific name is given (or any vErSiOn of it), return it with the
      # right
      if (tolower(v) %in% tolower(all_sn_vn$scientific_name)) {
        sc_n <- 
          all_sn_vn %>%
          filter(tolower(.data$scientific_name) == tolower(v)) %>%
          pull(.data$scientific_name)
        sc_n
      } else {
        # search within the columns with vernacular names
        sc_n <- 
          all_sn_vn %>%
          filter(if_any(starts_with("vernacular_name"),
                        ~ tolower(.) %in% tolower(v))) %>%
          pull(.data$scientific_name)
        if (length(sc_n) == 0) {
          message(glue("{v} not found"))
          sc_n <- NA_character_
        }
        sc_n
      }
    })
}
