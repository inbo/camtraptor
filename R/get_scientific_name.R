#' Get scientific name based on its vernacular name
#'
#' This function returns the scientific name(s) of a vector of vernacular names
#' based on the observations table of the given camera trap data package.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#'   1. `observations` 2. `deployments` 3. `multimedia`
#'
#'   and a list with metadata: `datapackage`
#' @param vernacular_name a character vector with the vernacular name(s) of the
#'   species as found in the `observations` table of the camera trap data
#'   package
#'
#' @importFrom dplyr .data %>% distinct filter pull tibble
#'
#' @export
#'
#' @return a character vector of scientific name(s)
#'
#' @examples
#' get_scientific_name(camtrapdp, "Norway Rat")
#'
get_scientific_name <- function(datapkg, vernacular_name) {

  check_datapkg(datapkg)

  observations <- datapkg$observations

  vernacular <- vernacular_name

  sn_vn <-
    observations %>%
    filter(tolower(vernacular_name) %in% tolower(vernacular)) %>%
    distinct(.data$scientific_name, .data$vernacular_name) %>%
    mutate(vernacular_name = tolower(.data$vernacular_name))

  # maintain the order of vernacular_name while returning scientific names
  tibble(vernacular_name = tolower(vernacular)) %>%
    left_join(sn_vn,
              by = "vernacular_name") %>%
    pull(.data$scientific_name)
}
