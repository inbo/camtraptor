#' Read camtrap-dp formatted data
#'
#' This function reads camera trap data formatted following the [Camera Trap
#' Data Package (Camtrap DP)](https://github.com/tdwg/camtrap-dp) format.
#'
#' Vernacular names are typically used while working with camera trap
#' _observations_, so they are added to the observations as defined in the
#' metadata (slot `taxonomic`), if present. Similarly, the scientific names in
#' the observations are not a mandatory field, so they are overwritten by the
#' well maintained scientific names as defined in the metadata (slot
#' `taxonomic`).
#'
#' @param path Path to the folder containing the camera trap data files.
#' @param multimedia If `TRUE`, read multimedia records into memory. If `FALSE`,
#'   ignore multimedia file to speed up reading larger Camtrap DP packages.
#'
#' @return A list containing three (tibble) data.frames:
#'
#'   1. `observations`
#'   2. `deployments`
#'   3. `multimedia`
#'
#'   and a list with metadata: `datapackage`
#'
#' @export
#'
#' @importFrom datapackage read_package read_resource
#' @importFrom dplyr %>% .data left_join relocate select starts_with
#' @importFrom here here
#' @importFrom jsonlite read_json

#' @importFrom readr read_csv cols col_character col_number col_datetime
#'
#' @examples
#' \dontrun{
#' library(here)
#' # Read Camtrap DP package
#' camtrap_dp_dir <- here(
#'     "inst",
#'     "extdata",
#'     "mica-muskrat-and-coypu-20210707160815")
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_dir)
#'
#' # Read Camtrap DP package and ignore multimedia file
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_dir, multimedia = FALSE)
#' }
read_camtrap_dp <- function(path, multimedia = TRUE) {
  # check multimedia
  assert_that(multimedia %in% c(TRUE, FALSE),
              msg = "multimedia must be a logical: TRUE or FALSE")
  # read files
  package <- read_package(file.path(path, "datapackage.json"))
  deployments <- read_resource(package, "deployments")
  observations <- read_resource(package, "observations")
  
  taxon_infos <- get_species(list(
    "datapackage" = package,
    "deployments" = deployments,
    "multimedia" = NULL,
    "observations" = observations
  ))
  if (!is.null(taxon_infos)) {
    # add vernacular names to observations
    observations <- left_join(observations,
                              taxon_infos,
                              by  = c("taxon_id", "scientific_name"))
    observations <- observations %>% 
      relocate(.data$scientific_name, .after = .data$camera_setup)
    observations <- observations %>% 
      relocate(starts_with("vernacular_name"), 
               .after = .data$scientific_name)
  }
  if (multimedia == TRUE) {
    multimedia <- read_resource(package, "multimedia")
  }

  # return list
  if (is.data.frame(multimedia)) {
    list(
      "datapackage" = package,
      "deployments" = deployments,
      "multimedia" = multimedia,
      "observations" = observations
    )
  } else {
    list(
      "datapackage" = package,
      "deployments" = deployments,
      "multimedia" = NULL,
      "observations" = observations
    )
  }
}
