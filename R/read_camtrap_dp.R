#' Read camtrap-dp formatted data
#'
#' This function reads camera trap data formatted following the [Camera Trap
#' Data Package (Camtrap DP)](https://github.com/tdwg/camtrap-dp) format.
#'
#' Vernacular names are typically used while working with camera trap
#' _observations_, so they are added to the observations as defined in the
#' metadata (slot `taxonomic`), if present.
#'
#' @param path Path to the folder containing the camera trap data files.
#' @param media If `TRUE`, read media records into memory. If `FALSE`,
#'   ignore media file to speed up reading larger Camtrap DP packages.
#'
#' @return A list containing three (tibble) data.frames:
#'   1. `observations`
#'   2. `deployments`
#'   3. `media`
#'
#'   and a list with metadata: `datapackage`
#'
#' @export
#'
#' @importFrom frictionless read_package read_resource
#' @importFrom dplyr %>% .data left_join one_of relocate select starts_with
#' @importFrom here here
#' @importFrom jsonlite read_json

#' @importFrom readr read_csv cols col_character col_number col_datetime
#'
#' @examples
#' \dontrun{
#' library(here)
#' # Read Camtrap DP package
#' camtrap_dp_dir <- here("inst", "extdata", "mica")
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_dir)
#'
#' # Read Camtrap DP package and ignore media file
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_dir, media = FALSE)
#' }
read_camtrap_dp <- function(path, media = TRUE) {
  # check media
  assert_that(media %in% c(TRUE, FALSE),
              msg = "media must be a logical: TRUE or FALSE")
  # read files
  package <- read_package(file.path(path, "datapackage.json"))
  deployments <- read_resource("deployments", package)
  observations <- read_resource("observations", package)

  taxon_infos <- get_species(list(
    "datapackage" = package,
    "deployments" = deployments,
    "media" = NULL,
    "observations" = observations
  ))
  if (!is.null(taxon_infos)) {
    cols_taxon_infos <- names(taxon_infos)
    # add vernacular names to observations
    observations <- left_join(observations,
                              taxon_infos,
                              by  = c("taxonID", "scientificName"))
    observations <- observations %>%
      relocate(one_of(cols_taxon_infos), .after = .data$cameraSetup)
  }
  if (media == TRUE) {
    media <- read_resource("media", package)
  }

  # return list
  if (is.data.frame(media)) {
    list(
      "datapackage" = package,
      "deployments" = deployments,
      "media" = media,
      "observations" = observations
    )
  } else {
    list(
      "datapackage" = package,
      "deployments" = deployments,
      "media" = NULL,
      "observations" = observations
    )
  }
}
