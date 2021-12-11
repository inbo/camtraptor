#' Read camtrap-dp formatted data
#'
#' This function reads camera trap data formatted following the [Camera Trap
#' Data Package (Camtrap DP)](https://github.com/tdwg/camtrap-dp) format.
#'
#' Vernacular names are typically used while working with camera trap
#' _observations_, so they are added to the observations as defined in the
#' metadata (slot `taxonomic`), if present.
#'
#' @param file Path or URL to a `datapackage.json` file.
#' @param media If `TRUE`, read media records into memory. If `FALSE`,
#'   ignore media file to speed up reading larger Camtrap DP packages.
#' @param path Path to the directory containing the datapackage. Use  `file`
#'   with path or URL to a `datapackage.json` file instead.
#' @return A list containing three (tibble) data.frames:
#'   1. `observations`
#'   2. `deployments`
#'   3. `media`
#'
#'   and a list with metadata: `datapackage`
#'
#' @export
#'
#' @importFrom dplyr %>% .data
#'
#' @examples
#' \dontrun{
#' library(here)
#' # Read Camtrap DP package
#' camtrap_dp_dir <- here("inst", "extdata", "mica", "datapackage.json")
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_dir)
#'
#' # Read Camtrap DP package and ignore media file
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_dir, media = FALSE)
#' }
read_camtrap_dp <- function(file = NULL,
                            media = TRUE,
                            path = lifecycle::deprecated()) {
  warning_detail <- paste(
    "Use argument `file` containing the path or URL",
    "to the `datapackage.json` file. The use of argument",
    "`path` with path to the local directory is deprecated since version 0.6.0."
  )
  if (lifecycle::is_present(path) | (!is.null(file) && dir.exists(file))) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "read_camtrap_dp(path)",
      details = warning_detail
    )
  }

  # define the right file value
  if (lifecycle::is_present(path)) {
    file <- file.path(path, "datapackage.json")
  }
  # file value is a valid path
  if (dir.exists(file)) {
    file <- file.path(file, "datapackage.json")
  }

  # check media
  assertthat::assert_that(media %in% c(TRUE, FALSE),
                          msg = "media must be a logical: TRUE or FALSE")
  # read files
  package <- frictionless::read_package(file)
  deployments <- frictionless::read_resource(package, "deployments")
  observations <- frictionless::read_resource(package, "observations")

  # get taxonomic info
  taxon_infos <- get_species(list(
    "datapackage" = package,
    "deployments" = deployments,
    "media" = NULL,
    "observations" = observations
  ))
  # add vernacular names to observations
  if (!is.null(taxon_infos)) {
    cols_taxon_infos <- names(taxon_infos)
    observations <- dplyr::left_join(observations,
                                     taxon_infos,
                                     by  = c("taxonID", "scientificName"))
    observations <- observations %>%
      dplyr::relocate(dplyr::one_of(cols_taxon_infos), .after = .data$cameraSetup)
  }
  if (media == TRUE) {
    media <- frictionless::read_resource(package, "media")
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
