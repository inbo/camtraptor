#' Get prefix table
#'
#' Store prefixes for info shown while hovering over a deployment with the
#' mouse. List fields in deployments as in
#' https://tdwg.github.io/camtrap-dp/data/#deployments
#'
#' Returns a data.frame of all prefixes with the following columns:
#' - `info`: deployment info
#' - `prefix`: prefix to use
#'
#' @noRd
#' @usage map_dep_prefixes()
map_dep_prefixes <- function() dplyr::as_tibble(mapdep_prefixes)

mapdep_prefixes <- structure(list(
  info = c(
    "scientificName", "deploymentID", "locationID", "locationName", "longitude",
    "latitude", "coordinateUncertainty", "start", "end", "setupBy", "cameraID",
    "cameraModel", "cameraInterval", "cameraHeight", "cameraTilt",
    "cameraHeading", "timestampIssues", "baitUse", "session", "array",
    "featureType", "habitat", "tags", "comments", "n_species", "n_obs",
    "n_individuals", "rai", "rai_individuals", "effort"
  ),
  prefix = c(
    "species: ",
    "deployment ID: ",
    "location ID: ",
    "location name: ",
    "longitude: ",
    "latitude: ",
    "coordinate uncertainty: ",
    "start: ",
    "end: ",
    "setup by: ",
    "camera ID: ",
    "camera model: ",
    "camera interval: ",
    "camera height: ",
    "camera tilt: ",
    "camera heading",
    "timestamp issues: ",
    "bait use: ",
    "session: ",
    "array: ",
    "feature type: ",
    "habitat: ",
    "tags: ",
    "comments: ",
    "species observed: ",
    "observations: ",
    "individuals: ",
    "RAI: ",
    "RAI (individuals): ",
    "effort: "
  )
))

#' Retrieve prefixes (fields) for text to show while hovering with mouse over a
#' deployment
#'
#' @param feature Character, one of:
#' - `n_species`
#' - `n_obs`
#' - `rai`,
#' - `effort`
#' @param infos character vector with deployment fields
#' @importFrom dplyr .data %>%
#' @noRd
get_prefixes <- function(feature, infos) {
  # n can represent #species, #observations, RAI, effort
  infos[infos == "n"] <- feature
  # get all prefixes
  prefixes <- map_dep_prefixes()
  # return the prefixes we need
  prefixes %>% dplyr::filter(.data$info %in% infos)
}
