#' Get prefix table
#'
#' Store prefixes for info shown while hovering over a deployment with the
#' mouse. List fields in deployments as in
#' https://tdwg.github.io/camtrap-dp/data/#deployments
#'
#' Returns a data.frame of all prefixes with the following columns: - `info`:
#' deployment info - `prefix`: prefix to use
#'
#' @importFrom dplyr as_tibble
#'
#' @noRd
#'
#' @usage map_dep_prefixes()
#'
#' @keywords internal
map_dep_prefixes <- function() as_tibble(mapdep_prefixes)

mapdep_prefixes <- structure(list(
  info = c("scientific_name", "deployment_id", "location_id", "location_name", "longitude",
           "latitude", "start", "end", "setup_by", "camera_id", "camera_model",
           "camera_interval", "camera_height", "bait_use", "session", "array",
           "feature_type", "habitat", "tags", "comments", "n_species", "n_obs",
           "rai", "effort"),
  prefix = c("species: ",
             "deployment ID: ",
             "location ID: ",
             "location name: ",
             "longitude: ",
             "latitude: ",
             "start: ",
             "end: ",
             "setup by: ",
             "camera ID: ",
             "camera model: ",
             "camera interval: ",
             "camera height: ",
             "bait use: ",
             "session: ",
             "array: ",
             "feature type: ",
             "habitat: ",
             "tags: ",
             "comments: ",
             "species observed: ",
             "observations: ",
             "RAI: ",
             "Effort: ")
))

#' Retrieve prefixes (fields) for text to show while hovering with mouse over a
#' deployment
#'
#' @param feature character, one of:
#'
#' - `n_species`
#' - `n_obs`
#' - `rai`,
#' - `effort`
#'
#' @param infos character vector with deployment fields
#'
#' @importFrom dplyr .data %>% filter
#'
#' @noRd
#'
#' @keywords internal
get_prefixes <- function(feature,
                         infos) {
  # n can represent #species, #observations, RAI, effort
  infos[infos == "n"] <- feature
  # get all prefixes
  prefixes <- map_dep_prefixes()
  # return the prefixes we need
  prefixes %>% filter(.data$info %in% infos)
}
