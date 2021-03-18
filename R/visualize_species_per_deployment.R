#' Visualize number of species per deployment
#'
#' This function visualizes the number of species detected by each deployment.
#' Possible filtering about time period, ... (to be added)
#'
#' @param deployments a tibble (data.frame) containing deployments
#' @param observations a tibble (data.frame) containing observations
#' @param cluster a logical value indicating whether using the cluster option
#'   while visualizing maps. Default: TRUE
#'
#' @importFrom dplyr distinct group_by count left_join select %>%
#' @importFrom leaflet leaflet addTiles addCircleMarkers markerClusterOptions
#' @importFrom glue glue
#'
#' @export
#'
#' @return a leaflet map
visualize_species_per_deployment <- function(deployments,
                                             observations,
                                             cluster = TRUE) {

  # get species detected by each deployment
  species <-
    observations %>%
    distinct(deployment_id, scientific_name)

  # get amount of species detected by each deployment
  n_species <-
    species %>%
    group_by(deployment_id) %>%
    count()

  # get deployments with at least one observations, even if unidentified
  active_deployments_id <- unique(species$deployment_id)
  n_active_deployments <- length(active_deployments_id)

  # all deployments
  deployment_ids <- deployments$deployment_id
  n_deployments <- length(deployment_ids)

  # deployment with no obsevations
  deployments_no_obs_ids <- deployment_ids[!deployment_ids %in% active_deployments_id]
  n_deployments_no_obs <- length(deployments_no_obs_ids)

  if (n_deployments_no_obs > 0) {
    message(glue("There are {n_deployments_no_obs} deployments",
      " with no observations: {deployments_no_obs*}",
      .transformer = collapse_transformer(
        sep = ", ",
        last = " and "
      )
    ))
  }

  # add coordinates for maps
  n_species <-
    n_species %>%
    left_join(deployments %>%
      select(
        deployment_id,
        location_id,
        location_name,
        longitude,
        latitude
      ),
    by = "deployment_id"
    )

  leaflet_map <-
    leaflet(n_species) %>%
    addTiles()

  if (cluster == TRUE) {
    leaflet_map %>% addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = ~n,
      stroke = FALSE,
      fillOpacity = 0.5,
      label = ~location_name,
      clusterOptions = markerClusterOptions()
    )
  } else {
    leaflet_map %>% addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = ~n,
      stroke = FALSE,
      fillOpacity = 0.5,
      label = ~location_name
    )
  }
}
