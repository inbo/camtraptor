#' Visualize number of species per deployment
#'
#' This function visualizes the number of species detected by each deployment.
#' Possible filtering about time period, ... (to be added)
#'
#' @param deployments a tibble (data.frame) containing deployments
#' @param observations a tibble (data.frame) containing observations
#' @param cluster a logical value indicating whether using the cluster option
#'   while visualizing maps. Default: TRUE
#' @param hover_column character with the name of the column to use for showing
#'   location deployment information while hovering the mouse over. By default:
#'   `deployment_id`. Use `NULL` to disable hovering.
#'
#' @importFrom dplyr .data count distinct group_by left_join mutate one_of
#'   select %>%
#' @importFrom leaflet leaflet addTiles addCircleMarkers markerClusterOptions
#' @importFrom glue glue
#' @importFrom rlang !! sym
#'
#' @export
#'
#' @return a leaflet map
#'
#' @examples
#' \dontrun{
#' # cluster enabled and  `deployment_id` column for hovering
#' visualize_species_per_deployment(camtrapdp$deployments,
#'                                  camtrapdp$observations)
#' # cluster disabled
#' visualize_species_per_deployment(camtrapdp$deployments,
#'                                  camtrapdp$observations,
#'                                  cluster = FALSE)
#' visualize_species_per_deployment(camtrapdp$deployments,
#'                                  camtrapdp$observations,
#'                                  hover_column = "location_name")
#' }
visualize_species_per_deployment <- function(deployments,
                                             observations,
                                             cluster = TRUE,
                                             hover_column = "deployment_id") {

  # get species detected by each deployment
  species <-
    observations %>%
    distinct(.data$deployment_id, .data$scientific_name)

  # get amount of species detected by each deployment
  n_species <-
    species %>%
    group_by(.data$deployment_id) %>%
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
      select(one_of("deployment_id",
                    "location_id", # optional field
                    "location_name", # optional field
                    "longitude",
                    "latitude")
      ),
    by = "deployment_id"
    )

  if (!is.null(hover_column)) {
    n_species <-
      n_species %>%
      mutate(hover_info = !!sym(hover_column))
  }

  # make basic start map
  leaflet_map <-
    leaflet(n_species) %>%
    addTiles()

  if (cluster == TRUE) {
    # cluster enabled
    if (!is.null(hover_column)) {
      # hovering enabled
      leaflet_map %>% addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~n,
        stroke = FALSE,
        fillOpacity = 0.5,
        label = ~hover_info,
        clusterOptions = markerClusterOptions())
    } else {
      # hovering disabled
      leaflet_map %>% addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~n,
        stroke = FALSE,
        fillOpacity = 0.5,
        clusterOptions = markerClusterOptions())
    }
  } else {
    # cluster disabled
    if (!is.null(hover_column)) {
      # # hovering enabled
      leaflet_map %>% addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~n,
        stroke = FALSE,
        fillOpacity = 0.5,
        label = ~hover_info
      )
    } else {
      # hovering disabled
      leaflet_map %>% addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~n,
        stroke = FALSE,
        fillOpacity = 0.5
      )
    }
  }
}
