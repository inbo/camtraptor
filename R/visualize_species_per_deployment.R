#' Visualize number of species per deployment
#'
#' This function visualizes the number of species detected by each deployment.
#' The circle size and color are proportional to the number of detected species.
#' Deployments without observations are shown as gray circles and a message is
#' returned.See [issue #6](https://github.com/inbo/camtrapdp/issues/6).
#' Unidentified species (empty scientific name) count as 0.
#'
#' Possible filtering about time period, ... (to be added)
#'
#' @param deployments a tibble (data.frame) containing deployments
#' @param observations a tibble (data.frame) containing observations
#' @param cluster a logical value indicating whether using the cluster option
#'   while visualizing maps. Default: TRUE
#' @param hover_column character with the name of the column to use for showing
#'   location deployment information while hovering the mouse over. One from: -
#'   `n`: number of species (default) - `deployment_id` - `location_id` -
#'   `location_name` - `NULL`: hovering disabled
#' @param relative_color_scale a logical indicating whether to use a relative
#'   color scale (`TRUE`) or an absolute scale (`FALSE`). If absolute scale is
#'   used, specify a valid `max_color_scale`
#' @param max_color_scale a number indicating the max value used in the color
#'   scale
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr .data bind_rows count distinct group_by left_join mutate
#'   pull one_of select %>%
#' @importFrom leaflet addLegend addTiles addCircleMarkers colorNumeric leaflet
#'   markerClusterOptions
#' @importFrom glue glue
#' @importFrom rlang !! sym
#'
#' @export
#'
#' @return a leaflet map
#'
#' @examples
#' \dontrun{
#' # cluster enabled and  number of species (`n`) shown while hovering
#' visualize_species_per_deployment(camtrapdp$deployments,
#'                                  camtrapdp$observations)
#' # cluster disabled
#' visualize_species_per_deployment(camtrapdp$deployments,
#'                                  camtrapdp$observations,
#'                                  cluster = FALSE)
#' # show location name while hovering
#' visualize_species_per_deployment(camtrapdp$deployments,
#'                                  camtrapdp$observations,
#'                                  hover_column = "location_name")
#'
#' # use absolute scale for colors
#' visualize_species_per_deployment(camtrapdp$deployments,
#'                                  camtrapdp$observations,
#'                                  relative_color_scale = FALSE,
#'                                  max_color_scale = 4)
#' }
visualize_species_per_deployment <- function(deployments,
                                             observations,
                                             cluster = TRUE,
                                             hover_column = "n",
                                             relative_color_scale = TRUE,
                                             max_color_scale = NULL) {

  # check cluster
  assert_that(cluster %in% c(TRUE, FALSE),
              msg = "cluster must be TRUE or FALSE")

  # check hover_column
  available_hover_cols <- c("n", "deployment_id", "location_id", "location_name")
  if (!is.null(hover_column)) {
    assert_that(
      hover_column %in% available_hover_cols,
      msg = glue("hover_column must be one of: {available_hover_cols*}",
                 .transformer = collapse_transformer(
                   sep = ", ",
                   last = " and ")))
  }

  # check combination relative_color_scale and max_color_scale
  if (relative_color_scale == FALSE) {
    assert_that(!is.null(max_color_scale),
                msg = "If you use an absolute color scale, max_color_scale must be a number, not NULL")
    assert_that(is.numeric(max_color_scale),
                msg = "If you use an absolute color scale, max_color_scale must be a number")
    assert_that(max_color_scale == as.integer(max_color_scale),
                msg = "If you use an absolute color scale, max_color_scale must be an integer")
  }
  if (relative_color_scale == TRUE & !is.null(max_color_scale)) {
    warning("Relative color scale used: max_color_scale value ignored.")
    max_color_scale <- NULL
  }

  # get species detected by each deployment
  species <-
    observations %>%
    distinct(.data$deployment_id, .data$scientific_name)

  # get deployments with unidentified observations
  unidentified_obs <-
    species %>%
    filter(is.na(.data$scientific_name)) %>%
    pull(.data$deployment_id)

  # get amount of species detected by each deployment
  n_species <-
    species %>%
    group_by(.data$deployment_id) %>%
    count()

  # remove the count of NA as species and set n as integer
  n_species <- n_species %>%
    mutate(n = ifelse(.data$deployment_id %in% unidentified_obs,
                      as.integer(.data$n - 1),
                      as.integer(.data$n))
    )

  # get deployments with at least one observations, even if unidentified
  active_deployments_id <- unique(species$deployment_id)
  n_active_deployments <- length(active_deployments_id)

  # all deployments
  deployment_ids <- deployments$deployment_id
  n_deployments <- length(deployment_ids)

  # deployment with no observations
  deployments_no_obs_ids <- deployment_ids[!deployment_ids %in% active_deployments_id]
  n_deployments_no_obs <- length(deployments_no_obs_ids)

  if (n_deployments_no_obs > 0) {
    max_print <- 20
    # Suppress long messages
    if (length(deployments_no_obs_ids) > max_print) {
      options_to_print <- c(deployments_no_obs_ids[1:max_print], "others..")
    } else {
      options_to_print <- deployments_no_obs_ids
    }
    message(glue("There are {n_deployments_no_obs} deployments",
      " with no observations: {options_to_print*}",
      .transformer = collapse_transformer(
        sep = ", ",
        last = " and "
      )
    ))

    # get deployments with no obs and set up n = NA (number of species)
    deployments_no_obs <-
      deployments %>%
      filter(.data$deployment_id %in% deployments_no_obs_ids) %>%
      select(.data$deployment_id) %>%
      mutate(n = NA_integer_)

    # add them to n_species
    n_species <-
      n_species %>%
      bind_rows(deployments_no_obs)
  }

  # add deployment geographic information for maps
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

  # add info while hovering
  if (!is.null(hover_column)) {
    n_species <-
      n_species %>%
      mutate(hover_info = as.character(!!sym(hover_column))) %>%
      mutate(hover_info = ifelse(is.na(.data$hover_info),
                                 "NA",
                                 .data$hover_info))
  } else {
    n_species$hover_info <- NA
  }

  if (relative_color_scale == FALSE) {
    # set all n > max_color_scale to max_color_scale
    n_species <-
      n_species %>%
      mutate(n = ifelse(.data$n > max_color_scale,
                        as.integer(max_color_scale),
                        .data$n))
  }

  # max number of species (with possible upper limit  `max_absolute_scale` in
  # case absolute scale is used) to set number of ticks in legend
  max_n_species <- ifelse(is.null(max_color_scale),
                          max(n_species$n, na.rm = TRUE),
                          max_color_scale)
  # define color palette
  palette_colors <- c("white", "blue")
  pal <- colorNumeric(
    palette = palette_colors,
    domain = c(0, max_n_species))
  # remove NA color to legend until issue is solved:
  # https://github.com/rstudio/leaflet/issues/615
  pal_without_na <- colorNumeric(
    palette = palette_colors,
    domain = c(0,max_n_species),
    na.color=rgb(0,0,0,0))

  bins <- ifelse(max_n_species < 6, max_n_species + 1, 6)

  # make basic start map
  leaflet_map <-
    leaflet(n_species) %>%
    addTiles()


  # if (cluster == TRUE) {
  #   # cluster enabled
    leaflet_map <-
        leaflet_map %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = ~ifelse(is.na(n), 10, n + 10),
          color = ~pal(n),
          stroke = FALSE,
          fillOpacity = 0.5,
          label = ~hover_info,
          clusterOptions = if (cluster == TRUE) markerClusterOptions() else NULL) %>%
        addLegend("bottomright", pal = pal_without_na, values = 0:max_n_species,
                  title = "Number of detected species",
                  opacity = 1,
                  bins = bins,
                  na.label = "",
                  labFormat = labelFormat_scale(max_color_scale = max_color_scale,
                                                digits = 1))
  # } else {
  #   # cluster disabled
  #   if (!is.null(hover_column)) {
  #     # # hovering enabled
  #     leaflet_map <-
  #       leaflet_map %>% addCircleMarkers(
  #         lng = ~longitude,
  #         lat = ~latitude,
  #         radius = ~ifelse(n > 0, n, 10),
  #         color = ~ifelse(n > 0, pal(n), "white"),
  #         stroke = FALSE,
  #         fillOpacity = 0.5,
  #         label = ~hover_info)
  #   } else {
  #     # hovering disabled
  #     leaflet_map <-
  #       leaflet_map %>% addCircleMarkers(
  #         lng = ~longitude,
  #         lat = ~latitude,
  #         radius = ~ifelse(n > 0, n, 10),
  #         color = ~ifelse(n > 0, pal(n), "white"),
  #         stroke = FALSE,
  #         fillOpacity = 0.5)
  #   }
  # }
  leaflet_map
}
