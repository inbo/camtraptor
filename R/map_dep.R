#' Visualize deployments features
#'
#' This function visualizes deployments features such as number of detected
#' species, number of observations and RAI on a dynamic map. The circle size and
#' color are proportional to the mapped feature. Deployments without
#' observations are shown as gray circles and a message is returned.
#'
#' Possible filtering about time period, ... (to be added)
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#'   1. `observations` 2. `deployments` 3. `multimedia`
#'
#'   and a list with metadata: `datapackage`
#' @param feature character, deployment feature to visualize. One of:
#'
#' - `n_species`: number of identified species
#' - `n_obs`: number of observations
#' - `rai`: Relative Abundance Index
#' - `effort`: effort (duration) of the deployment
#'
#' @param species a character with a scientific name. Required for  `rai`,
#'   optional for `n_obs`. Default: `NULL`
#' @param effort_unit time unit to use while visualizing deployment effort
#'   (duration). One of:
#'
#' - `second`
#' - `minute`
#' - `hour`
#' - `day`
#' - `month`
#' - `year`
#' - `NULL` (default) duration objects (e.g. 2594308s (~4.29 weeks)) are shown
#' while hovering and seconds shown in legend
#' @param cluster a logical value
#'   indicating whether using the cluster option while visualizing maps.
#'   Default: TRUE
#' @param hover_columns character with the name of the columns to use for
#'   showing location deployment information while hovering the mouse over. One
#'   or more from deployment columns. Use `NULL` to disable hovering. Default
#'   information:
#'
#'   - `n`: number of species, number of observations, RAI or effort (column
#'   created internally by a `get_*()` function)
#'   - `start`: start deployment
#'   - `end`: end deployment -
#'   - `deployment_id` deployment unique identifier
#'   - `location_id` location unique identifier
#'   - `location_name` location name
#'   - `latitude`:
#'   - `longitude`:
#'
#'   See [section Deployment of Camtrap DP
#'   standard](https://tdwg.github.io/camtrap-dp/data/#deployments) for the full
#'   list of columns you can use
#' @param relative_color_scale a logical indicating whether to use a relative
#'   color scale (`TRUE`) or an absolute scale (`FALSE`). If absolute scale is
#'   used, specify a valid `max_color_scale`
#' @param max_color_scale a number indicating the max value used in the color
#'   scale
#' @param radius_range a vector of length 2 containing the lower and upper limit
#'   of the circle radius. The lower value is used for deployments with zero
#'   observations/identified species. The upper value for the deployment(s) with
#'   the highest number of identified species/observations. Default: `c(10, 50)`
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr .data %>% as_tibble bind_rows bind_cols count distinct filter
#'   group_by left_join mutate pull one_of rename select
#' @importFrom leaflet addLegend addTiles addCircleMarkers colorNumeric leaflet
#'   markerClusterOptions
#' @importFrom glue glue
#' @importFrom grDevices rgb
#' @importFrom htmltools HTML
#' @importFrom lubridate is.POSIXt
#' @importFrom purrr map2 map
#' @importFrom tidyr unite
#'
#' @export
#'
#' @return a leaflet map
#'
#' @examples
#' \dontrun{
#' # show number of species
#' map_dep(
#'   camtrapdp,
#'   "n_species"
#' )
#'
#' # show number of observations
#' map_dep(
#'   camtrapdp,
#'   "n_obs"
#' )
#'
#' # show number of observations of Rattus norvegicus
#' map_dep(
#'   camtrapdp,
#'   "n_obs",
#'   species = "Rattus norvegicus"
#' )
#'
#' # show RAI
#' map_dep(
#'   camtrapdp,
#'   "rai",
#'   species = "Rattus norvegicus"
#' )
#'
#' # show effort (basic duration in seconds)
#' map_dep(
#'   camtrapdp,
#'   "effort"
#' )
#'
#' # show effort (days)
#' map_dep(
#'   camtrapdp,
#'   "effort",
#'   effort_unit = "day"
#' )
#'
#' # show effort (months)
#' map_dep(
#'   camtrapdp,
#'   "effort",
#'   effort_unit = "month"
#' )
#'
#' # cluster disabled
#' map_dep(camtrapdp,
#'   "n_species"
#'   cluster = FALSE
#' )
#'
#' # show only number of observations and location name while hovering
#' map_dep(camtrapdp,
#'   "n_obs",
#'   hover_columns = c("location_name", "n")
#' )
#'
#' # use absolute scale for colors
#' map_dep(camtrapdp,
#'   "n_species",
#'   relative_color_scale = FALSE,
#'   max_color_scale = 4
#' )
#'
#' # change max and min size circles
#' map_dep(
#'   camtrapdp,
#'   "n_obs",
#'   radius_range = c(40, 150)
#' )
#' }
map_dep <- function(datapkg,
                    feature,
                    species = NULL,
                    effort_unit = NULL,
                    cluster = TRUE,
                    hover_columns = c("n", "deployment_id",
                                      "location_id", "location_name",
                                      "latitude", "longitude",
                                      "start", "end"),
                    relative_color_scale = TRUE,
                    max_color_scale = NULL,
                    radius_range = c(10, 50)
) {

  # check input data package
  check_datapkg(datapkg)

  # define possible feature values
  features <- c("n_species", "n_obs", "rai", "effort")

  # check feature
  check_value(feature, features, "feature", null_allowed = FALSE)
  assert_that(length(feature) == 1,
              msg = "feature must have length 1")

  # define possible effort_unit values
  effort_units <- c("second", "minute", "hour", "day", "week", "month", "year")

  # check effort_unit in combination with feature
  if (!is.null(effort_unit) & feature != "effort") {
    warning(glue("effort_unit argument ignored for feature = {feature}"))
    effort_unit <- NULL
  }

  # check effort_unit
  check_value(effort_unit, effort_units, "effort_unit", null_allowed = TRUE)
  assert_that(length(effort_unit) <= 1,
              msg = "effort_unit must have length 1 or be NULL")

  # extract observations and deployments
  observations <- datapkg$observations
  deployments <- datapkg$deployments

  # check species in combination with feature
  if (!is.null(species) & feature %in% c("n_species", "effort")) {
    warning(glue("species argument ignored for feature = {feature}"))
    species <- NULL
  }

  # check cluster
  assert_that(cluster %in% c(TRUE, FALSE),
              msg = "cluster must be TRUE or FALSE"
  )

  # check hover_columns
  if (!is.null(hover_columns)) {
    # check all hover_columns values are allowed
    possible_hover_columns <- map_dep_prefixes()$info
    possible_hover_columns <-
      possible_hover_columns[!possible_hover_columns %in% features]
    hover_columns <- match.arg(arg = hover_columns,
                               choices = c(possible_hover_columns, "n"),
                               several.ok = TRUE)
    # check all hover_columns are in deployments
    not_found_cols <- hover_columns[!hover_columns %in% names(deployments) &
                                      hover_columns != "n"]
    n_not_found_cols <- length(not_found_cols)
    if (n_not_found_cols > 0) {
      warning(glue("There are {n_not_found_cols} columns defined in",
                   " hover_columns not found in deployments: {not_found_cols*}",
                   .transformer = collapse_transformer(
                     sep = ", ",
                     last = " and "
                   )
      ))
    }
  }

  # check combination relative_color_scale and max_color_scale
  if (relative_color_scale == FALSE) {
    assert_that(!is.null(max_color_scale),
                msg = paste("If you use an absolute color scale,",
                            "max_color_scale must be a number, not NULL")
    )
    assert_that(is.numeric(max_color_scale),
                msg = paste("If you use an absolute color scale,",
                            "max_color_scale must be a number")
    )
  }

  if (relative_color_scale == TRUE & !is.null(max_color_scale)) {
    warning("Relative color scale used: max_color_scale value ignored.")
    max_color_scale <- NULL
  }

  # calculate and get feature values
  if (feature == "n_species") {
    feat_df <- get_n_species(datapkg)
  } else if (feature == "n_obs") {
    feat_df <- get_n_obs(datapkg, species = species)
  } else if (feature == "rai") {
    feat_df <- get_rai(datapkg, species = species)
    feat_df <- feat_df %>% rename(n = .data$rai)
  } else if (feature == "effort") {
    feat_df <- get_effort(datapkg)
    if (!is.null(effort_unit)) {
      feat_df$effort <- transform_effort_to_common_units(feat_df$effort,
                                                         unit = effort_unit)
    }
    feat_df <- feat_df %>% rename(n = .data$effort)
  }

  # add deployment information for maps
  # first, mandatory fields to make maps and join
  deploy_columns_to_add <- c("deployment_id", "latitude", "longitude")
  # second, columns for hovering text
  deploy_columns_to_add <- unique(c(deploy_columns_to_add,
                                    hover_columns[hover_columns != "n"]))
  feat_df <-
    feat_df %>%
    left_join(deployments %>%
                select(one_of(deploy_columns_to_add)),
              by = "deployment_id"
    )

  # add info while hovering
  if (!is.null(hover_columns)) {
    hover_info_df <- get_prefixes(feature, hover_columns)
    ## set n_species or n_obs or rai or effort to n in hover_info_df
    hover_info_df$info[hover_info_df$info %in% features] <- "n"
    hover_infos <- as_tibble(map2(hover_info_df$prefix,
                                  hover_info_df$info,
                                  function(x,y) {
                                    info <- feat_df[[y]]
                                    if (is.POSIXt(info)) {
                                      info <- format(info)
                                    }
                                    paste0(x, as.character(feat_df[[y]]))
                                  }), .name_repair = "minimal") %>%
      unite(col = "hover_info", sep = "</p><p>")
    hover_infos <-
      hover_infos %>%
      mutate(hover_info = paste0("<p>", .data$hover_info, "</p>"))
    hover_infos$hover_info <- map(hover_infos$hover_info, ~HTML(.))
    feat_df <-
      feat_df %>%
      bind_cols(hover_infos)
  } else {
    feat_df$hover_info <- NA
  }

  # Set upper limit if absolute value is used and it is lower than some values
  if (relative_color_scale == FALSE) {
    # set all n > max_color_scale to max_color_scale
    feat_df <-
      feat_df %>%
      mutate(n = ifelse(.data$n > max_color_scale,
                        max_color_scale,
                        .data$n
      ))
  }

  # max number of species/obs (with possible upper limit  `max_absolute_scale`
  # in case absolute scale is used) to set number of ticks in legend
  max_n <- ifelse(is.null(max_color_scale),
                  max(feat_df$n, na.rm = TRUE),
                  max_color_scale
  )
  # define color palette
  palette_colors <- c("white", "blue")
  pal <- colorNumeric(
    palette = palette_colors,
    domain = c(0, max_n)
  )
  # remove NA color to legend until this issue is solved:
  # https://github.com/rstudio/leaflet/issues/615
  pal_without_na <- colorNumeric(
    palette = palette_colors,
    domain = c(0, max_n),
    na.color = rgb(0, 0, 0, 0)
  )

  # define bins for ticks of legend
  # bins <- ifelse(max_n < 6, as.integer(max_n) + 1, 6)
  bins <- 6

  # define size scale for avoiding too small or too big circles
  radius_max <- radius_range[2]
  radius_min <- radius_range[1]
  conv_factor <- (radius_max - radius_min)/max(feat_df$n, na.rm = TRUE)

  # define title legend
  title <- get_legend_title(feature)
  # add unit to legend title (for effort)
  title <- add_unit_to_legend_title(title,
                                    unit = effort_unit,
                                    use_brackets = TRUE)

  # make basic start map
  leaflet_map <-
    leaflet(feat_df) %>%
    addTiles()

  leaflet_map %>%
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = ~ ifelse(is.na(n), radius_min, n * conv_factor + radius_min),
      color = ~ pal(n),
      stroke = FALSE,
      fillOpacity = 0.5,
      label = ~hover_info,
      clusterOptions = if (cluster == TRUE) markerClusterOptions() else NULL
    ) %>%
    addLegend("bottomright",
              pal = pal_without_na,
              values = ~n,
              title = title,
              opacity = 1,
              bins = bins,
              na.label = "",
              labFormat = labelFormat_scale(
                max_color_scale = max_color_scale
              )
    )
}
