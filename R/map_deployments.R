#' Visualize deployments features
#'
#' This function visualizes deployments features such as number of detected
#' species, number of observations and RAI (Relative Abundance Index) on a
#' dynamic map. The circle size and colour are proportional to the mapped
#' feature.
#'
#' @param feature Deployment feature to visualize.
#'   One of:
#'   - `n_species`: Number of identified species.
#'   - `n_obs`: Number of observations.
#'   - `n_individuals`: Number of individuals.
#'   - `rai`: Relative Abundance Index.
#'   - `rai_individuals`: Relative Abundance Index based on number of individuals.
#'   - `effort`: Effort (duration) of the deployment.
#' @param species Character with a scientific name.
#'   Required for `rai`, optional for `n_obs`.
#'   Default: `NULL`.
#' @param effort_unit Time unit to use while visualizing deployment effort
#'   (duration).
#'   One of:
#'   - `second`
#'   - `minute`
#'   - `hour`
#'   - `day`
#'   - `month`
#'   - `year`
#'   If  `NULL` (default), the effort is returned in hours.
#' @param cluster Logical value indicating whether using the cluster option
#'   while visualizing maps.
#'   Default: `TRUE`.
#' @param hover_columns Character vector with the name of the columns to use for
#'   showing location deployment information on mouse hover.
#'   One or more from deployment columns.
#'   Use `NULL` to disable hovering.
#'   Default information:
#'   - `n`: Number of species, number of observations, RAI or effort (column
#'   created internally by a `get_*()` function).
#'   - `species`: Species name(s).
#'   - `deploymentStart`: Start deployment.
#'   - `deploymentEnd`: End deployment.
#'   - `deploymentID`: Deployment unique identifier.
#'   - `locationID`: Location unique identifier.
#'   - `locationName`: Location name.
#'   - `latitude`
#'   - `longitude`
#'
#'   See the [Deployment](https://camtrap-dp.tdwg.org/data/#deployments)
#'   section of Camtrap DP for the full list of columns you can use.
#' @param palette The palette name or the colour function that values will be
#'   mapped to.
#'   Typically one of the following:
#'   - A character vector of RGB or named colours. Examples: `c("#000000",
#'   "#0000FF", "#FFFFFF"))`,`topo.colors(10))`.
#'   - The full name of a RColorBrewer palette, e.g. "BuPu" or "Greens", or
#'   viridis palette: `"viridis"`, `"magma"`, `"inferno"` or `"plasma"`.
#'   For more options, see parameter `palette` of [leaflet::colorNumeric()].
#' @param zero_values_show Logical indicating whether to show deployments with
#'   zero values.
#'   Default: `TRUE`.
#' @param zero_values_icon_url Character with URL to icon for showing
#'   deployments with zero values.
#'   Default: a cross (multiply symbol)
#'   `"https://img.icons8.com/ios-glyphs/30/000000/multiply.png"`.
#' @param zero_values_icon_size A number to set the size of the icon to show
#'   deployments with zero values.
#'   Default: 10.
#' @param na_values_show Logical indicating whether to show deployments with
#'   zero values. Notice that only feature `"n_species"`
#'   generates NA values. 
#'   Default: `TRUE`.
#' @param na_values_icon_url Character with URL to icon for showing
#'   deployments with `NA` values. Notice that only feature `"n_species"`
#'   generates NA values.
#'   Default: a red cross (multiply symbol)
#'   `"https://img.icons8.com/ios-glyphs/30/FA5252/multiply.png"`.
#' @param na_values_icon_size A number to set the size of the icon to show
#'   deployments with `NA` values. Notice that only feature `"n_species"`
#'   generates NA values.
#'   Default: 10.
#' @param relative_scale Logical indicating whether to use a relative colour
#'   and radius scale (`TRUE`) or an absolute scale (`FALSE`).
#'   If absolute scale is used, specify a valid `max_scale`.
#' @param max_scale Number indicating the max value used to map colour
#'   and radius.
#' @param radius_range Vector of length 2 containing the lower and upper limit
#'   of the circle radius.
#'   The lower value is used for deployments with zero feature value, i.e. no
#'   observations, no identified species, zero RAI or zero effort.
#'   The upper value is used for the deployment(s) with the highest feature
#'   value (`relative_scale` = `TRUE`) or `max_scale` (`relative_scale`
#'   = `FALSE`).
#'   Default: `c(10, 50)`.
#' @inheritParams n_species
#' @return Leaflet map.
#' @family visualization functions
#' @export
#' @examples
#' \dontrun{
#' x <- example_dataset()
#' 
#' # Show number of species
#' map_deployments(
#'   x,
#'   "n_species"
#' )
#'
#' # Show number of observations (observations of unidentified species included
#' # if any)
#' map_deployments(
#'   x,
#'   "n_obs"
#' )
#'
#' # Show number of observations of Anas platyrhynchos
#' map_deployments(
#'   x,
#'   "n_obs",
#'   species = "Anas platyrhynchos"
#' )
#'
#' # Show number of individuals (individuals of unidentified species included if
#' # any)
#' map_deployments(
#'   x,
#'   "n_individuals"
#' )
#'
#' # Show RAI
#' map_deployments(
#'   x,
#'   "rai",
#'   species = "Anas strepera"
#' )
#' 
#' # Show RAI calculated by using number of detected individuals
#' map_deployments(
#'   x,
#'   "rai_individuals",
#'   species = "Anas strepera"
#' )
#'
#' # Show effort (hours)
#' map_deployments(
#'   x,
#'   "effort"
#' )
#' # Show effort (days)
#' map_deployments(
#'   x,
#'   "effort",
#'   effort_unit = "day"
#' )
#'
#' # Use viridis palette (viridis palettes)
#' map_deployments(
#'   x,
#'   "n_obs",
#'   palette = "viridis"
#' )
#'
#' # Use "BuPu" colour palette (RColorBrewer palettes)
#' map_deployments(
#'   x,
#'   "n_obs",
#'   palette = "BuPu"
#' )
#'
#' # Use a palette defined by colour names
#' map_deployments(
#'   x,
#'   "n_obs",
#'   palette = c("black", "blue", "white")
#' )
#'
#' # Use a palette defined by hex colours
#' map_deployments(
#'   x,
#'   "n_obs",
#'   palette = c("#000000", "#0000FF", "#FFFFFF")
#' )
#'
#' # Do not show deployments with zero values
#' x %>%
#'   filter_observations(lifeStage == "subadult") %>%
#'   map_deployments(
#'     "n_obs",
#'     zero_values_show = FALSE
#'   )
#'
#' # Use same icon but but a non default colour for zero values deployments,
#' # E.g. red (hex: E74C3C)
#' x %>%
#'   filter_observations(lifeStage == "subadult") %>%
#'   map_deployments(
#'     "n_obs",
#'     zero_values_icon_url = "https://img.icons8.com/ios-glyphs/30/E74C3C/multiply.png"
#'   )
#'
#' # ... or yellow (F1C40F)
#' x %>%
#'   filter_observations(lifeStage == "subadult") %>%
#'   map_deployments(
#'     "n_obs",
#'     zero_values_icon_url = "https://img.icons8.com/ios-glyphs/30/F1C40F/multiply.png"
#'   )
#'
#' # Use another icon via a different URL, e.g. the character Fry from Futurama
#' # in green (2ECC71)
#' x %>%
#'   filter_observations(lifeStage == "subadult") %>%
#'   map_deployments(
#'     "n_obs",
#'     zero_values_icon_url = "https://img.icons8.com/ios-glyphs/30/2ECC71/futurama-fry.png"
#'   )
#'   
#' # Set size of the icon for zero values deployments
#' x %>% 
#'   filter_observations(lifeStage == "subadult") %>%
#'   map_deployments("n_obs", zero_values_icon_size = 30)
#'
#' # Use another icon url/size for visualizing NA values (`"n_species"` feature)
#' x %>%
#'   filter_observations(deploymentID != "00a2c20d") %>%
#'   map_deployments(
#'     feature = "n_species",
#'     na_values_icon_url = "https://img.icons8.com/ios-glyphs/30/E74C3C/futurama-fry.png",
#'     na_values_icon_size = 60
#' )
#'
#' # Disable cluster
#' map_deployments(
#'   x,
#'   "n_species",
#'   cluster = FALSE
#' )
#'
#' # Show only number of observations and location name while hovering
#' map_deployments(
#'   x,
#'   "n_obs",
#'   hover_columns = c("locationName", "n")
#' )
#'
#' # Use absolute scale for colours and radius
#' map_deployments(
#'   x,
#'   "n_species",
#'   relative_scale = FALSE,
#'   max_scale = 4
#' )
#'
#' # Change max and min size circles
#' map_deployments(
#'   x,
#'   "n_obs",
#'   radius_range = c(40, 150)
#' )
#' }
map_deployments <- function(x,
                    feature,
                    species = NULL,
                    effort_unit = NULL,
                    cluster = TRUE,
                    hover_columns = c(
                      "n", "species", "deploymentID", "locationID",
                      "locationName", "latitude", "longitude",
                      "deploymentStart", "deploymentEnd"
                    ),
                    palette = "inferno",
                    zero_values_show = TRUE,
                    zero_values_icon_url = "https://img.icons8.com/ios-glyphs/30/000000/multiply.png",
                    zero_values_icon_size = 10,
                    na_values_show = TRUE,
                    na_values_icon_url = "https://img.icons8.com/ios-glyphs/30/FA5252/multiply.png",
                    na_values_icon_size = 10,
                    relative_scale = TRUE,
                    max_scale = NULL,
                    radius_range = c(10, 50)) {

  # Check camera trap data package
  camtrapdp::check_camtrapdp(x)
  
  # Define possible feature values
  features <- c(
    "n_species",
    "n_obs",
    "n_individuals",
    "rai",
    "rai_individuals",
    "effort"
  )

  # Check feature
  check_value(feature, features, "feature", null_allowed = FALSE)
  assertthat::assert_that(
    length(feature) == 1,
    msg = "`feature` must have length 1"
  )

  # Check effort_unit in combination with feature
  if (!is.null(effort_unit) & feature != "effort") {
    warning(glue::glue("`effort_unit` ignored for `feature = {feature}`."))
    effort_unit <- NULL
  }
  
  # Check palette/colours
  viridis_valid_palettes <- c(
    "magma",
    "inferno",
    "plasma",
    "viridis"
  )
  r_color_brewer_palettes <- rownames(RColorBrewer::brewer.pal.info)
  palettes <- c(viridis_valid_palettes, r_color_brewer_palettes)
  if (length(palette) == 1) {
    check_value(
      arg = palette,
      options = palettes,
      arg_name = "palette",
      null_allowed = FALSE
    )
  }

  # Check zero_values_show is a toggle (TRUE or FALSE)
  assertthat::assert_that(
      assertthat::is.flag(zero_values_show),
      msg = "zero_values_show must be a logical: TRUE or FALSE."
  )
  assertthat::assert_that(
    !is.na(zero_values_show),
    msg = "zero_values_show must be a logical: TRUE or FALSE."
  )
  
  # Check na_values_show is a toggle (TRUE or FALSE)
  assertthat::assert_that(
    assertthat::is.flag(na_values_show),
    msg = "na_values_show must be a logical: TRUE or FALSE."
  )
  assertthat::assert_that(
    !is.na(na_values_show),
    msg = "na_values_show must be a logical: TRUE or FALSE."
  )
  
  # Check zero_values_icon_url
  assertthat::assert_that(
    assertthat::is.string(zero_values_icon_url),
    msg = "`zero_values_icon_url` must be a character (URL)."
  )
  # Check zero_values_icon_url in combination with zero_values_show
  if (!zero_values_show) {
    message(glue::glue(
      "`zero_values_show` is {zero_values_show}: ",
      "`zero_values_icon_url` ignored."
    ))
    zero_values_icon_url <- NULL
  }

  # Check na_values_icon_url
  assertthat::assert_that(
    assertthat::is.string(na_values_icon_url),
    msg = "`na_values_icon_url` must be a character (URL)."
  )
  # Check na_values_icon_url in combination with na_values_show
  if (!na_values_show) {
    message(glue::glue(
      "`na_values_show` is {na_values_show}: ",
      "`na_values_icon_url` ignored."
    ))
    na_values_icon_url <- NULL
  }

  # Check zero_values_icon_size
  assertthat::assert_that(
    is.numeric(zero_values_icon_size),
    msg = "`zero_values_icon_size` must be a number."
  )
  # Check zero_values_icon_size in combination with zero_values_show
  if (!zero_values_show) {
    message(glue::glue(
      "`zero_values_show` is {zero_values_show}: ",
      "`zero_values_icon_size` is ignored."
    ))
    zero_values_icon_size <- NULL
  }
  
  # Check na_values_icon_size
  assertthat::assert_that(
    is.numeric(na_values_icon_size),
    msg = "`na_values_icon_size` must be a number."
  )
  # Check na_values_icon_size in combination with na_values_show
  if (!na_values_show) {
    message(glue::glue(
      "`na_values_show` is {na_values_show}: ",
      "`na_values_icon_size` is ignored."
    ))
    na_values_icon_size <- NULL
  }
  
  # Extract observations and deployments
  observations <- observations(x)
  deployments <- deployments(x)

  # Get average lat lon for empty map without deployments
  avg_lat <- mean(deployments$latitude, na.rm = TRUE)
  avg_lon <- mean(deployments$longitude, na.rm = TRUE)

  # Check species in combination with feature and remove from hover in case
  if (is.null(species) | (!is.null(species) & feature %in% c(
    "n_species",
    "effort"
  ))) {
    if (!is.null(species) & feature %in% c("n_species", "effort")) {
      warning(glue::glue("`species` ignored for `feature = {feature}`"))
      species <- NULL
    }
    hover_columns <- hover_columns[hover_columns != "species"]
  } else {
    # Convert species to scientificName in hover_columns
    hover_columns <- replace(
      hover_columns,
      hover_columns == "species",
      "scientificName"
    )
  }

  # Check cluster
  assertthat::assert_that(cluster %in% c(TRUE, FALSE),
    msg = "cluster must be TRUE or FALSE"
  )

  # Check hover_columns
  if (!is.null(hover_columns)) {
    # Check all hover_columns values are allowed
    possible_hover_columns <- map_dep_prefixes()$info
    possible_hover_columns <-
      possible_hover_columns[!possible_hover_columns %in% features]
    hover_columns <- match.arg(
      arg = hover_columns,
      choices = c(possible_hover_columns, "n"),
      several.ok = TRUE
    )
    # Check all hover_columns are in deployments except scientificName
    not_found_cols <- hover_columns[!hover_columns %in% names(deployments) &
      hover_columns != "n" &
      hover_columns != "scientificName"]
    n_not_found_cols <- length(not_found_cols)
    if (n_not_found_cols > 0) {
      warning(glue::glue(
        "Can't find {n_not_found_cols} columns defined in `hover_columns` in ",
        "deployments: ",
        glue::glue_collapse(not_found_cols, sep = ", ", last = " and ")
      ))
    }
  }

  # Check combination relative_scale and max_scale
  if (relative_scale == FALSE) {
    assertthat::assert_that(
      !is.null(max_scale),
      msg = paste(
        "If you use an absolute scale,",
        "`max_scale` must be a number, not `NULL`."
      )
    )
    assertthat::assert_that(
      is.numeric(max_scale),
      msg = paste(
        "If you use an absolute scale,",
        "`max_scale` must be a number."
      )
    )
  }

  if (relative_scale == TRUE & !is.null(max_scale)) {
    warning("Relative scale used: max_scale value ignored.")
    max_scale <- NULL
  }

  # Calculate and get feature values
  if (feature == "n_species") {
    feat_df <- n_species(x)
  } else if (feature == "n_obs") {
    feat_df <- get_n_obs(x, species = species)
  } else if (feature == "n_individuals") {
    feat_df <- get_n_individuals(x, species = species)
  } else if (feature == "rai") {
    feat_df <- get_rai(x, species = species)
    feat_df <- feat_df %>% dplyr::rename(n = "rai")
  } else if (feature == "rai_individuals") {
    feat_df <- get_rai_individuals(
      x,
      species = species
    )
    feat_df <- feat_df %>% dplyr::rename(n = rai)
  } else if (feature == "effort") {
    if (is.null(effort_unit)) {
      effort_unit <- "hour" # Default value of get_effort()
    }
    feat_df <- get_effort(x, unit = effort_unit)
    feat_df <- feat_df %>% dplyr::rename(n = "effort")
  }

  # Define title legend
  title <- get_legend_title(feature)
  # Add unit to legend title (for effort)
  title <- add_unit_to_legend_title(
    title,
    unit = effort_unit,
    use_brackets = TRUE
  )

  # Add informative message if no deployments left after applying filters and
  # return empty map
  if (nrow(feat_df) == 0) {
    message("No deployments left.")
    leaflet_map <-
      leaflet::leaflet(feat_df) %>%
      leaflet::setView(lng = avg_lon, lat = avg_lat, zoom = 10) %>%
      leaflet::addTiles() %>%
      leaflet::addControl(htmltools::tags$b(title), position = "bottomright")
    return(leaflet_map)
  }

  # Add deployment information for maps
  # first, mandatory fields to make maps and join
  deploy_columns_to_add <- c("deploymentID", "latitude", "longitude")
  # second, columns for hovering text
  deploy_columns_to_add <- unique(c(
    deploy_columns_to_add,
    hover_columns[hover_columns != "n" &
      hover_columns != "scientificName"]
  ))
  feat_df <-
    feat_df %>%
    dplyr::left_join(
      deployments %>% dplyr::select(dplyr::one_of(deploy_columns_to_add)),
      by = "deploymentID"
    )

  # Add info while hovering
  if (!is.null(hover_columns)) {
    hover_info_df <- get_prefixes(feature, hover_columns)
    # Set n_species or n_obs or rai or rai_individuals or effort to n in
    # hover_info_df
    hover_info_df$info[hover_info_df$info %in% features] <- "n"
    hover_infos <-
    dplyr::as_tibble(
      purrr::map2(hover_info_df$prefix, hover_info_df$info,
        function(x, y) {
          info <- feat_df[[y]]
          if (lubridate::is.POSIXt(info)) {
            info <- format(info)
          }
          paste0(x, as.character(feat_df[[y]]))
        }),
        .name_repair = "minimal"
      ) %>%
      tidyr::unite(col = "hover_info", sep = "</p><p>")
    hover_infos <-
      hover_infos %>%
      dplyr::mutate(hover_info = paste0("<p>", .data$hover_info, "</p>"))
    hover_infos$hover_info <-
      purrr::map(hover_infos$hover_info, ~ htmltools::HTML(.))
    feat_df <-
      feat_df %>%
      dplyr::bind_cols(hover_infos)
  } else {
    feat_df$hover_info <- NA
  }

  # Set upper limit if absolute value is used and it is lower than some values
  if (relative_scale == FALSE) {
    # Set all n > max_scale to max_scale
    feat_df <-
      feat_df %>%
      dplyr::mutate(
        n = ifelse(.data$n > max_scale, max_scale, .data$n)
      )
  }

  # Max number of species/obs (with possible upper limit  `max_absolute_scale`
  # in case absolute scale is used) to set number of ticks in legend
  max_n <- ifelse(is.null(max_scale),
    ifelse(!all(is.na(feat_df$n)),
      max(feat_df$n, na.rm = TRUE),
      0
    ),
    max_scale
  )

  # Define colour palette
  pal <- leaflet::colorNumeric(
    palette = palette,
    domain = c(0, max_n)
  )

  # Define bins for ticks of legend
  # bins <- ifelse(max_n < 6, as.integer(max_n) + 1, 6)
  bins <- 6

  # Define size scale for avoiding too small or too big circles
  radius_max <- radius_range[2]
  radius_min <- radius_range[1]
  if (max_n != 0) {
    conv_factor <- (radius_max - radius_min) / max_n
  } else {
    conv_factor <- 0
  }

  # Define legend values
  legend_values <- seq(from = 0, to = max_n, length.out = bins)

  # Non zero values deploys (n > 0 and is not NA)
  non_zero_values <- feat_df %>% dplyr::filter(.data$n > 0)
  # Zero values
  zero_values <- feat_df %>% dplyr::filter(.data$n == 0)
  # NA values (only returned by n_species)
  na_values <- feat_df %>% dplyr::filter(is.na(.data$n))
  # Make basic start map
  leaflet_map <-
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addScaleBar()

  # Add markers for deployments with zero valuesif needed
  if (zero_values_show & nrow(zero_values) > 0) {
    # Create icon for zero values
    zero_icons <- leaflet::icons(
      iconUrl = zero_values_icon_url,
      iconWidth = zero_values_icon_size,
      iconHeight = zero_values_icon_size
    )
    # Add icons for zero values to the map
    leaflet_map <-
      leaflet_map %>%
      leaflet::addMarkers(
        icon = zero_icons,
        data = zero_values,
        lng = ~longitude,
        lat = ~latitude,
        label = ~hover_info,
        clusterOptions = if (cluster == TRUE) leaflet::markerClusterOptions() else NULL
      )
  }

  # Add markers for deployments with NA values if needed
  if (na_values_show & nrow(na_values) > 0) {
    # Create icons for NA values
    na_icons <- leaflet::icons(
      iconUrl = na_values_icon_url,
      iconWidth = na_values_icon_size,
      iconHeight = na_values_icon_size
    )
    # Add icons with NAs to the map
    leaflet_map <-
      leaflet_map %>%
      leaflet::addMarkers(
        icon = na_icons,
        data = na_values,
        lng = ~longitude,
        lat = ~latitude,
        label = ~hover_info,
        clusterOptions = if (cluster == TRUE) leaflet::markerClusterOptions() else NULL
      )
  }
  if (nrow(non_zero_values) > 0) {
    leaflet_map <-
      leaflet_map %>%
      leaflet::addCircleMarkers(
        data = non_zero_values,
        lng = ~longitude,
        lat = ~latitude,
        radius = ~ ifelse(is.na(n), radius_min, n * conv_factor + radius_min),
        color = ~ pal(n),
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~hover_info,
        clusterOptions = if (cluster == TRUE) leaflet::markerClusterOptions() else NULL
      ) %>%
      leaflet::addLegend(
        "bottomright",
        pal = pal,
        values = legend_values,
        title = title,
        opacity = 1,
        bins = bins,
        na.label = "",
        labFormat = labelFormat_scale(max_scale = max_scale)
      )
  }
  leaflet_map
}
