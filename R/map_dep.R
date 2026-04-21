#' Visualize deployments features
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Please use [map_summary()] instead.
#' @param x Camera trap data package object, as returned by
#'   [camtrapdp::read_camtrapdp()].
#' @inheritParams get_n_obs
#' @inherit map_summary
#' @export
map_dep <- function(
    x,
    feature,
    species = NULL,
    sex = NULL,
    life_stage = NULL,
    effort_unit = NULL,
    cluster = TRUE,
    hover_columns = c(
      "n", "species", "deploymentID", "locationID",
      "locationName", "latitude", "longitude", "start", "end"
    ),
    palette = "inferno",
    zero_values_show = TRUE,
    zero_values_icon_url = 
      "https://img.icons8.com/ios-glyphs/30/000000/multiply.png",
    zero_values_icon_size = 10,
    na_values_show = TRUE,
    na_values_icon_url = 
      "https://img.icons8.com/ios-glyphs/30/FA5252/multiply.png",
    na_values_icon_size = 10,
    relative_scale = TRUE,
    max_scale = NULL,
    radius_range = c(10, 50)) {
  # Throw a warning: `map_dep()` is deprecated
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "map_dep()",
    with = "map_summary()"
  )

  # Throw a deprecation warning if sex is not NULL and filter observations
  if (!is.null(sex)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "map_dep(sex)",
      details = paste0(
        "Please use `filter_observations()` ",
        "with the condition on `sex` instead."
      )
    )
    x <- filter_observations(x, sex %in% {{ sex }})
  }

  # Throw a warning if life_stage is not NULL and filter observations
  if (!is.null(life_stage)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "map_dep(life_stage)",
      details = paste0(
        "Please use `filter_observations()` ",
        "with the condition on `lifeStage` instead."
      )
    )
    x <- filter_observations(x, lifeStage %in% life_stage)
  }
  
  # Throw a warning if `species` is not NULL and filter observations
  if (!is.null(species)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "map_dep(species)",
      details = paste0(
        "Please use `filter_observations()` ",
        "with the condition on `scientificName` instead."
      )
    )
    x <- filter_observations(x, scientificName %in% species)
  }
  
  # Map deprecated feature values to new ones
  features <- c(
    "n_species" = "n_scientificName",
    "n_obs" = "n_observations",
    "n_individuals" = "sum_count",
    "rai" = "rai_observations",
    "rai_individuals" = "rai_count",
    "effort" = "effort_duration"
  )
  # Add new features to the possible features
  features <- c(
    setdiff(.features_observations, features),
    setdiff(.features_deployments, features),
    features
  )
  
  # Replace the deprecated feature with the non deprecated value and return
  # warning
  if (feature %in% names(features)) {
    warning(
      glue::glue(
        "`{feature}` is deprecated as of camtraptor 1.0.0. ",
        "Please use `{features[[feature]]}` instead."
      )
    )
    feature <- features[[feature]]
  }
  # Remove names (deprecated feature names)
  names(features) <- NULL
  
  
  # Map deprecated `hover_columns` to new `hover_columns`
  hover_columns[hover_columns == "n"] <- feature
  hover_columns[hover_columns == "species"] <- "scientificName"
  hover_columns[hover_columns == "start"] <- "deploymentStart"
  hover_columns[hover_columns == "end"] <- "deploymentEnd"
  if (feature == "effort_duration") {
    # Remove `"scientificName"` from `hover_columns` if present as it is not
    # available in deployments summary
    hover_columns <- hover_columns[hover_columns != "scientificName"]
  }
  
  # Run the right summary function based on the feature value
  if (feature == "effort_duration") {
    group_by_vars <- unique(
      c(
        .group_bys_deployments[.group_bys_deployments %in% hover_columns],
        "latitude",
        "longitude"
      )
    )
    df <- summarize_deployments(x, group_by = group_by_vars)
  } else {
    group_by_vars <- unique(
      c(
        .group_bys_deployments[.group_bys_deployments %in% hover_columns],
        .group_bys_observations[.group_bys_observations %in% hover_columns],
        "latitude",
        "longitude"
      )
    )
    if (feature == "n_scientificName") {
      # Remove `scientificName` from grouping variables to get total number of
      # species
      group_by_vars <- group_by_vars[group_by_vars != "scientificName"]
      # Remove `scientificName` from hover columns as it will not be available
      # in the summary data frame
      hover_columns <- hover_columns[hover_columns != "scientificName"]
    }
    if (zero_values_show | na_values_show) {
      df <- summarize_observations(x, group_by = group_by_vars, extend = TRUE)
    } else {
      df <- summarize_observations(x, group_by = group_by_vars)
    }
  }
  
  # Run `map_summary()` with the obtained summary data frame
  map_summary(
    df,
    feature,
    effort_unit = effort_unit,
    cluster = cluster,
    hover_columns = hover_columns,
    palette = palette,
    zero_values_show = zero_values_show,
    zero_values_icon_url = zero_values_icon_url,
    zero_values_icon_size = zero_values_icon_size,
    na_values_show = na_values_show,
    na_values_icon_url = na_values_icon_url,
    na_values_icon_size = na_values_icon_size,
    relative_scale = relative_scale,
    max_scale = max_scale,
    radius_range = radius_range
  )
}
