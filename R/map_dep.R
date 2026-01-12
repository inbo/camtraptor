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
  
  # Run the right summary function based on the feature value
  if (feature %in% c("effort", "effort_duration")) {
    summarize_deployments(x)
  } else {
    summarize_observations(x)
  }
  map_summary(
    x,
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
