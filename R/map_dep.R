#' Visualize deployments features
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This function is deprecated. Please use [map_deployments()] instead.
#'
#' @inheritParams map_deployments
#' @inherit map_deployments
#' @export
map_dep <- function(x,
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
                    zero_values_icon_url = "https://img.icons8.com/ios-glyphs/30/000000/multiply.png",
                    zero_values_icon_size = 10,
                    na_values_show = TRUE,
                    na_values_icon_url = "https://img.icons8.com/ios-glyphs/30/FA5252/multiply.png",
                    na_values_icon_size = 10,
                    relative_scale = TRUE,
                    max_scale = NULL,
                    radius_range = c(10, 50)) {
  
  # Throw a deprecation warning if sex is not NULL and filter observations
  if (!is.null(sex)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "map_dep(sex)",
      msg = "Please use `filter_observations()` beforehand."
    )
    x <- filter_observations(x, sex %in% {{ sex }})
  }
  
  # Throw a warning if life_stage is not NULL and filter observations
  if (!is.null(life_stage)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "map_dep(life_stage)",
      msg = "Please use `filter_observations()` beforehand."
    )
    x <- filter_observations(x, lifeStage %in% life_stage)
  }
  
  lifecycle::deprecate_warn(when = "1.0.0",
                            what = "map_dep()",
                            with = "map_deployments()"
  )
  
  map_deployments(
    x,
    feature,
    species = NULL,
    effort_unit = NULL,
    cluster = TRUE,
    hover_columns = c("n", "species", "deploymentID", "locationID",
                      "locationName", "latitude", "longitude", "start", "end"
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
    radius_range = c(10, 50)
  )
}
