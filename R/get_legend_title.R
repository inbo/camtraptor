#' Get legend title table
#'
#' Store legend titles for deployment visualizations: RAI, effort, number of
#' observations, etc.
#' Returns a data frame of all titles with the following columns:
#' - `feature`: Deployment feature to visualize.
#' - `legend_title`: Legend title.
#'
#' @noRd
#' @usage map_legend_title()
map_legend_title <- function() dplyr::as_tibble(mapdep_legend_titles)

mapdep_legend_titles <- structure(list(
  feature = c(
    "n_species",
    "n_obs",
    "n_individuals",
    "rai",
    "rai_individuals",
    "effort"
  ),
  legend_title = c(
    "Number of detected species",
    "Number of observations",
    "Number of individuals",
    "RAI",
    "RAI (individuals)",
    "Effort"
  )
))

#' Retrieve legend title for deployment visualizations
#'
#' @param feature Character, one of:
#'   - `n_species`
#'   - `n_obs`
#'   - `rai`
#'   - `effort`
#' @importFrom dplyr .data %>%
#' @noRd
get_legend_title <- function(feat) {
  # get all legend titles
  titles <- map_legend_title()
  # return the legend title we need
  titles %>%
    dplyr::filter(.data$feature == feat) %>%
    dplyr::pull(.data$legend_title)
}


#' Add unit to legend title
#'
#' This function is useful when a unit (e.g. temporal unit) should be added to
#' legend title.
#'
#' @param title A character with legend title.
#' @param unit Character with unit to add to `title`.
#' @param use_brackets Logical.
#'   If `TRUE` (default) `unit` is wrapped between brackets, e.g. `(days)`.
#' @noRd
#' @usage map_legend_title("My title", unit = "day", use_bracket = TRUE)
add_unit_to_legend_title <- function(title, unit = NULL, use_brackets = TRUE) {
  if (is.null(unit)) {
    title
  } else {
    if (use_brackets == TRUE) {
      unit <- paste0("(", unit, ")")
    }
    paste(title, unit)
  }
}
