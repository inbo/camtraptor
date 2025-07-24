#' Summarize observations for deprecated functions
#'
#' Summarizes observations of a Camera Trap Data Package object for the
#' deprecated functions `get_n_obs()`, `get_n_individuals()`, `get_rai()` and
#' `get_rai_individuals()`. The deprecation of these functions and their
#' arguments `species`, `life_stage` and `sex` is also handled here.
#'
#' @param func_name Character, name of the deprecated function that is called.
#'   One of`get_n_obs()`, `get_n_individuals()`, `get_rai()` and
#'   `get_rai_individuals()`. Default: `NULL`.
#' @inheritParams get_n_obs
#' @return A tibble data frame with summarized observations as returned by
#'   `summarize_observations()`.
#' @noRd
summarize_observations_for_deprecated_functions <- function(
    x,
    ...,
    species = "all",
    sex = NULL,
    life_stage = NULL,
    function_name = NULL) {
  # Check function_name
  assertthat::assert_that(
    rlang::is_character(function_name, n = 1),
    msg = "Argument `function_name` must be a single character string"
  )
  check_value(
    function_name,
    c("get_n_species",
      "get_n_obs",
      "get_n_individuals",
      "get_rai",
      "get_rai_individuals"),
    "function_name",
    null_allowed = FALSE
  )
  
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = paste0(function_name, "()"),
    details = glue::glue(
      "Please use `summarize_observations(x, group_by = c(\"deploymentID\", \"scientificName\")` ",
      "instead."
    )
  )
  
  # Avoid to call variables like column names to make life easier using
  # filter_observations()
  sex_values <- sex
  
  # Check `species` values
  check_value(
    species,
    c("all", dplyr::pull(taxa(x), "scientificName")),
    "species",
    null_allowed = TRUE
  )
  # Check `sex` values
  check_value(
    sex_values,
    as.character(unique(dplyr::pull(observations(x), "sex"))),
    "sex",
    null_allowed = TRUE
  )
  # Check `life_stage` values
  check_value(
    life_stage,
    as.character(unique(dplyr::pull(observations(x), "lifeStage"))),
    "lifeStage",
    null_allowed = TRUE
  )
  
  # If filtering predicates are passed via `...`, they return error as they are
  # defunct. This will trigger an error if `...` contains filtering predicates
  preds <- list(...)
  # Return warning if ellipses are used and are not filtering predicates
  if (length(preds) > 0) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = paste0(function_name, "(...)"),
      details = glue::glue(
        "Arguments passed via `...` are deprecated as of ",
        "camtraptor 1.0.0 and are ignored. Please, use ",
        "`filter_deployments()` to filter on deployments."
      )
    )
  }
  
  # Filter by `sex` and deprecate it
  if (!is.null(sex_values) && !"all" %in% sex_values) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = paste0(function_name, "(sex)"),
      details = glue::glue(
        "Argument `sex` is deprecated as of camtraptor 1.0.0. Please, use ",
        "`filter_observations()` to filter by `sex`."
      )
    )
    x <- filter_observations(x, sex %in% sex_values)
  }
  
  # Filter by `lifeStage`
  if (!is.null(life_stage)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = paste0(function_name, "(life_stage)"),
      details = glue::glue(
        "Argument `life_stage` is deprecated as of camtraptor 1.0.0. Please, ",
        "use `filter_observations()` to filter by `lifeStage`."
      )
    )
    x <- filter_observations(x, lifeStage %in% life_stage)
  }
  
  # Filter by `species`
  if (is.null(species)) {
    summary <- summarize_observations(x, group_by = "deploymentID")
  } else {
    if (!"all" %in% species) {
      lifecycle::deprecate_warn(
        when = "1.0.0",
        what = paste0(function_name, "(species)"),
        details = glue::glue(
          "Argument `species` is deprecated as of camtraptor 1.0.0. Please, ",
          "use `filter_observations()` to filter by `scientificName`."
        )
      )
      x <- filter_observations(x, scientificName %in% species)
    }
    summary <- summarize_observations(
      x,
      group_by = c("deploymentID", "scientificName")
    )
  }
}
