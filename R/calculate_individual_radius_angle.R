#' Calculate animal position
#'
#' Calculates the position of animal relative to a camera based on image pixel
#' positions and site calibration models.
#'
#' @param animal_positions Data frame (tibble) of animal position digitization
#'   data. It must contain (at least) the columns defined in args
#'   `deployment_id`, `event_id`, `x`, `y`, `image_width` and `image_height`.
#' @param calibration_models Named list of deployment calibration models or site
#'   calibration models (`calibs` objects), produced using `cal.site()` (not yet
#'   included in this package). The deployment names are used as names.
#' @param deployment_id Column in `animal_positions` against which names of the
#'   elements can be matched to apply the right deployment calibration models.
#'   Default: `"deploymentID"`.
#' @param event_id Column in `animal_positions` containing the event ID the
#'   images belong to. Default: `"eventID"`.
#' @param x Column in `animal_positions` containing x pixel positions for each
#'   digitised point. Default: `"x"`.
#' @param y Column in `animal_positions` containing y pixel positions for each
#'   digitised point. Default: `"y"`.
#' @param image_width Column in `animal_positions` containing the pixel x
#'   dimension of each image. Default: `"imageWidth"`. Notice that the pixel x
#'   dimension must be consistent for each deployment.
#' @param image_height Column in `animal_positions` containing the pixel y
#'   dimension of each image. Default: `"imageHeight"`. Notice that the pixel y
#'   dimension must be consistent for each deployment.
#' @return Original tibble data frame as passed via `animal_positions` with
#'   additional columns:
#'   - `radius`: Radial distance from camera.
#'   - `angle`: Angular distance from camera.
#'   - `frame_count`: Indicator of the frame order within each sequence.
#' @family density estimation functions
#' @export
#' @examples
#' # Use default values
#' calculate_individual_radius_angle(animal_positions, calibration_models)
calculate_individual_radius_angle <- function(
    animal_positions, # renamed from animal_pos
    calibration_models, # renamed from calib_models
    deployment_id = "deploymentID", # renamed from dep_tag
    event_id = "eventID", # renamed from sequence_id
    x = "x",
    y = "y",
    image_width = "imageWidth",
    image_height = "imageHeight"
    ) {
  # animal_positions is a data.frame
  assertthat::assert_that(is.data.frame(animal_positions))
  # Check presence required columns
  required <- c(deployment_id, event_id, x, y, image_width, image_height)
  not_found_cols <- required[!required %in% names(animal_positions)]
  assertthat::assert_that(
    length(not_found_cols) == 0,
    msg = glue::glue(
      "Columns ",
      glue::glue_collapse(
        glue::backtick(not_found_cols), sep = ", ", last = " and "
      ),
      " not found in `animal_positions`."
    )
  )

  # `calibration_models` is a list
  assertthat::assert_that(is.list(calibration_models))
  # `calibration_models` is a named list
  assertthat::assert_that(
    !is.null(names(calibration_models)),
    msg = "`calibration_models` must be a named list."
  )

  deps <- unique(animal_positions[[deployment_id]])
  got_model <- deps %in% names(calibration_models)
  null_model <- names(calibration_models)[unlist(
    lapply(calibration_models, function(m) {
      is.null(m$model) | is.null(m$cam.model)
    })
  )]
  got_model[match(null_model, deps)] <- FALSE
  if (!all(got_model)) {
    warning(
      glue::glue(
        "Some deployments have no matching calibration model and are stripped ",
        "out: ",
        glue::glue_collapse(deps[!got_model], sep = ",")
      )
    )
    # remove deployments without matching calibration
    animal_positions <- subset(
      animal_positions, animal_positions[, deployment_id] %in% deps[got_model]
    )
    deps <- deps[got_model]
  }

  # Check that image width and height are the same for all multimedia from the
  # same deployment
  n_dims <-
    animal_positions %>%
    dplyr::group_by(.data[[deployment_id]]) %>%
    dplyr::summarise(
      heights = dplyr::n_distinct(.data[[image_width]]),
      widths = dplyr::n_distinct(.data[[image_height]])
    )
  dep_multidim <-
    n_dims %>%
    dplyr::filter(.data$heights > 1 | .data$widths > 1) %>%
    dplyr::distinct(.data[[deployment_id]]) %>%
    dplyr::pull(.data[[deployment_id]])
  if (length(dep_multidim) > 0) {
    warning(
      glue::glue(
        "There is more than one unique value per deployment for `imageWidth` ",
        "and/or `imageHeight` in deployment(s): ",
        glue::glue_collapse(dep_multidim, sep = ", ")
      )
    )
  }

  res <- lapply(deps, function(d) {
    dt <- animal_positions %>% dplyr::filter(.data[[deployment_id]] == d)
    cm <- calibration_models[[d]]$cam.model
    sm <- calibration_models[[d]]$model
    rel_x <- dt[[x]] / dt[[image_width]] - 0.5
    rel_y <- dt[[y]] / dt[[image_height]]
    r <- predict_r(sm, rel_x, rel_y)
    a <- cm$APratio * (dt[[x]] / dt[[image_width]] - 0.5)
    dplyr::tibble(
      dt,
      radius = r,
      angle = a
    )
  })

  res <- dplyr::bind_rows(res)
  tab <- table(res[[event_id]])
  res$frame_count <- sequence(tab)
  res
}
