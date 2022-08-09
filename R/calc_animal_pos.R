#' Calculate animal position
#'
#' Calculate position relative to camera given image pixel positions and site
#' calibration models.
#'
#' @param animal_pos Data.frame (tibble) of animal position digitisation data.
#'   It must contain (at least) the columns defined in args `x`, `y`, `dep_tag`,
#'   `image_width`, `image_height`.
#' @param calib_models Named list of deployment calibration models or site calibration
#'   models (`calibs` objects), produced using `cal.site()` (not yet included in
#'   this package). The deployment names are used as names.
#' @param dep_tag Character naming the column within `animal_pos` against which
#'   names of the elements can be matched to apply the right deployment
#'   calibration models. Default: `"deployment"`.
#' @param sequence_id Character naming the column within `animal_pos` containing
#'   the sequence ID the images belong to. Default: `"sequenceID"`.
#' @param x Character naming the column within `animal_pos` containing x pixel
#'   positions for each digitised point. Default: `"x"`.
#' @param y Character naming the column within `animal_pos` containing y pixel
#'   positions for each digitised point. Default: `"y"`.
#' @param image_width Pixel x dimension of each image.
#'   It must be consistent for each deployment.
#' @param image_height Pixel y dimension of each image.
#'   It must be consistent for each deployment.
#'
#' @export
#' @return Original (tibble) dataframe as passed via `animal_pos` with
#'   additional columns:
#'   - `radius`: radial distance from camera
#'   - `angle`: angular distance from camera
#'   - `frame_count`: indicator of the frame order within each sequence
#'
#' @examples
#' # use default values
#' calc_animal_pos(animal_positions, dep_calib_models)
calc_animal_pos <- function(animal_pos, calib_models,
                            dep_tag = "deployment",
                            sequence_id = "sequenceID",
                            x = "x", y = "y",
                            image_width = "ImageWidth",
                            image_height = "ImageHeight") {
  # animal_pos is a data.frame
  assertthat::assert_that(is.data.frame(animal_pos))
  # Check presence required columns
  required <- c(dep_tag, sequence_id, x, y, image_width, image_height)
  not_found_cols <- required[!required %in% names(animal_pos)]
  assertthat::assert_that(
    length(not_found_cols) == 0,
    msg = glue::glue(
      "Columns ",
      glue::glue_collapse(not_found_cols, sep = ", ", last = " and "),
      " not found in animal_pos."
     )
  )

  # calib_models is a list
  assertthat::assert_that(is.list(calib_models))
  # calib_models is a named list
  assertthat::assert_that(!is.null(names(calib_models)),
                          msg = "calib_models must be a named list.")

  deps <- unique(animal_pos[[dep_tag]])
  got_model <- deps %in% names(calib_models)
  null_model <- names(calib_models)[unlist(
    lapply(calib_models, function(m) {is.null(m$model) | is.null(m$cam.model)})
  )]
  got_model[match(null_model, deps)] <- FALSE
  if (!all(got_model)) {
    warning(
      glue::glue(
        "Some deployments have no matching calibration model ",
        "and are stripped out: ",
        glue::glue_collapse(deps[!got_model], sep = ",")
      )
    )
    # remove deployments without matching calibration
    animal_pos <- subset(animal_pos, animal_pos[, dep_tag] %in% deps[got_model])
    deps <- deps[got_model]
  }

  # Check that image width and height are the same for all multimedia from the
  # same deployment
  n_dims <- animal_pos %>% 
    dplyr::group_by(.data[[dep_tag]]) %>% 
    dplyr::summarise(heights = dplyr::n_distinct(.data[[image_width]]),
                     widths = dplyr::n_distinct(.data[[image_height]])
  )
  dep_multidim <- n_dims %>% 
    dplyr::filter(.data$heights > 1 | .data$widths > 1) %>% 
    dplyr::distinct(.data$deployment) %>% 
    dplyr::pull(.data$deployment)
  if (length(dep_multidim) > 0) {
    warning(
      glue::glue(
        "There is more than one unique value per deployment for ImageWidth",
        " and/or ImageHeight in deployment(s): ",
        glue::glue_collapse(dep_multidim, sep=",")
      )
    )
  }

  res <- lapply(deps, function(d) {
    dt <- animal_pos %>% dplyr::filter(.data[[dep_tag]] == d)
    cm <- calib_models[[d]]$cam.model
    sm <- calib_models[[d]]$model
    rel_x <- dt[[x]] / dt[[image_width]] - 0.5
    rel_y <- dt[[y]] / dt[[image_height]]
    r <- predict_r(sm, rel_x, rel_y)
    a <- cm$APratio * (dt[[x]] / dt[[image_width]] - 0.5)
    dplyr::tibble(
      dt,
      radius = r,
      angle = a)
  })
  
  res <- dplyr::bind_rows(res)
  tab <- table(res[[sequence_id]])
  res$frame_count <- sequence(tab)
  res
}
