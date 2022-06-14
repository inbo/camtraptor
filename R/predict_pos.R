#' Predict position
#'
#' Predicts position relative to camera given image pixel positions and site
#' calibration models.
#'
#' @param dat Dataframe of animal position digitisation data.
#'   It must contain (at least) the following columns:
#'   - columns defined in args `x`, `y` and `dep_tag`.
#'   - `ImageWidth`, `ImageHeight`: x and y pixel dimensions of each image. It
#'   must be consistent for each deployment.
#' @param mods Named list of deployment calibration models.
#' @param dep_tag Character naming the column within `dat` against which names of the
#'   elements can be matched to apply the right deployment calibration models.
#'   Default: `"deployment"`.
#' @param x Character naming the column within `dat` containing x pixel
#'   positions for each digitised point. Default: `"x"`.
#' @param y Character naming the column within `dat` containing y pixel
#'   positions for each digitised point. Default: `"y"`.
#'
#' @export
#' @return Original (tibble) dataframe as passed via `dat` with additional
#'   columns:
#'   - `radius`: radial distance from camera
#'   - `angle`: angular distance from camera
#'   - `frame_count`: indicator of the frame order within each sequence
#'
#' @examples
#' To be done
predict_pos <- function(dat, mods, dep_tag = "deployment", x = "x", y = "y") {
  # dat is a data.frame
  assertthat::assert_that(is.data.frame(dat))
  # Check presence required columns
  required <- c(x, y, "ImageWidth", "ImageHeight", dep_tag)
  not_found_cols <- required[!required %in% names(dat)]
  assertthat::assert_that(
    length(not_found_cols) == 0,
    msg = glue::glue("Columns {not_found_cols} not found in dat.")
  )

  # mods is a list
  assertthat::assert_that(is.list(mods))
  # mods is a named list
  assertthat::assert_that(!is.null(names(mods)),
                          msg = "mods must be a named list.")

  deps <- unique(dat[, dep_tag])
  got_model <- deps %in% names(mods)
  null_model <- names(mods)[unlist(lapply(mods, function(m) is.null(m$model)))]
  got_model[match(null_model, deps)] <- FALSE
  if (!all(got_model)) {
    warning(
      glue::glue(
        "Some deployments have no matching calibration model",
        "and are stripped out:\n",
        glue::glue_collapse(deps[!got_model], sep = "\n")
      )
    )
    # remove deployments without matching calibration
    dat <- subset(dat, dat[, dep_tag] %in% deps[got_model])
    deps <- deps[got_model]
  }

  # Check that image width and height are the same for all multimedia from the
  # same deployment
  multidim <- lapply(tapply(dat$ImageWidth, dat[,dep_tag], unique), length) > 1 |
    lapply(tapply(dat$ImageHeight, dat[,dep_tag], unique), length) > 1
  if (any(multidim)) {
    warning(
      glue::glue(
        "There is more than one unique value per deployment for ImageWidth",
        "and/or ImageHeight in deployment(s):\n",
        glue::glue_collapse(names(which(multidim)), sep="\n")
      )
    )
  }

  res <- lapply(deps, function(d) {
    dt <- subset(dat, dat[,dep_tag]==d)
    cm <- mods[[d]]$cam.model
    sm <- mods[[d]]$model
    dplyr::tibble(
      dt,
      radius = predict_r(sm, dt[[x]]/dt$ImageWidth-0.5, dt[[y]]/dt$ImageHeight),
      angle = cm$APratio * (dt[[x]]/dt$ImageWidth-0.5))
  })
  res <- dplyr::bind_rows(res)
  tab <- table(res$sequence_id)
  res$frame_count <- sequence(tab)
  res
}
