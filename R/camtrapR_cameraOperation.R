#' Get camera operation matrix
#'
#' Returns the [camera operation matrix](
#' https://jniedballa.github.io/camtrapR/reference/cameraOperation.html) as
#' returned by [camtrapR::cameraOperation()](
#' https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).
#'
#' The deployment data are by default grouped by `locationName` (station ID in
#' camtrapR jargon) or another column specified by the user via the 
#' `station_col` argument. If multiple deployments are linked to same location, 
#' daily efforts higher than 1 occur.
#'
#' Partially active days, e.g. the first or the last day of a deployment, result
#' in decimal effort values as in [camtrapR::cameraOperation()](
#' https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).
#'
#' @param station_col Column name to use for identifying the stations. Default:
#'   `"locationName"`.
#' @param camera_col Column name of the column specifying Camera ID. Default:
#'   `NULL`.
#' @param session_col Column name to use for identifying the session. Default:
#'   `NULL`. Use it for creating multi-session / multi-season detection
#'   histories.
#' @param use_prefix Logical (`TRUE` or `FALSE`). If `TRUE` the returned row
#'   names will start with prefix `"Station"` as returned by
#'   [camtrapR::cameraOperation()](
#'   https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).
#'   Default: `FALSE`.
#' @inheritParams get_species
#' @return A matrix. Row names always indicate the station ID. Column names are
#'   dates.
#' @family camtrapR-derived functions
#' @export
#' @examples
#' x <- example_dataset()
#' camtrapR_cameraOperation(x)
#'
#' # Specify column with station names
#' camtrapR_cameraOperation(x, station_col = "locationID")
#'
#' # Specify column with session IDs
#' x_sessions <- x
#' x_sessions$data$deployments <- deployments(x_sessions) %>%
#'   dplyr::mutate(session = ifelse(
#'     stringr::str_starts(.data$locationName, "B_DL_"),
#'       "after2020",
#'       "before2020"
#'   )
#' )
#' camtrapR_cameraOperation(x_sessions, session_col = "session")
#'
#' # Specify column with camera IDs
#' x_cameras <- x_sessions
#' x_cameras$data$deployments$cameraID <- c(1, 2, 3, 4)
#' camtrapR_cameraOperation(x_cameras, camera_col = "cameraID")
#'
#' # Specify both session and camera IDs
#' camtrapR_cameraOperation(
#'   x_cameras,
#'   camera_col = "cameraID",
#'   session_col = "session"
#' )
#' 
#' # Use prefix Station as in camtrapR's camera operation matrix
#' camtrapR_cameraOperation(x, use_prefix = TRUE)
camtrapR_cameraOperation <- function(x,
                       station_col = "locationName",
                       camera_col = NULL,
                       session_col = NULL,
                       use_prefix = FALSE) {
  # Check Camera Trap Data Package
  camtrapdp::check_camtrapdp(x)
  
  # Check that station_col is a single string
  assertthat::assert_that(assertthat::is.string(station_col))
  # Check that station_col is one of the columns in deployments
  assertthat::assert_that(
    station_col %in% names(deployments(x)),
    msg = glue::glue(
      "Station column name (`{station_col}`) is not valid: ",
      "it must be one of the deployments column names."
    )
  )
  
  # Check that `station_col` doesn't contain empty values (NA)
  n_na <- deployments(x) %>%
    dplyr::filter(is.na(.data[[station_col]])) %>%
    nrow()
  assertthat::assert_that(
    n_na == 0,
    msg = glue::glue(
      "Column `{station_col}` must be non-empty: ",
      "{n_na} NAs found."
    )
  )
  
  # Check that `station_col` do not contain the reserved words "__SESS_" and
  # "__CAM_" (no need to remove NAs beforehand as station_col must not contain
  # any NA, see previous check)
  assertthat::assert_that(
    all(!stringr::str_detect(string = deployments(x)[[station_col]],
                             pattern = "__SESS_|__CAM_")),
    msg = glue::glue(
      "Station column name (`{station_col}`) must not contain any of the ",
      "reserved words: \"__SESS_\", \"__CAM_\"."
    )
  )
  
  # Check that `session_col` exists in deployments, if defined, and that its
  # values do not contain the reserved words "__SESS_" and "__CAM_"
  if (!is.null(session_col)) {
    assertthat::assert_that(assertthat::is.string(session_col))
    assertthat::assert_that(
      session_col %in% names(deployments(x)),
      msg = glue::glue(
        "Session column name (`{session_col}`) is not valid: ",
        "it must be one of the deployments column names."
      )
    )
    session_values <- deployments(x)[[session_col]]
    session_values <- session_values[!is.na(session_values)]
    assertthat::assert_that(
      all(!stringr::str_detect(string = session_values,
                               pattern = "__SESS_|__CAM_")),
      msg = glue::glue(
        "Session column name (`{session_col}`) must not contain any of the ",
        "reserved words: \"__SESS_\", \"__CAM_\"."
      )
    )
  }
  
  # Check that `camera_col` exists in deployments, if defined, and that its
  # values do not contain the reserved words "__SESS_" and "__CAM_"
  if (!is.null(camera_col)) {
    assertthat::assert_that(assertthat::is.string(camera_col))
    assertthat::assert_that(
      camera_col %in% names(deployments(x)),
      msg = glue::glue(
        "Camera column name (`{camera_col}`) is not valid: ",
        "it must be one of the deployments column names."
      )
    )
    camera_values <- deployments(x)[[camera_col]]
    camera_values <- camera_values[!is.na(camera_values)]
    assertthat::assert_that(
      all(!stringr::str_detect(string = camera_values,
                           pattern = "__SESS_|__CAM_")),
      msg = glue::glue(
        "Camera column name (`{camera_col}`) must not contain any of the ",
        "reserved words: \"__SESS_\", \"__CAM_\"."
      )
    )
  }
  
  assertthat::assert_that(
    use_prefix %in% c(TRUE, FALSE),
    msg = "use_prefix must be TRUE or FALSE."
  )

  # Extract the deployments
  deploys <- deployments(x)

  # very first day among all stations
  first_day <- min(deploys$deploymentStart)
  # very last day among all stations
  last_day <- max(deploys$deploymentEnd)

  # get sequence with all days from very first day to very last day
  days_operations <- seq(
    lubridate::date(first_day),
    lubridate::date(last_day),
    by = "days"
  )
  # get a string version of this: useful for setting names of final matrix
  days_operations_string <- as.character(days_operations)
  # convert to datetime as it helps while operating with "+" and "-"
  days_operations <- lubridate::as_datetime(days_operations)
  # add aux variables, start_day and end_day for each deployment
  deploys <-
    deploys %>%
    dplyr::mutate(
      start_day = lubridate::date(.data$deploymentStart),
      end_day = lubridate::date(.data$deploymentEnd)
    )

  # make a operation table per deployment
  deployment_operational <- purrr::map(
    deploys$deploymentID,
    function(x) {
      start_day <-
        deploys %>%
        dplyr::filter(.data$deploymentID == x) %>%
        dplyr::pull(start_day)
      end_day <-
        deploys %>%
        dplyr::filter(.data$deploymentID == x) %>%
        dplyr::pull(end_day)
      operational <- days_operations > start_day & days_operations < end_day
      operational[operational == TRUE] <- 1
      # edge cases start and end day
      deploy_df <-
        deploys %>%
        dplyr::filter(.data$deploymentID == x)
      daily_effort_start <- calc_daily_effort(deploy_df, calc_start = TRUE)
      daily_effort_end <- calc_daily_effort(deploy_df, calc_end = TRUE)
      operational[days_operations == start_day] <- daily_effort_start
      operational[days_operations == end_day] <- daily_effort_end
      operational <- dplyr::as_tibble(operational)
      names(operational) <- x
      return(operational)
    }
  )
  names(deployment_operational) <- deploys$deploymentID
  
  # add session to station names
  if (!is.null(session_col)) {
    deploys <- deploys %>%
      dplyr::mutate(!!station_col := paste(.data[[station_col]], 
                                         .data[[session_col]],
                                         sep = "__SESS_")
      )
  }
  
  # add camera to column names
  if (!is.null(camera_col)) {
    deploys <- deploys %>%
      dplyr::mutate(!!station_col := paste(.data[[station_col]], 
                                           .data[[camera_col]],
                                           sep = "__CAM_")
      )
  }
  
  # get for each station_col which days a deployment was active
  camOps <- purrr::map_dfc(
    unique(deploys[[station_col]]),
    function(loc_name) {
      # get deployments linked to the location
      deploys_id <-
        deploys %>%
        dplyr::filter(.data[[station_col]] == loc_name) %>%
        dplyr::pull(.data$deploymentID)
      # get operational dfs linked to these deployment_ids
      dep_dfs <- deployment_operational[
        names(deployment_operational) %in% deploys_id
      ]
      dep_op <- dplyr::bind_cols(dep_dfs)
      # sum daily effort along all deployments at same location
      dep_op <- dplyr::as_tibble(rowSums(dep_op[, names(dep_op)], na.rm = TRUE))
      # set locations as station id
      names(dep_op) <- loc_name
      if (use_prefix == TRUE) {
        names(dep_op) <- paste0("Station", names(dep_op))
      }
      # the 0s should actually be NAs meaning "camera(s) not set up". Notice
      # that in the actual stadium of camera trap dp exchange format, 0s as
      # returned by camtrapR::cameraOperation()` meaning "camera(s) not
      # operational", will never occur.
      dep_op[dep_op == 0] <- NA
      dep_op[[names(dep_op)]] <- as.numeric(dep_op[[names(dep_op)]])
      return(dep_op)
    }
  )
  # transform to matrix
  camOps <- as.matrix(camOps)
  # add names to rows (days)
  rownames(camOps) <- days_operations_string
  # transpose to get location name as rows and days as columns and return
  t(camOps)
}
