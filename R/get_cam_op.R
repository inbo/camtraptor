#' Get camera operation matrix
#'
#' Returns the [camera operation matrix](
#' https://jniedballa.github.io/camtrapR/reference/cameraOperation.html) as
#' returned by [camtrapR::cameraOperation()](
#' https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).
#'
#' The deployment data are by default grouped by `locationName` (station ID in
#' camtrapR jargon) or another column specified by the user.
#' If multiple deployments are linked to same location, daily efforts higher
#' than 1 occur.
#'
#' Partially active days, e.g. the first or the last day of a deployment, result
#' in decimal effort values as in [camtrapR::cameraOperation()](
#' https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param station_col Column name to use for identifying the stations.
#'   Default: `"locationName"`.
#' @param use_prefix Logical (`TRUE`or `FALSE`).
#'   If `TRUE` the returned row names will start with prefix `"Station"` as
#'   returned by [camtrapR::cameraOperation()](
#'   https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).
#'   Default: `FALSE`.
#' @param datapkg Deprecated.
#'   Use `package` instead.
#' @param ... filter predicates for filtering on deployments.
#' @return A matrix.
#'   Row names always indicate the station ID.
#'   Column names are dates.
#' @family exploration functions
#' @importFrom dplyr %>% .data
#' @export
#' @examples
#' get_cam_op(mica)
#'
#' # Applying filter(s) on deployments, e.g. deployments with latitude >= 51.18
#' get_cam_op(mica, pred_gte("latitude", 51.18))
#'
#' # Specify column with station names
#' get_cam_op(mica, station_col = "locationID")
#'
#' # Use prefix Station as in camtrapR's camera operation matrix
#' get_cam_op(mica, use_prefix = TRUE)
get_cam_op <- function(package = NULL,
                       ...,
                       station_col = "locationName",
                       use_prefix = FALSE,
                       datapkg = NULL) {
  # check camera trap data package
  check_package(package, datapkg, "get_cam_op")

  # Check that station_col is a single string
  assertthat::assert_that(assertthat::is.string(station_col))
  # Check that station_col is one of the columns in deployments
  assertthat::assert_that(
    station_col %in% names(package$data$deployments),
    msg = glue::glue(
      "Station column name (`{station_col}`) is not valid: ",
      "it must be one of the deployments column names."
    )
  )
  
  # Check that station_col doesn't contain empty values (NA)
  n_na <- package$data$deployments %>%
    dplyr::filter(is.na(.data[[station_col]])) %>%
    nrow()
  assertthat::assert_that(
    n_na == 0,
    msg = glue::glue(
      "Column `{station_col}` must be non-empty: ",
      "{n_na} NAs found."
    )
  )

  assertthat::assert_that(
    use_prefix %in% c(TRUE, FALSE),
    msg = "use_prefix must be TRUE or FALSE."
  )

  # extract and apply filtering on deployments
  deploys <- apply_filter_predicate(
    df = package$data$deployments,
    verbose = TRUE,
    ...
  )

  # very first day among all stations
  first_day <- min(deploys$start)
  # very last day among all stations
  last_day <- max(deploys$end)

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
      start_day = lubridate::date(.data$start),
      end_day = lubridate::date(.data$end)
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

  # get for each location which days a deployment was active
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
