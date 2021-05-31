#' Get camera operation matrix
#'
#' This function returns the [camera operation
#' matrix](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html)
#' as returned by `camtrapR::cameraOperation()`.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#'   1. `observations` 2. `deployments` 3. `multimedia`
#'
#'   and a list with metadata: `datapackage`
#'
#' @importFrom purrr map2_dfc
#' @importFrom dplyr %>% as_tibble filter mutate pull
#' @importFrom lubridate date
#' @return a matrix. Row names always indicate the `location_name` (station) and
#'   the `camera_id` in the form
#'   `"Station"`+`location_name`[+`"__CAM_"`+`camera_id`] if `camera_id` is specified in `deployments`. Column names are
#'   dates.
#'
#' @export
#'
get_cam_op <- function(datapkg) {
  # check data package
  check_datapkg(datapkg)

  # extract deployments
  deploys <- datapkg$deployments

  # very first day among all stations
  first_day <- min(deploys$start)
  # very last day among all stations
  last_day <- max(deploys$end)

  # get sequence with all days from very first day to very last day
  days_operations <- seq(date(first_day), date(last_day), by = "days")
  # get a string version of this: useful for setting names of final matrix
  days_operations_string <- as.character(days_operations)

  # add aux variables, start_day and end_day for each deployment
  deploys <- deploys %>%
    mutate(start_day = date(start),
           end_day = date(end))

  # make a operation table per deployment
  deployment_operational <- map(
    deploys$deployment_id,
    function(x) {
      start_day <- deploys %>% filter(.data$deployment_id == x) %>% pull(start_day)
      end_day <- deploys %>% filter(.data$deployment_id == x) %>% pull(end_day)
      operational <- days_operations >= start_day & days_operations <= end_day
      operational[operational == TRUE] <- 1
      operational <- as_tibble(operational)
      names(operational) <- x
    })

  # get for each location which days a deployment was active
  camOps <- map2_dfc(deploys$location_name,
                     deploys$camera_id,
                     function(x,y) {
                       # get deployments linked to the location



                       if (!is.na(y)) {
                         names(operational) <- paste0("Station",x,"__CAM_", y)
                       } else {
                         names(operational) <- paste0("Station",x)
                       }

                       return(operational)
  })
  # transform to matrix
  camOps <- as.matrix(camOps)
  # add names to rows (days)
  rownames(camOps) <- days_operations_string
  # transpose to get location_name as rows and days as columns and return
  t(camOps)
}
