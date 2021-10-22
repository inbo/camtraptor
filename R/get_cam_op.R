#' Get camera operation matrix
#'
#' This function returns the [camera operation matrix](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html)
#' as returned by camtrapR's function `cameraOperation()`.
#' 
#' The deployment data are grouped by `location_name` (station ID in camtrapR
#' jargon). If multiple deploymnts are linked to same location, daily
#' efforts higher than one occur.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param ... filter predicates for filtering on deployments
#' @importFrom purrr map_dfc
#' @importFrom dplyr %>% .data as_tibble filter mutate pull bind_cols
#' @importFrom lubridate as_datetime date
#' @return a matrix. Row names always indicate the `location_name` (station ID)
#'   `"Station"`+`location_name`. Column names are dates.
#'
#' @export
#' @examples 
#' get_cam_op(camtrapdp)
#' # applying filter(s) on deployments, e.g. deployments with latitude >= 51.28
#' get_cam_op(camtrapdp, pred_gte("latitude", 51.28))
get_cam_op <- function(datapkg, ...) {
  # check data package
  check_datapkg(datapkg)

  # extract and apply filtering on deployments
  deploys <- apply_filter_predicate(
    df = datapkg$deployments,
    verbose = TRUE,
    ...)
  
  # very first day among all stations
  first_day <- min(deploys$start)
  # very last day among all stations
  last_day <- max(deploys$end)

  # get sequence with all days from very first day to very last day
  days_operations <- seq(date(first_day), date(last_day), by = "days")
  # get a string version of this: useful for setting names of final matrix
  days_operations_string <- as.character(days_operations)
  # convert to datetime as it helps while operating with "+" and "-"
  days_operations <- as_datetime(days_operations)
  # add aux variables, start_day and end_day for each deployment
  deploys <- deploys %>%
    mutate(start_day = date(.data$start),
           end_day = date(.data$end))

  # make a operation table per deployment
  deployment_operational <- map(
    deploys$deployment_id,
    function(x) {
      start_day <- deploys %>% filter(.data$deployment_id == x) %>% pull(start_day)
      end_day <- deploys %>% filter(.data$deployment_id == x) %>% pull(end_day)
      operational <- days_operations > start_day & days_operations < end_day
      operational[operational == TRUE] <- 1
      # edge cases start and end day
      deploy_df  <- deploys %>% 
        filter(.data$deployment_id == x)
      daily_effort_start <- calc_daily_effort(deploy_df, calc_start=TRUE)
      operational[days_operations == start_day] <- daily_effort_start
      daily_effort_end <- calc_daily_effort(deploy_df, calc_end=TRUE)
      operational[days_operations == end_day] <- daily_effort_end
      operational <- as_tibble(operational)
      names(operational) <- x
      return(operational)
    })
  names(deployment_operational) <- deploys$deployment_id
  
  # get for each location which days a deployment was active
  camOps <- map_dfc(unique(deploys$location_name),
                    function(loc_name) {
                      # get deployments linked to the location name
                      deploys_id <- 
                        deploys %>%
                        filter(.data$location_name == loc_name) %>%
                        pull(.data$deployment_id)
                      # get operational dfs linked to these deployment_ids
                      dep_dfs <- deployment_operational[names(deployment_operational) %in% deploys_id]
                      dep_op <- bind_cols(dep_dfs)
                      # sum daily effort along all deployments at same location
                      dep_op <- as_tibble(rowSums(dep_op[, names(dep_op)]))
                      # set location name as station id
                      names(dep_op) <- paste0("Station",loc_name)
                      dep_op[[paste0("Station",loc_name)]] <- as.numeric(dep_op[[paste0("Station",loc_name)]])
                      return(dep_op)
                    })
  # transform to matrix
  camOps <- as.matrix(camOps)
  # add names to rows (days)
  rownames(camOps) <- days_operations_string
  # transpose to get location_name as rows and days as columns and return
  t(camOps)
}
