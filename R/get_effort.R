#' Get effort
#'
#' Function to get the effort (deployment duration) per deployment.
#'
#' @param datapkg a camera trap data package object, as returned by
#'   `read_camtrap_dp()`, i.e. a list containing three data.frames:
#'
#' 1. `observations`
#' 2. `deployments`
#' 3. `multimedia`
#'
#' and a list with metadata: `datapackage`
#'
#' @importFrom dplyr .data %>% mutate %>% select
#' @importFrom lubridate as.duration
#' @export

#' @return a tibble (data.frame) with the following columns:
#' - `deployment_id` deployment unique identifier
#' - `effort`: a duration object (duration is a class from lubridate package)
#'
#' @examples
#' get_effort(camtrapdp)
#'
get_effort <- function(datapkg) {

  # get deployments
  deployments <- datapkg$deployments

  # calculate effort of deployments
  deployments %>%
    mutate(effort = as.duration(.data$end - .data$start)) %>%
    select(.data$deployment_id, .data$effort)
}
