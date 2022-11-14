#' Fit an activity model
#'
#' Fits an activity model to data package data and estimates activity 
#' level (proportion of time spent active).
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param species A character string indicating species subset to analyse; user
#'   input required if NULL.
#' @param reps Number of bootstrap replicates to run.
#' @return An `actmod` list.
#' @seealso \code{\link{activity::fitact}}
#' @family density estimation functions
#' @export
fit_actmodel <- function(package, 
                         species=NULL, 
                         reps=999){
  if(is.null(species)) species <- select_species(package)
  deps <- package$data$deployments
  obs <- package$data$observations
  if(sum(obs$scientificName==species, na.rm=TRUE)>1){ 
    obs <- left_join(obs, dplyr::select(deps, deploymentID, latitude, longitude),
                     by="deploymentID")
    suntimes <- insol::daylength(obs$latitude, obs$longitude, 
                                 insol::JD(obs$timestamp), 0)
    timeshift <- pi - mean(suntimes[, 1] + suntimes[,3]/2) * pi/12
    obs$solartime <- obs %>%
      with(activity::solartime(timestamp, latitude, longitude, 0)) %>%
      .$solar %>%
      + timeshift %>%
      activity::wrap()
    obs %>%
      subset(scientificName==species) %>%
      .$solartime %>%
      activity::fitact(adj = 1.5, sample = "data", reps = reps)
  } else
    NULL
}
