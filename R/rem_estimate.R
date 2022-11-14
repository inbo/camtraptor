#' Integrated random encounter model density estimate
#' 
#' Estimates animal density for a given species given a camtrap DP datapackage.
#' Models for detection radius and angle, speed and activity level can be 
#' fitted externally and provided as arguments, or are fitted internally if not 
#' provided. Input units are assumed to be distance in m and time in seconds.
#' 
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param check_deployments Logical indicating whether to check deployment
#' calibration model diagnostic plots. If `TRUE` (default) runs 
#' `check_deployment_models`; radius, angle and speed data from any excluded 
#' deployments are then dropped from analysis. If `FALSE` all data are used.
#' @param activity_model An activity model fitted using `activity::fitact` or
#' `fit_actmodel`; fitted internally if `NULL`.
#' @param radius_model A detection function model for radii fitted using 
#' `fitdf` or `fit_detmodel` with argument `transect="point"`; fitted 
#' internally if `NULL`.
#' @param angle_model A detection function model for angles fitted using 
#' `fitdf` or `fit_detmodel`; fitted internally if `NULL`.
#' @param speed_model A named vector with elements `estimate` and `se`
#' (giving mean and standard error of speed), as derived from `fit_speedmodel`; 
#' fitted internally if `NULL`.
#' @param species A character string indicating species subset to analyse; user
#'   input required if NULL.
#' @param reps Number of bootstrap replicates for error estimation. 
#' @return A dataframe with .
#' @seealso \code{\link{Distance::ds}}
#' @family density estimation functions
#' @export
rem_estimate <- function(package,
                         check_deployments=TRUE,
                         activity_model=NULL,
                         radius_model=NULL,
                         angle_model=NULL,
                         speed_model=NULL,
                         species=NULL,
                         reps=999){
  
  if(check_deployments) package <- check_deployment_models(package)
  if(is.null(species)) species <- select_species(package)
  
  if(is.null(activity_model)) 
    activity_model <- fit_actmodel(package, species, reps)
  
  if(is.null(radius_model)) 
    radius_model <- fit_detmodel(distance~1, package, species,
                                 transect="point", order=0, truncation=10)
  
  if(is.null(angle_model))
    angle_model <- fit_detmodel(angle~1, package, species, order=0)
  
  if(is.null(speed_model))
    speed_model <- fit_speedmodel(package, species)
  
  data <- data.frame(
    observations = get_n_individuals(package, species=species)$n,
    effort = get_effort(package, unit="second")$effort) %>%
    suppressMessages()
  param <- data.frame(parameter=c("radius", "angle", "speed", "activity"),
                      rbind(radius_model$edd, 
                            angle_model$edd * 2,
                            speed_model, 
                            activity_model@act[1:2]))
  rownames(param) <- NULL
  res <- rem(data, param) %>%
    dplyr::mutate(estimate = estimate * c(1, 180/pi, 1, 1, 86400, 1e6),
                  se = se * c(1, 180/pi, 1, 1, 86400, 1e6))
  res$'%cv' <- 100 * res$se / res$estimate
  res$unit = c("m", "deg", "m/s", "none", "n/d", "n/km2")
  res
}
