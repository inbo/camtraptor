#' Estimate average animal speed
#'
#' Calculates harmonic mean and standard error of animal speed while active
#' from a data package.
#'
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param species A character string indicating species subset to analyse; user
#'   input required if NULL.
#' @return A two-element numeric vector containing mean and standard error
#'   speed while active.
#' @family density estimation functions
#' @export
fit_speedmodel <- function(package, species=NULL){
  if(is.null(species)) species <- select_species(package)
  obs <- package$data$observations %>%
    subset(scientificName==species & speed > 0.01 & speed < 10)
  if("useDeployment" %in% names(obs)) obs <- subset(obs, useDeployment)
  mn <- 1/mean(1/obs$speed, na.rm=FALSE)
  se <- mn^2 * sqrt(var(1/obs$speed, na.rm=FALSE)/nrow(obs))
  c(estimate=mn, se=se)
}
