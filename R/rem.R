#' Fit a random encounter model
#'
#' Estimates REM density given dataframes of trap rate and auxiliary 
#' parameter data
#'
#' @param data A dataframe containing a row per sampling location and columns:
#'  - observations: the number of animal contact events
#'  - effort: the amount of camera time
#'  If `stratum_areas` is provided, additional column required:
#'  - stratumID: key identifying which stratum each location sits in
#' @param param A dataframe containing REM parameter estimates with columns
#'  parameter (parameter name), estimate(parameter standard error) and se 
#'  (parameter standard error); use one row per parameter, with the following
#'  names:
#'  Mandatory
#'  - radius: effective detection radius
#'  - angle: effective detection angle
#'  - speed: average animal speed
#'  Optionally
#'  - activity: activity level (proportion of time spent active)
#'  If activity is provided, speed is assumed to be average speed while active,
#'  otherwise it is taken to be day range (distance traveled per day)
#' @param stratum_areas A dataframe with one row per stratum and columns:
#' - stratumID: stratum ID key, matched with the same key in data
#' - area: stratum areas (or proportional coverage of the study area)
#' @param reps Number of bootstrap replicates for error estimation. 
#' @return A dataframe with the original parameters plus trap rate and density
#'  estimates and standard errors.
#' @details The function makes no assumptions about units. It is up to the user to ensure 
#'  that these are harmonised across data and parameters.
#' @family density estimation functions
#' @export
rem <- function(data, param, stratum_areas=NULL, reps=999){
  
  traprate <- function(data){
    if(is.null(stratum_areas)){
      sum(data$observations) / sum(data$effort)
    } else{
      local_density <- sapply(stratum_areas$stratumID, function(stratum){
        i <- data$stratumID==stratum
        sum(data$observations[i]) / sum(data$effort[i])
      })
      sum(local_density * stratum_areas$area) / sum(stratum_areas$area)
    }
  }
  
  sampled_traprate <- function(){
    i <- if(is.null(stratum_areas)) 
      sample(1:nrow(data), replace=TRUE) else
        as.vector(sapply(stratum_areas$stratumID, function(stratum){
          sample(which(data$stratumID==stratum), replace=TRUE)
        }))
    traprate(data[i, ])
  }
  
  if(!all(c("effort", "observations") %in% names(data)))
    stop("data must contain (at least) columns effort and observations")
  if(!all(c("speed", "radius", "angle") %in% param$parameter))
    stop("param must contain (at least) parameters speed, radius and angle")
  if(!is.null(stratum_areas)){
    if(!"stratumID" %in% names(data))
      stop("data must contain column stratumID for stratified analysis")
    if(!all(c("stratumID", "area") %in% names(stratum_areas)))
      stop("stratum_areas must contain columns stratumID and area")
    if(!all(data$stratumID %in% stratum_areas$stratumID)) 
      stop("Not all strata in data are present in stratum_areas")
  }  
  
  if(!("activity" %in% param$parameter)) 
    param <- rbind(param, data.frame(parameter="activity", estimate=1, se=0))
  param <- param %>%
    dplyr::select(parameter, estimate, se) %>%
    dplyr::filter(parameter %in% c("radius", "angle", "speed", "activity"))
  add <- ifelse(param$parameter == "angle", 2, 0)
  multiplier <- pi / prod(param$estimate + add)
  
  tr_sample <- replicate(reps, sampled_traprate())
  tr <- data.frame(parameter="traprate", estimate=traprate(data), se=sd(tr_sample))
  density <- multiplier * tr$estimate
  Es <- c(tr$estimate, param$estimate + add)
  SEs <- c(tr$se, param$se)
  SE <- density * sqrt(sum((SEs/Es)^2))
  rbind(param, tr, data.frame(parameter="density", estimate=density, se=SE))
}
