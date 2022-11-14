#' Check deployment calibration models
#' 
#' Displays deployment calibration model diagnostic plots and allows 
#' users to record interactively whether each deployment is reliable.
#' 
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @return The original package with logical column `useDeployment`
#'  added to deployments and observations data.
#' @family density estimation functions
#' @export
check_deployment_models <- function(package){
  plot_folder <- file.path(package$directory, "positioning_plots")
  if(!dir.exists(plot_folder)) stop("The specified folder does not exist.")
  plot_dirs <- list.dirs(plot_folder, recursive=FALSE)
  plots <- file.path(plot_dirs, "ratio.jpeg") %>%
    lapply(jpeg::readJPEG)
  names(plots) <- basename(plot_dirs)
  
  depdat <- package$data$deployments %>%
    dplyr::select(deploymentID, locationName)
  depdat$useDeployment <- FALSE
  
  for(i in 1:nrow(depdat)){
    dep <- depdat$deploymentID[i]
    if(dep %in% names(plots)){
      img <- plots[[dep]]
      imdim <- dim(img)
      p <- ggplot() + annotation_raster(img, 1, imdim[2], 1, imdim[1]) + 
        xlim(1, imdim[2]) + ylim(1, imdim[1]) +
        theme_void() + ggtitle(depdat$locationName[i])
      print(p)
      answer <- NA
      while(is.na(answer) || !answer %in% c("y", "n"))
        answer <- readline("Use deployment? (y/n): ") %>% tolower()
      if(answer=="y") depdat$useDeployment[i] <- TRUE
    }
  }
  
  if("useDeployment" %in% names(package$data$observations))
    package$data$observations <- select(package$data$observations, -useDeployment)
  package$data$observations <- package$data$observations %>%
    left_join(depdat, by="deploymentID")
  
  package$data$deployments$useDeployment <- depdat$useDeployment
  package
}
