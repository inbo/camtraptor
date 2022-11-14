#' Fit a detection function model
#' 
#' Fits a detection function to a data package and estimates effective 
#' detection distance (EDD).
#' 
#' @param package Camera trap data package object, as returned by
#'   `read_camtrap_dp()`.
#' @param species A character string indicating species subset to analyse; user
#'   input required if NULL.
#' @return A `ddf` detection function model list, with additional element
#'   `edd`, a vector with estimated and standard error effective detection 
#'   distance, or the `newdata` dataframe with EDD estimate and se added.
#' @seealso \code{\link{Distance::ds}}
#' @family density estimation functions
#' @export
fit_detmodel <- function(package,
                         species=NULL){
  if(is.null(species)) species <- select_species(package)
  data <- package$data$observations %>%
    subset(scientificName==species) %>%
    dplyr::select(all_of(allvars)) %>%
    tidyr::drop_na() %>%
    as.data.frame()
  if("useDeployment" %in% names(data)) data <- subset(data, useDeployment)
  
}

