#' Fit detection model
#'
#' Fits a detection model to distance or angle data and adds predicted
#' effective detection distance (EDD) to the model output.
#'
#' @param formula A two sided formula relating distance or angle data 
#'   to covariates.
#' @param data A dataframe containing the variables in the model.
#' @param newdata A dataframe containing covariate values at which to predict
#'   effective detection distance. If `NULL` and no covariates 
#'   (`formula=variable~1`), overall mean EDD is estimated; if `NULL` and 
#'   covariates, EDD estimated at mean continuous covariate values and levels
#'   of factors.
#' @param ... Additional parameters passed to `ds` function. 
#' @return A `ddf` detection function model list, with additional element
#'   `edd`, a vector with estimated and standard error effective detection 
#'   distance, or the `newdata` dataframe with EDD estimate and se added.
#' @seealso \code{\link{Distance::ds}}
#' @family density estimation functions
#' @export
fitdf <- function(formula,
                  data,
                  newdata=NULL,
                  ...){
  
  # get and check model variables
  allvars <- all.vars(formula)
  depvar <- allvars[1]
  covars <- tail(allvars, -1)
  if(!all(allvars %in% names(data))) stop("Can't find all model variables in data")
  if("distance" %in% covars) stop("Cannot use \"distance\" as a covariate name - rename and try again")
  
  classes <- dplyr::summarise_all(data, class)
  if(classes[depvar]=="numeric"){
    data <- data %>%
      dplyr::rename(distance=all_of(depvar)) %>%
      dplyr::mutate(distance=abs(distance))
  } else{
    cats <- strsplit(as.character(dplyr::pull(data, depvar)), "-")
    data$distbegin <- unlist(lapply(cats, function(x) as.numeric(x[1])))
    data$distend <- unlist(lapply(cats, function(x) as.numeric(x[2])))
    data$distance <- (data$distbegin + data$distend) / 2
  }
  
  # model fitting
  args <- c(data=list(data), formula=formula[-2], list(...))
  mod <- suppressWarnings(suppressMessages(do.call(ds, args)$ddf))
  
  # esw prediction
  if(length(covars)==0) 
    newdata <- data.frame(x=0) else{
      if(is.null(newdata)){
        newdata <- data %>% dplyr::select(all_of(covars)) %>%
          lapply(function(x) 
            if(is.numeric(x)) mean(x, na.rm=T) else sort(unique(x)))  %>%
          expand.grid()
      } else{
        if(!all(covars %in% names(newdata))) stop("Can't find all model covariates in newdata")
      }}
  prdn <- predict(mod, newdata, esw=TRUE, se.fit=TRUE)
  if(mod$meta.data$point){
    prdn$se.fit <- 0.5 * prdn$se.fit / (pi * prdn$fitted)^0.5
    prdn$fitted <- sqrt(prdn$fitted/pi)
  }
  ed <- cbind(estimate=prdn$fitted, se=prdn$se.fit)
  if(length(covars)>=1) ed <- cbind(newdata, ed)
  mod$edd <- ed
  mod
}
