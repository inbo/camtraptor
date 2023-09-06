#' Sample of Camtrap DP formatted data
#'
#' A [Camera Trap Data Package](https://camtrap-dp.tdwg.org) as read by 
#' [read_camtrap_dp()].
#' The source data are derived from the [Camtrap DP example dataset
#' ](https://camtrap-dp.tdwg.org/example/).
#'
#' @family sample data
#' @source
#' <https://camtrap-dp.tdwg.org/example/>
#' @examples
#' \dontrun{
#' # mica.rda was created with:
#' mica <- read_camtrap_dp(
#'   "https://raw.githubusercontent.com/tdwg/camtrap-dp/b5c32179a9f072781386ca02e255e442a70902fa/example/datapackage.json"
#' )
#' save(mica, file = "data/mica.rda")
#' }
"mica"

#' Sample of animal position digitization data
#'
#' A tibble data frame with the following columns:
#' - `deploymentID`
#' - `sequenceID`
#' - `x` and `y`: The coordinates.
#' - `imageWidth` and `imageHeight`: The image dimensions.
#'
#' @family sample data
"animal_positions"

#' Sample of deployment calibration models
#'
#' A list containing a number of calibration models (`calibs`) or site
#' calibration models (`depcal`).
#' The deployment names are used as names.
#'
#' @family sample data
"dep_calib_models"
