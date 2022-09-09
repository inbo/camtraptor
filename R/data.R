#' Sample of Camtrap DP formatted data
#'
#' A sample [Camera Trap Data Package](https://tdwg.github.io/camtrap-dp) as
#' read by [read_camtrap_dp()].
#' This sample is an extract from a larger Camera Trap Data Package saved as raw
#' data in `inst/extdata/mica_zenodo_5590881`.
#' To save memory, the `media.csv` resource of the latter Data Package is not
#' present.
#' The entire dataset can be downloaded from [Zenodo](
#' https://zenodo.org/record/5590881).
#'
#' @family sample data
#' @source
#' <https://github.com/tdwg/camtrap-dp/tree/ad0278ef86ef518dacfb306c598dce97667cfb81/example>
#' @examples
#' \dontrun{
#' # mica.rda was created with the code below.
#' mica <- read_camtrap_dp(
#'   system.file(
#'     "extdata/mica",
#'     "datapackage.json",
#'     package = "camtraptor"
#'   )
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
