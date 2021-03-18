#' Read camtrap-dp formatted data
#'
#' This function reads cameratrap data formatted following the [Camera Trap Data Package](https://github.com/tdwg/camtrap-dp) ([Camtrap DP](https://github.com/tdwg/camtrap-dp)) format.
#'
#'
#' @param path a path to the folder containing camera trap files
#'
#' @export
#'
#' @importFrom jsonlite read_json
#' @importFrom readr read_csv cols col_character col_number
#' @importFrom here here
#' @return a list of tibbles (dataframes)
#'
#' @examples
#' \dontrun{
#' library(here)
#' camtrap_dp_folder <- here("inst", "extdata", "gmu8-monitoring-faunabeheerzone-8-20210301093537")
#' gmu8 <- read_camtrap_dp(camtrap_dp_folder)
#'
#' library(here)
#' camtrap_dp_folder <- here("inst", "extdata", "mica-muskrat-and-coypu-20210302172233")
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_folder)
#' }
read_camtrap_dp <- function(path) {
  # add asserts to check presence of the 4 files
  # TO BE DONE

  # check uniqueness PKs (ids) to be sure no problems arise in other functions
  # TO BE DONE

  # define files
  datapackage <- "datapackage.json"
  deployments <- "deployments.csv"
  multimedia <- "multimedia.csv"
  observations <- "observations.csv"

  # read files
  datapackage <- read_json(path = here(path, datapackage))
  deployments <- read_csv(file = here(path, deployments))
  multimedia <- read_csv(
    file = here(path, multimedia),
    col_types = cols(
      comments = col_character()
    )
  )
  observations <- read_csv(
    file = here(path, observations),
    col_types = cols(
      classification_confidence = col_number()
    )
  )

  # return list
  list(
    "datapackage" = datapackage,
    "deployments" = deployments,
    "multimedia" = multimedia,
    "observations" = observations
  )
}
