#' Read camtrap-dp formatted data
#'
#' This function reads cameratrap data formatted following the [Camera Trap Data Package](https://github.com/tdwg/camtrap-dp) ([Camtrap DP](https://github.com/tdwg/camtrap-dp)) format.
#'
#'
#' @param path a path to the folder containing camera trap files
#' @param multimedia (logical) if `TRUE`, the default, multimedia are read and
#'   loaded into memory. If `FALSE` the multimedia file is not read. This flag
#'   can be handy in case of very large camtrap-dp data packages due to the high
#'   number of multimedia records
#'
#' @export
#'
#' @importFrom jsonlite read_json
#' @importFrom readr read_csv cols col_character col_number col_datetime
#' @importFrom here here
#' @return a list of tibbles (dataframes)
#'
#' @examples
#' \dontrun{
#' library(here)
#' # read multimedia file (default)
#' camtrap_dp_folder <- here("inst", "extdata", "mica-muskrat-and-coypu-20210302172233")
#' muskrat_coypu <- read_camtrap_dp(camtrap_dp_folder)
#'
#' # do not read multimedia file
#' camtrap_dp_folder <- here("inst", "extdata", "gmu8-monitoring-faunabeheerzone-8-20210301093537")
#' gmu8 <- read_camtrap_dp(camtrap_dp_folder, multimedia = FALSE)
#' }
read_camtrap_dp <- function(path, multimedia = TRUE) {
  # add asserts to check presence of the 4 files
  # TO BE DONE

  # check uniqueness PKs (ids) to be sure no problems arise in other functions
  # TO BE DONE

  # define files
  datapackage <- "datapackage.json"
  deployments <- "deployments.csv"
  observations <- "observations.csv"

  # read files
  datapackage <- read_json(path = here(path, datapackage))
  deployments <- read_csv(file = here(path, deployments))
  if (multimedia == TRUE) {
    multimedia <- "multimedia.csv"
    multimedia <- read_csv(
      file = here(path, multimedia),
      col_types = cols(
        comments = col_character()
      )
    )
  }
  observations <- read_csv(
    file = here(path, observations),
    col_types = cols(
      classification_confidence = col_number()
    )
  )
  # return list
  if (is.data.frame(multimedia)) {
    list(
      "datapackage" = datapackage,
      "deployments" = deployments,
      "multimedia" = multimedia,
      "observations" = observations
    )
  } else {
    list(
      "datapackage" = datapackage,
      "deployments" = deployments,
      "multimedia" = NULL,
      "observations" = observations
    )
  }
}
