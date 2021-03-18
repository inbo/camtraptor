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
#' library(here)
#' camtrap_dp_folder <- here("data", "gmu8-monitoring-faunabeheerzone-8-20210301093537")
#' camtrap_dp <- read_camtrap_dp(camtrap_dp_folder)
read_camtrap_dp <- function(camtrap_dp_folder,
                            datapackage = "datapackage.json",
                            deployments = "deployments.csv",
                            multimedia = "multimedia.csv",
                            observations = "observations.csv") {
  # add asserts to check presence of the 4 files
  # TO BE DONE

  # check uniqueness PKs (ids) to be sure no problems arise in other functions
  # TO BE DONE

  # read files
  datapackage <- read_json(path = here(camtrap_dp_folder, datapackage))
  deployments <- read_csv(file = here(camtrap_dp_folder, deployments))
  multimedia <- read_csv(
    file = here(camtrap_dp_folder, multimedia),
    col_types = cols(
      comments = col_character()
    )
  )
  observations <- read_csv(
    file = here(camtrap_dp_folder, observations),
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
