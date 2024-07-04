#' @importFrom camtrapdp read_camtrapdp
#' @export
#' @family read functions
camtrapdp::read_camtrapdp

#' @importFrom camtrapdp example_dataset
#' @export
#' @family sample data functions
example_dataset <- memoise::memoise(camtrapdp::example_dataset)

#' @importFrom camtrapdp deployments
#' @export
#' @family accessor functions
camtrapdp::deployments

#' @importFrom camtrapdp media
#' @export
#' @family accessor functions
camtrapdp::media

#' @importFrom camtrapdp observations
#' @export
#' @family accessor functions
camtrapdp::observations

#' @importFrom camtrapdp events
#' @export
#' @family accessor functions
camtrapdp::events

#' @importFrom camtrapdp taxa
#' @export
#' @family accessor functions
camtrapdp::taxa

#' @importFrom camtrapdp filter_deployments
#' @export
#' @family filter functions
camtrapdp::filter_deployments

#' @importFrom camtrapdp filter_media
#' @export
#' @family filter functions
camtrapdp::filter_media

#' @importFrom camtrapdp filter_observations
#' @export
#' @family filter functions
camtrapdp::filter_observations
