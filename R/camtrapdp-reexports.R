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