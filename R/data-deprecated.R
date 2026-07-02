#' Deprecated datasets in camtraptor
#'
#' `r lifecycle::badge("deprecated")`
#' -   The `mica` dataset was a Camtrap DP 0.1.6 data package and it is not
#' supported anymore. Replaced by [example_dataset()].
#' - The sample of animal position digitization data `animal_pos` is deprecated.
#' Use [`animal_positions`] instead. The column `"sequenceID"` has been renamed to `"eventID"`.
#' - The sample of deployment calibration models `calib_models` is deprecated.
#' Use [`calibration_models`] instead.
#' 
#' @name mica
#' @family sample data
#' @family read sample data functions
#' @export mica
NULL

#' @name animal_pos
#' @inherit mica title description
#' @family sample data
#' @export animal_pos
NULL

#' @name calib_models
#' @inherit mica title description
#' @family sample data
#' @export calib_models
NULL

#' Creates an active binding for the deprecated object name without throwing a
#' deprecation warning when accessed, and then returns the value of the new
#' object.
#' @noRd
deprecated_object <- function(name, new_name, ns) {
  pkgname <- getNamespaceName(ns)
  
  if (exists(name, envir = ns, inherits = FALSE) && !bindingIsActive(name, ns)) {
    rm(list = name, envir = ns)
  }
  if (!exists(name, envir = ns, inherits = FALSE) || !bindingIsActive(name, ns)) {
    makeActiveBinding(name, function() {
      target <- tryCatch(
        getExportedValue(pkgname, new_name),
        error = function(e) NULL
      )
      if (is.function(target)) target() else target
    }, ns)
  }
}

#' Deprecates objects in the package namespace
#' @noRd
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  deprecated_object("mica", "example_dataset", ns)
  deprecated_object("animal_pos", "animal_positions", ns)
  deprecated_object("calib_models", "calibration_models", ns)
}
