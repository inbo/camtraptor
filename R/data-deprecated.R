#' Deprecated datasets in camtraptor
#'
#' - The `mica` dataset was a Camtrap DP 0.1.6 data package and it is not
#' supported anymore. Replaced by [example_dataset()].
#' - The sample of animal position digitization data `animal_pos` is deprecated.
#' Use `animal_positions` instead.
#' - The sample of deployment calibration models `calib_models` is deprecated.
#' Use `calibration_models` instead.
#' 
#' @name camtraptor-deprecated
#' @aliases mica animal_pos calib_models
#' @family sample data
#' @export mica animal_pos calib_models
NULL

#' Creates an active binding for the deprecated object name that will throw a
#' deprecation warning when accessed, and then return the value of the new
#' object.
#' @noRd
deprecated_object <- function(name, new_name, ns, pkgname) {
  if (exists(name, envir = ns, inherits = FALSE) && !bindingIsActive(name, ns)) {
    rm(list = name, envir = ns)
  }
  if (!exists(name, envir = ns, inherits = FALSE) || !bindingIsActive(name, ns)) {
    makeActiveBinding(name, function() {
      .Deprecated(new_name, package = pkgname, old = name)
      target <- getExportedValue(pkgname, new_name)
      if (is.function(target)) target() else target
    }, ns)
  }
}

#' Deprecates objects in the package namespace.
#' @noRd
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  deprecated_object("mica", "example_dataset", ns, pkgname)
  deprecated_object("animal_pos", "animal_positions", ns, pkgname)
  deprecated_object("calib_models", "calibration_models", ns, pkgname)
}
