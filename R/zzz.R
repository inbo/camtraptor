#' Print list of options
#'
#' @param regex Character. A regular expression to parse.
#' @param ... Additional arguments passed to the collapse.
#'
#' @noRd
#'
#' @importFrom glue glue_collapse
#'
#' @keywords internal
collapse_transformer <- function(regex = "[*]$", ...) {
  function(code, envir) {
    if (grepl(regex, code)) {
      code <- sub(regex, "", code)
    }
    res <- eval(parse(text = code), envir)
    glue_collapse(res, ...)
  }
}

#' Custom label format function
#'
#' Add "+" to last label of legend while using absolute scale. At the moment
#' only numeric scale is needed and therefore implemented.
#'
#' @source based on leaflet's
#'   [labelFormat()](https://github.com/rstudio/leaflet/commit/bb3ab964486b357ddc160a7032cfdce6cd8fbe35)
#'    function
#'
#' @param max_color_scale a number indicating the maximum value of the absolute
#'   color scale (`NULL` if relative scale is used, default)
#' @param prefix a prefix of legend labels
#' @param suffix a suffix of legend labels
#' @param digits the number of digits of numeric values in labels
#' @param big.mark the thousand separator
#' @param transform a function to transform the label value
#'
#' @noRd
#'
#' @keywords internal
labelFormat_scale <- function(max_color_scale = NULL,
                              prefix = "",
                              suffix = "",
                              digits = 3,
                              big.mark = ",",
                              transform = identity) {

  formatNum <- function(x, max_color_scale) {
    cuts_chrs <- format(round(transform(x), digits),
                       trim = TRUE,
                       scientific = FALSE,
                       big.mark = big.mark)
    if (!is.null(max_color_scale)) {
      n = length(x)
      if (x[n] == max_color_scale) {
        cuts_chrs[n] <- paste0(cuts_chrs[n], "+")
      }
    }
    return(cuts_chrs)
  }

  function(type, ...) {
    switch(
      type,
      numeric = (function(cuts) {
        paste0(prefix, formatNum(cuts, max_color_scale), suffix)
      })(...)
    )
  }
}
