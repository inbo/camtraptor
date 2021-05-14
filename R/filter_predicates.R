#' Filter predicate
#'
#' @name filter_predicate
#' @param key (character) the key for the predicate. See "Keys" below
#' @param value (various) the value for the predicate
#' @param ...,.list For `pred_or()` or `pred_and()`, one or more objects of
#' class `filter_predicate`, created by any `pred*` function
#' @importFrom glue glue double_quote
#' @importFrom lubridate is.POSIXct
#' @importFrom purrr map map_chr
#'
#' @return a predicate object. An object of class predicate is a list with the
#'   following slots:
#'   1. `arg`: a (list of) character with all arguments in the predicate(s)
#'   2. `value`:  a (list of) character with all values in the predicate(s)
#'   3. `type`:  a (list of) character with all predicate types, see section
#'   "predicate methods" here below
#'   4. `expr`: a character: body of a filter expression
#'
#' @section Predicate methods and their equivalent types:
#'
#' `pred*` functions are named for the 'type' of operation they do, inspired  by
#' GBIF [occurrence
#' predicates](https://www.gbif.org/developer/occurrence#predicates)
#'
#' The following functions take one key and one value and are associated to the
#' following types:
#' - `pred`: equals
#' - `pred_lt`: lessThan
#' - `pred_lte`: lessThanOrEquals
#' - `pred_gt`: greaterThan
#' - `pred_gte`: greaterThanOrEquals
#' - `pred_like`: like (NOT IMPLEMENTED YET!)
#'
#' The following function is only for geospatial queries, and only
#' accepts a WKT string:
#' - `pred_within`: within (NOT IMPLEMENTED YET!)
#'
#' The following functions are only for stating that you do (not) want
#' a key to be NA, so only accepts one key:
#' - `pred_na`: isNA
#' - `pred_notna`: isNotNA
#'
#' The following two functions accept multiple individual filter predicates,
#' separating them by either "and" or "or":
#' - `pred_and`: and
#' - `pred_or`: or
#'
#' The not predicate accepts one predicate; that is, this negates whatever
#' predicate is passed in, e.g., not the location "B_ML_val 05_molenkreek":
#' - `pred_not`: not
#'
#' The following function is special in that it accepts a single key but many
#' values, stating that you want to search for all the listed values, e.g.
#' one of the locations in: "B_ML_val 05_molenkreek", "B_ML_val 03_De Val" and
#' "B_ML_val 06_Oostpolderkreek"
#' - `pred_in`: in
#'
#' @section What happens internally:
#'
#' Internally, the input to `pred*` functions turn into a character string,
#' which forms the body of a filter expression.
#' For example:
#'
#' `pred("tags", "boven de stroom")` gives:
#'
#' ```
#' $arg
#' [1] "tags"
#'
#' $value
#' [1] "boven de stroom"
#'
#' $type
#' [1] "equals"
#'
#' $expr
#' (tags == "boven de stroom")
#'
#' ```
#'
#' `pred_gt("latitude", 51.27)` gives, (only `expr` slot shown):
#'
#' ```
#' (latitude > 51.27)
#' ```
#'
#' `pred_or()` gives:
#'
#' ```
#' ((tags == "boven de stroom") | (latitude > 51.28))
#' ```
#'
#' `pred_or()` gives:
#'
#' ```
#' ((tags == "boven de stroom") & (latitude > 51.28))
#' ```
#'
#' @section Keys:
#'
#' Acceptable arguments to the `key` parameter are the column names of the
#' data.frame you are applying the filter predicates.
#'
#' @family get_functions
#' @examples
#' # one arg one value predicates
#' pred("scientific_name", "Anas platyrhynchos")
#' pred("tags", "boven de stroom")
#' pred_gt("latitude", 51.28)
#' pred_gte("latitude", 51.28)
#' pred_lt("longitude", 3.59)
#' pred_lte("longitude", 3.59)
#' pred_not("location_name", "B_ML_val 03_De Val")
#'
#' # and, or predicates
#' pred_and(pred_lt("longitude", 3.59),pred_gt("latitude", 51.28))
#' pred_or(pred_gte("count", 2), pred("vernacular_name", "Norway Rat"))
#'
#' # use Dates as argument
#' start_date <- as.Date("2020-06-03", format = "%Y-%m-%d")
#' end_date <- as.Date("2020-06-10", format = "%Y-%m-%d")
#' pred_or(pred_gte("start", start_date), pred_lte("end", end_date))
#'
#' # use datetimes (POSIXct) as argument
#' start_date <- lubridate::as_datetime("2020-06-03")
#' end_date <- lubridate::as_datetime("2020-06-10")
#' pred_or(pred_gte("start", start_date), pred_lte("end", end_date))
#'
#' # one arg multiple values predicates
#' locations <- c("B_ML_val 03_De Val", "B_ML_val 05_molenkreek")
#' pred_in("location_name", locations)
#' pred_notin("location_name", locations)
#' start_dates <- as_datetime(c("2020-06-03 20:10:18","2020-06-03 20:04:33"))
#' pred_in("start", start_dates)
#' pred_notin("start", start_dates)
#'
#' # one arg, no value predicates
#' pred_na("scientific_name")
#' pred_notna("scientific_name")
#'
#' @export
pred_primitive <- function(arg, value, symbol, type) {
  # checks
  check_filter_arg_value(arg, value)
  check_filter_value(value)
  check_filter_symbol(symbol)
  check_filter_type(type)
  # build predicate object
  predicate <- list(arg = arg, value = value, type = type)
  # build expr
  if (any(is.POSIXct(value), class(value) == "Date")) {
    value <- double_quote(value)
    predicate$expr <- glue("({arg} {symbol} as_datetime({value}))")
  } else {
    if (is.character(value)){
      value <- double_quote(value)
    }
    predicate$expr <- glue("({arg} {symbol} {value})")
  }
  return(structure(predicate, class = "filter_predicate"))
}
#' @rdname filter_predicate
#' @export
pred <- function(arg, value) {
  pred_primitive(arg, value, symbol = "==", type = "equals")
}

#' @rdname filter_predicate
#' @export
pred_not <- function(arg, value) {
  pred_primitive(arg, value, symbol = "!=", type = "notEquals")
}

#' @rdname filter_predicate
#' @export
pred_gt <- function(arg, value) {
  pred_primitive(arg, value, symbol = ">", type = "greaterThan")
}

#' @rdname filter_predicate
#' @export
pred_gte <- function(arg, value) {
  pred_primitive(arg, value, symbol = ">=", type = "greaterThanOrEquals")
}
#' @rdname filter_predicate
#' @export
pred_lt <- function(arg, value) {
  pred_primitive(arg, value, symbol = "<", type = "lessThan")
}
#' @rdname filter_predicate
#' @export
pred_lte <- function(arg, value) {
  pred_primitive(arg, value, symbol = "<=", type = "lessThanOrEquals")
}
#' @rdname filter_predicate
#' @importFrom glue glue double_quote glue_collapse
#' @export
pred_in <- function(arg, value) {
  # check arg and value
  check_filter_arg(arg)
  check_filter_value_type(value)
  # build predicate object
  predicate <- list(arg = arg, value = value, type = "in")
  # build expr
  if (any(all(is.POSIXct(value)), class(value) == "Date")) {
    value <- double_quote(value)
    if (length(value) > 0) {
      predicate$expr <- glue(
        "({arg} %in% as_datetime(c(",
        glue_collapse(value, sep = ","),
        ")))"
      )
    } else {
      predicate$expr <- glue("({arg} %in% as_datetime(character(0)))")
    }
  } else {
    if (is.character(value)){
      value <- double_quote(value)
    }
    if (length(value) > 0) {
      predicate$expr <- glue(
        "({arg} %in% c(",
        glue_collapse(value, sep = ","),
        "))"
      )
    } else {
      predicate$expr <- glue("({arg} %in% character(0))")
    }
  }
  return(structure(predicate, class = "filter_predicate"))
}
#' @rdname filter_predicate
#' @importFrom glue glue
#' @export
pred_notin <- function(arg, value) {
  # build predicate object starting from the "in" predicate
  predicate <- pred_in(arg, value)
  # set right type
  predicate$type <- "notIn"
  # add negation to expr
  predicate$expr <- glue("(!", predicate$expr, ")")
  return(structure(predicate, class = "filter_predicate"))
}
#' @rdname filter_predicate
#' @importFrom glue glue
#' @export
pred_na <- function(arg) {
  # check arg
  check_filter_arg(arg)
  # build predicate object
  predicate <- list(arg = arg, value = NA, type = "na")
  # build expr
  predicate$expr <- glue("(is.na({arg}))")
  return(structure(predicate, class = "filter_predicate"))
}
#' @rdname filter_predicate
#' @importFrom glue glue
#' @export
pred_notna <- function(arg) {
  # build predicate object
  predicate <- list(arg = arg, value = NA, type = "notNa")
  # add negation to expr
  predicate$expr <- glue("(!is.na({arg}))")
  return(structure(predicate, class = "filter_predicate"))
}
#' @importFrom purrr map map_chr
#' @importFrom glue glue glue_collapse
#' @export
pred_and_or_primitive <- function(symbol, ...) {
  preds <- list(...)
  # build predicate object
  predicate <- list(
    arg = map(preds,  ~.[["arg"]]),
    value = map(preds,  ~.[["value"]]),
    type = map(preds,  ~.[["type"]])
  )
  # build expr
  filter_expr <- map_chr(preds, ~.[["expr"]])
  filter_expr <- glue_collapse(filter_expr, sep = symbol)
  filter_expr <- glue("(", filter_expr, ")")
  # add expr to predicate
  predicate$expr <- filter_expr
  return(structure(predicate, class = "filter_predicate"))
}
#' @rdname filter_predicate
#' @export
pred_and <- function(...) {
  pred_and_or_primitive(symbol = " & ", ...)
}
#' @rdname filter_predicate
#' @export
pred_or <- function(...) {
  pred_and_or_primitive(symbol = " | ", ...)
}
#' @importFrom glue glue
#' @export
apply_filter_predicate <- function(df, verbose, ...) {
  preds <- list(...)
  if (length(preds) > 0) {
    filters <- pred_and(...)
    arg <- unlist(filters$arg)
    # check that all arg values are valid column names in df
    check_value(arg = arg,
                options = names(df),
                null_allowed = FALSE,
                arg_name = "predicate's arg")
    filter_expr <- glue("df %>% filter", filters$expr)
    if (verbose == TRUE) message(filter_expr)
    eval(parse(text = filter_expr))
  } else df
}

# helpers
check_filter_arg_value <- function(arg, value) {
  check_filter_arg(arg)
  check_filter_value(value)
}
#' @importFrom assertthat assert_that
check_filter_arg <- function(arg) {
  # check arg
  assert_that(is.character(arg), msg = "'arg' must be a character")
  assert_that(length(arg) == 1, msg = "'arg' must be length 1")
}
#' @importFrom assertthat assert_that
check_filter_value_type <- function(value) {
  # check value
  assert_that(
    any(is.character(value),
        is.numeric(value),
        class(value) == "Date",
        is.POSIXct(value)),
    msg = "'value' must be a character, a number, a date or a datetime(POSIXct)"
  )
}
#' @importFrom assertthat assert_that
check_filter_value_length <- function(value) {
  assert_that(length(value) == 1, msg = "'value' must be length 1")
}
check_filter_value <- function(value) {
  check_filter_value_type(value)
  check_filter_value_length(value)
}
#' @importFrom assertthat assert_that
check_filter_symbol <- function(symbol) {
  # check symbol
  assert_that(is.character(symbol), msg = "'symbol' must be a character")
  assert_that(length(symbol) == 1, msg = "'symbol' must be length 1")
}
#' @importFrom assertthat assert_that
check_filter_type <- function(type) {
  # check type
  assert_that(is.character(type), msg = "'type' must be a character")
  assert_that(length(type) == 1, msg = "'type' must be length 1")
}
