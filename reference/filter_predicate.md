# Filter predicate

Filter predicate

## Usage

``` r
pred(arg, value)

pred_not(arg, value)

pred_gt(arg, value)

pred_gte(arg, value)

pred_lt(arg, value)

pred_lte(arg, value)

pred_in(arg, value)

pred_notin(arg, value)

pred_na(arg)

pred_notna(arg)

pred_and(...)

pred_or(...)
```

## Arguments

- arg:

  (character) The key for the predicate. See "Keys" below.

- value:

  (various) The value for the predicate.

- ...:

  For `pred_or()` or `pred_and()`: one or more objects of class
  `filter_predicate`, created by any other `pred*` function.

## Value

A predicate object. An object of class predicate is a list with the
following elements:

- `arg`: A (list of) character with all arguments in the predicate(s).

- `value`: A (list of) character with all values in the predicate(s).

- `type`: A (list of) character with all predicate types, see section
  "predicate methods" here below.

- `expr`: A character: body of a filter expression.

## Predicate methods and their equivalent types

`pred*` functions are named for the 'type' of operation they do,
inspired by GBIF [occurrence
predicates](https://www.gbif.org/developer/occurrence#predicates)

The following functions take one key and one value and are associated to
the following types:

- `pred`: equals

- `pred_not`: notEquals

- `pred_lt`: lessThan

- `pred_lte`: lessThanOrEquals

- `pred_gt`: greaterThan

- `pred_gte`: greaterThanOrEquals

- `pred_like`: like (NOT IMPLEMENTED YET!)

The following function is only for geospatial queries, and only accepts
a WKT string:

- `pred_within`: within (NOT IMPLEMENTED YET!)

The following functions are only for stating that you do (not) want a
key to be `NA`, so only accepts one key:

- `pred_na`: isNA

- `pred_notna`: isNotNA

The following two functions accept multiple individual filter
predicates, separating them by either "and" or "or":

- `pred_and`: and

- `pred_or`: or

The following function is special in that it accepts a single key but
many values, stating that you want to search for all the listed values,
e.g. one of the locations in: "B_ML_val 05_molenkreek", "B_ML_val 03_De
Val" and "B_ML_val 06_Oostpolderkreek"

- `pred_in`: in

- `pred_notin`: notIn

## What happens internally

Internally, the input to `pred*` functions turn into a character string,
which forms the body of a filter expression. For example:

`pred("tags", "boven de stroom")` gives:

    $arg
    [1] "tags"

    $value
    [1] "boven de stroom"

    $type
    [1] "equals"

    $expr
    (tags == "boven de stroom")

`pred_gt("latitude", 51.27)` gives, (only `expr` element shown):

    (latitude > 51.27)

`pred_or()` gives:

    ((tags == "boven de stroom") | (latitude > 51.28))

`pred_or()` gives:

    ((tags == "boven de stroom") & (latitude > 51.28))

## Keys

Acceptable arguments to the `key` parameter are the column names of the
data frame you are applying the filter predicates.

## See also

Other filter functions:
[`apply_filter_predicate()`](https://inbo.github.io/camtraptor/reference/apply_filter_predicate.md)

## Examples

``` r
# One arg one value predicates
pred("scientificName", "Anas platyrhynchos")
#> $arg
#> [1] "scientificName"
#> 
#> $value
#> [1] "Anas platyrhynchos"
#> 
#> $type
#> [1] "equals"
#> 
#> $expr
#> (scientificName == "Anas platyrhynchos")
#> 
#> attr(,"class")
#> [1] "filter_predicate"
pred("tags", "boven de stroom")
#> $arg
#> [1] "tags"
#> 
#> $value
#> [1] "boven de stroom"
#> 
#> $type
#> [1] "equals"
#> 
#> $expr
#> (tags == "boven de stroom")
#> 
#> attr(,"class")
#> [1] "filter_predicate"
pred_gt("latitude", 51.18)
#> $arg
#> [1] "latitude"
#> 
#> $value
#> [1] 51.18
#> 
#> $type
#> [1] "greaterThan"
#> 
#> $expr
#> (latitude > 51.18)
#> 
#> attr(,"class")
#> [1] "filter_predicate"
pred_gte("latitude", 51.18)
#> $arg
#> [1] "latitude"
#> 
#> $value
#> [1] 51.18
#> 
#> $type
#> [1] "greaterThanOrEquals"
#> 
#> $expr
#> (latitude >= 51.18)
#> 
#> attr(,"class")
#> [1] "filter_predicate"
pred_lt("longitude", 3.95)
#> $arg
#> [1] "longitude"
#> 
#> $value
#> [1] 3.95
#> 
#> $type
#> [1] "lessThan"
#> 
#> $expr
#> (longitude < 3.95)
#> 
#> attr(,"class")
#> [1] "filter_predicate"
pred_lte("longitude", 3.95)
#> $arg
#> [1] "longitude"
#> 
#> $value
#> [1] 3.95
#> 
#> $type
#> [1] "lessThanOrEquals"
#> 
#> $expr
#> (longitude <= 3.95)
#> 
#> attr(,"class")
#> [1] "filter_predicate"
pred_not("locationName", "B_DL_val 3_dikke boom")
#> $arg
#> [1] "locationName"
#> 
#> $value
#> [1] "B_DL_val 3_dikke boom"
#> 
#> $type
#> [1] "notEquals"
#> 
#> $expr
#> (locationName != "B_DL_val 3_dikke boom")
#> 
#> attr(,"class")
#> [1] "filter_predicate"

# and/or predicates
pred_and(pred_lt("longitude", 3.59), pred_gt("latitude", 51.28))
#> $arg
#> $arg[[1]]
#> [1] "longitude"
#> 
#> $arg[[2]]
#> [1] "latitude"
#> 
#> 
#> $value
#> $value[[1]]
#> [1] 3.59
#> 
#> $value[[2]]
#> [1] 51.28
#> 
#> 
#> $type
#> $type[[1]]
#> [1] "lessThan"
#> 
#> $type[[2]]
#> [1] "greaterThan"
#> 
#> 
#> $expr
#> ((longitude < 3.59) & (latitude > 51.28))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
pred_or(pred_gte("count", 2), pred("vernacular_name", "Norway Rat"))
#> $arg
#> $arg[[1]]
#> [1] "count"
#> 
#> $arg[[2]]
#> [1] "vernacular_name"
#> 
#> 
#> $value
#> $value[[1]]
#> [1] 2
#> 
#> $value[[2]]
#> [1] "Norway Rat"
#> 
#> 
#> $type
#> $type[[1]]
#> [1] "greaterThanOrEquals"
#> 
#> $type[[2]]
#> [1] "equals"
#> 
#> 
#> $expr
#> ((count >= 2) | (vernacular_name == "Norway Rat"))
#> 
#> attr(,"class")
#> [1] "filter_predicate"

# Use dates as argument
start_date <- as.Date("2020-06-03", format = "%Y-%m-%d")
end_date <- as.Date("2020-06-10", format = "%Y-%m-%d")
pred_or(pred_gte("start", start_date), pred_lte("end", end_date))
#> $arg
#> $arg[[1]]
#> [1] "start"
#> 
#> $arg[[2]]
#> [1] "end"
#> 
#> 
#> $value
#> $value[[1]]
#> [1] "2020-06-03"
#> 
#> $value[[2]]
#> [1] "2020-06-10"
#> 
#> 
#> $type
#> $type[[1]]
#> [1] "greaterThanOrEquals"
#> 
#> $type[[2]]
#> [1] "lessThanOrEquals"
#> 
#> 
#> $expr
#> ((start >= lubridate::as_datetime("2020-06-03")) | (end <= lubridate::as_datetime("2020-06-10")))
#> 
#> attr(,"class")
#> [1] "filter_predicate"

# Use datetimes (POSIXct) as argument
start_date <- lubridate::as_datetime("2020-06-03")
end_date <- lubridate::as_datetime("2020-06-10")
pred_or(pred_gte("start", start_date), pred_lte("end", end_date))
#> $arg
#> $arg[[1]]
#> [1] "start"
#> 
#> $arg[[2]]
#> [1] "end"
#> 
#> 
#> $value
#> $value[[1]]
#> [1] "2020-06-03 UTC"
#> 
#> $value[[2]]
#> [1] "2020-06-10 UTC"
#> 
#> 
#> $type
#> $type[[1]]
#> [1] "greaterThanOrEquals"
#> 
#> $type[[2]]
#> [1] "lessThanOrEquals"
#> 
#> 
#> $expr
#> ((start >= lubridate::as_datetime("2020-06-03")) | (end <= lubridate::as_datetime("2020-06-10")))
#> 
#> attr(,"class")
#> [1] "filter_predicate"

# One arg multiple values predicates
locations <- c("B_ML_val 03_De Val", "B_ML_val 05_molenkreek")
pred_in("location_name", locations)
#> $arg
#> [1] "location_name"
#> 
#> $value
#> [1] "B_ML_val 03_De Val"     "B_ML_val 05_molenkreek"
#> 
#> $type
#> [1] "in"
#> 
#> $expr
#> (location_name %in% c("B_ML_val 03_De Val","B_ML_val 05_molenkreek"))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
pred_notin("location_name", locations)
#> $arg
#> [1] "location_name"
#> 
#> $value
#> [1] "B_ML_val 03_De Val"     "B_ML_val 05_molenkreek"
#> 
#> $type
#> [1] "notIn"
#> 
#> $expr
#> (!(location_name %in% c("B_ML_val 03_De Val","B_ML_val 05_molenkreek")))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
start_dates <- lubridate::as_datetime(c("2020-06-03 20:10:18", "2020-06-03 20:04:33"))
pred_in("start", start_dates)
#> $arg
#> [1] "start"
#> 
#> $value
#> [1] "2020-06-03 20:10:18 UTC" "2020-06-03 20:04:33 UTC"
#> 
#> $type
#> [1] "in"
#> 
#> $expr
#> (start %in% as_datetime(c("2020-06-03 20:10:18","2020-06-03 20:04:33")))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
pred_notin("start", start_dates)
#> $arg
#> [1] "start"
#> 
#> $value
#> [1] "2020-06-03 20:10:18 UTC" "2020-06-03 20:04:33 UTC"
#> 
#> $type
#> [1] "notIn"
#> 
#> $expr
#> (!(start %in% as_datetime(c("2020-06-03 20:10:18","2020-06-03 20:04:33"))))
#> 
#> attr(,"class")
#> [1] "filter_predicate"

# One arg, no value predicates
pred_na("scientificName")
#> $arg
#> [1] "scientificName"
#> 
#> $value
#> [1] NA
#> 
#> $type
#> [1] "na"
#> 
#> $expr
#> (is.na(scientificName))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
pred_notna("scientificName")
#> $arg
#> [1] "scientificName"
#> 
#> $value
#> [1] NA
#> 
#> $type
#> [1] "notNa"
#> 
#> $expr
#> (!is.na(scientificName))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
```
