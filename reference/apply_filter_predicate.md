# Intermediate function to apply filter predicates on a data frame

This function is used internally by all the `get_*()` functions to
filter on deployments.

## Usage

``` r
apply_filter_predicate(df, verbose, ...)
```

## Arguments

- df:

  Data frame we want to apply filter(s) expression(s)

- verbose:

  Show (`TRUE`) or not (`FALSE`) the filter predicate expression.

- ...:

  filter predicates to apply to `df`

## Value

A data frame.

## See also

Other filter functions:
[`pred()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)

## Examples

``` r
# and
apply_filter_predicate(
  mica$data$deployments,
  verbose = TRUE,
  pred_gte("latitude", 51.28),
  pred_lt("longitude", 3.56)
)
#> df %>% dplyr::filter((latitude >= 51.28) & (longitude < 3.56))
#> # A tibble: 0 × 24
#> # ℹ 24 variables: deploymentID <chr>, locationID <chr>, locationName <chr>,
#> #   longitude <dbl>, latitude <dbl>, coordinateUncertainty <dbl>, start <dttm>,
#> #   end <dttm>, setupBy <chr>, cameraID <chr>, cameraModel <chr>,
#> #   cameraInterval <dbl>, cameraHeight <dbl>, cameraTilt <dbl>,
#> #   cameraHeading <dbl>, timestampIssues <lgl>, baitUse <fct>, session <chr>,
#> #   array <chr>, featureType <fct>, habitat <chr>, tags <chr>, comments <chr>,
#> #   _id <chr>
# Equivalent of
apply_filter_predicate(
  mica$data$deployments,
  verbose = TRUE,
  pred_and(
    pred_gte("latitude", 51.28),
    pred_lt("longitude", 3.56)
  )
)
#> df %>% dplyr::filter(((latitude >= 51.28) & (longitude < 3.56)))
#> # A tibble: 0 × 24
#> # ℹ 24 variables: deploymentID <chr>, locationID <chr>, locationName <chr>,
#> #   longitude <dbl>, latitude <dbl>, coordinateUncertainty <dbl>, start <dttm>,
#> #   end <dttm>, setupBy <chr>, cameraID <chr>, cameraModel <chr>,
#> #   cameraInterval <dbl>, cameraHeight <dbl>, cameraTilt <dbl>,
#> #   cameraHeading <dbl>, timestampIssues <lgl>, baitUse <fct>, session <chr>,
#> #   array <chr>, featureType <fct>, habitat <chr>, tags <chr>, comments <chr>,
#> #   _id <chr>


# or
apply_filter_predicate(
  mica$data$deployments,
  verbose = TRUE,
  pred_or(
    pred_gte("latitude", 51.28),
    pred_lt("longitude", 3.56)
  )
)
#> df %>% dplyr::filter(((latitude >= 51.28) | (longitude < 3.56)))
#> # A tibble: 0 × 24
#> # ℹ 24 variables: deploymentID <chr>, locationID <chr>, locationName <chr>,
#> #   longitude <dbl>, latitude <dbl>, coordinateUncertainty <dbl>, start <dttm>,
#> #   end <dttm>, setupBy <chr>, cameraID <chr>, cameraModel <chr>,
#> #   cameraInterval <dbl>, cameraHeight <dbl>, cameraTilt <dbl>,
#> #   cameraHeading <dbl>, timestampIssues <lgl>, baitUse <fct>, session <chr>,
#> #   array <chr>, featureType <fct>, habitat <chr>, tags <chr>, comments <chr>,
#> #   _id <chr>
```
