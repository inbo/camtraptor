# Add deployment coordinates to observations

This function adds deployment coordinates to `observations` based on
`deploymentID`.

## Usage

``` r
add_coordinates(x)
```

## Arguments

- x:

  Camera trap data package object, as returned by
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

## Value

Camera trap data package object, where `observations` is updated by
appending two new columns: `latitude` and `longitude`

## Details

When the `latitude` and/or the `longitude` columns are already present
in `observations`, a warning is issued and the original object is
returned unchanged.

## Examples

``` r
x <- example_dataset()

# Add coordinates to observations
add_coordinates(x) %>% observations()
#> # A tibble: 549 × 34
#>    observationID deploymentID mediaID  eventID  eventStart         
#>    <chr>         <chr>        <chr>    <chr>    <dttm>             
#>  1 705e6036      00a2c20d     NA       4bb69c45 2020-05-30 02:57:37
#>  2 07840dcc_1    00a2c20d     07840dcc 4bb69c45 2020-05-30 02:57:37
#>  3 401386c7_1    00a2c20d     401386c7 4bb69c45 2020-05-30 02:57:40
#>  4 ca3ff293_1    00a2c20d     ca3ff293 4bb69c45 2020-05-30 02:57:40
#>  5 e8b8e44c_1    00a2c20d     e8b8e44c 4bb69c45 2020-05-30 02:57:41
#>  6 b8690a15_1    00a2c20d     b8690a15 4bb69c45 2020-05-30 02:57:41
#>  7 64734615_1    00a2c20d     64734615 4bb69c45 2020-05-30 02:57:42
#>  8 12ce5004_1    00a2c20d     12ce5004 4bb69c45 2020-05-30 02:57:43
#>  9 f372acaf_1    00a2c20d     f372acaf 4bb69c45 2020-05-30 02:57:43
#> 10 97140835_1    00a2c20d     97140835 4bb69c45 2020-05-30 02:57:44
#> # ℹ 539 more rows
#> # ℹ 29 more variables: eventEnd <dttm>, observationLevel <fct>,
#> #   observationType <fct>, cameraSetupType <fct>, scientificName <chr>,
#> #   count <dbl>, lifeStage <fct>, sex <fct>, behavior <chr>,
#> #   individualID <chr>, individualPositionRadius <dbl>,
#> #   individualPositionAngle <dbl>, individualSpeed <dbl>, bboxX <dbl>,
#> #   bboxY <dbl>, bboxWidth <dbl>, bboxHeight <dbl>, …
```
