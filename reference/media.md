# Get or set media

Re-export of
[`camtrapdp::media()`](https://inbo.github.io/camtrapdp/reference/media.html).

## Usage

``` r
media(x)

media(x) <- value
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

- value:

  A data frame to assign as media.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with media.

## See also

Other accessor functions:
[`contributors()`](https://inbo.github.io/camtraptor/reference/contributors.md),
[`deployments()`](https://inbo.github.io/camtraptor/reference/deployments.md),
[`events()`](https://inbo.github.io/camtraptor/reference/events.md),
[`individuals()`](https://inbo.github.io/camtraptor/reference/individuals.md),
[`locations()`](https://inbo.github.io/camtraptor/reference/locations.md),
[`observations()`](https://inbo.github.io/camtraptor/reference/observations.md),
[`taxa()`](https://inbo.github.io/camtraptor/reference/taxa.md)

## Examples

``` r
x <- example_dataset()
# Get media
media(x)
#> # A tibble: 423 × 12
#>    mediaID  deploymentID captureMethod   timestamp           filePath filePublic
#>    <chr>    <chr>        <fct>           <dttm>              <chr>    <lgl>     
#>  1 07840dcc 00a2c20d     activityDetect… 2020-05-30 02:57:37 https:/… TRUE      
#>  2 401386c7 00a2c20d     activityDetect… 2020-05-30 02:57:40 https:/… TRUE      
#>  3 ca3ff293 00a2c20d     activityDetect… 2020-05-30 02:57:40 https:/… TRUE      
#>  4 e8b8e44c 00a2c20d     activityDetect… 2020-05-30 02:57:41 https:/… TRUE      
#>  5 b8690a15 00a2c20d     activityDetect… 2020-05-30 02:57:41 https:/… TRUE      
#>  6 64734615 00a2c20d     activityDetect… 2020-05-30 02:57:42 https:/… TRUE      
#>  7 12ce5004 00a2c20d     activityDetect… 2020-05-30 02:57:43 https:/… TRUE      
#>  8 f372acaf 00a2c20d     activityDetect… 2020-05-30 02:57:43 https:/… TRUE      
#>  9 97140835 00a2c20d     activityDetect… 2020-05-30 02:57:44 https:/… TRUE      
#> 10 d1326349 00a2c20d     activityDetect… 2020-05-30 02:57:44 https:/… TRUE      
#> # ℹ 413 more rows
#> # ℹ 6 more variables: fileName <chr>, fileMediatype <chr>, exifData <chr>,
#> #   favorite <lgl>, mediaComments <chr>, eventID <chr>

# Set media (not recommended outside a function)
media(x) <- head(media(x), 1)
```
