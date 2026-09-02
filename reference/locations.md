# Get locations

Re-export of
[`camtrapdp::locations()`](https://inbo.github.io/camtrapdp/reference/locations.html).

## Usage

``` r
locations(x)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with the locations, containing the following columns:

- `locationID`

- `locationName`

- `latitude`

- `longitude`

- `coordinateUncertainty`

## See also

Other accessor functions:
[`contributors()`](https://inbo.github.io/camtraptor/reference/contributors.md),
[`deployments()`](https://inbo.github.io/camtraptor/reference/deployments.md),
[`events()`](https://inbo.github.io/camtraptor/reference/events.md),
[`individuals()`](https://inbo.github.io/camtraptor/reference/individuals.md),
[`media()`](https://inbo.github.io/camtraptor/reference/media.md),
[`observations()`](https://inbo.github.io/camtraptor/reference/observations.md),
[`taxa()`](https://inbo.github.io/camtraptor/reference/taxa.md)

## Examples

``` r
x <- example_dataset()
locations(x)
#> # A tibble: 4 × 5
#>   locationID locationName               latitude longitude coordinateUncertainty
#>   <chr>      <chr>                         <dbl>     <dbl>                 <dbl>
#> 1 e254a13c   B_HS_val 2_processiepark       51.5      4.77                   187
#> 2 2df5259b   B_DL_val 5_beek kleine vi…     51.2      5.66                   187
#> 3 ff1535c0   B_DL_val 3_dikke boom          51.2      5.66                   187
#> 4 ce943ced   B_DM_val 4_'t WAD              50.7      4.01                   187
```
