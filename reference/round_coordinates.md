# Round coordinates to generalize camera trap locations

Rounds deployment coordinates to a certain number of digits to
fuzzy/generalize camera trap locations. This function can be used before
publishing data in order to protect sensitive species and/or prevent
theft of active cameras.

## Usage

``` r
round_coordinates(package, digits = 3)
```

## Arguments

- package:

  A Camtrap DP, as read by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- digits:

  Number of decimal places to round coordinates to (`1`, `2` or `3`).

## Value

`package` with rounded coordinates as well as updated
`coordinateUncertainty`.(in deployments) and `coordinatePrecision` (in
metadata).

## Details

Rounding coordinates is a recommended method to generalize sensitive
biodiversity information (see [Section
4.2](https://doi.org/10.15468/doc-5jp4-5g10#s-generalization) in Chapman
2020). Choose a number of digits that aligns with the sensitivity of the
data and notice the effect on precision and uncertainty. Publish the
coordinates as is (i.e. do not use this function) if the data are not
sensitive.

|             |        |                     |                                |
|-------------|--------|---------------------|--------------------------------|
| sensitivity | digits | coordinatePrecision | coordinateUncertainty          |
| high        | 1      | 0.1                 | original uncertainty + 15691 m |
| medium      | 2      | 0.01                | original uncertainty + 1570 m  |
| low         | 3      | 0.001               | original uncertainty + 157 m   |

For records with `coordinateUncertainty = NA` the function will assume
the coordinates were obtained by GPS and use `30 m` as original
uncertainty, before adding uncertainty caused by rounding. The added
uncertainty is the largest possible value caused by rounding (see [Table
3](https://doi.org/10.15468/doc-gg7h-s853#table-uncertainty) in Chapman
& Wieczorek 2020).

## See also

Other publication functions:
[`write_dwc()`](https://inbo.github.io/camtraptor/reference/write_dwc.md),
[`write_eml()`](https://inbo.github.io/camtraptor/reference/write_eml.md)

## Examples

``` r
# Round coordinates of example package to 3 digits
mica <- round_coordinates(mica, 3)

# coordinatePrecision is set in metadata
mica$coordinatePrecision
#> [1] 0.001

# coordinateUncertainty is set in data: original uncertainty (or 30) + 157 m
mica$data$deployments$coordinateUncertainty
#> [1] 187 187 187 187
```
