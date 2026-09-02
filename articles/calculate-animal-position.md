# Calculate animal position

This vignette shows how to calculate the **radial distance and angular
position** of animals relative to a camera using
[`calculate_individual_radius_angle()`](https://inbo.github.io/camtraptor/reference/calculate_individual_radius_angle.md).

## Setup

Load package:

``` r

library(camtraptor)
#> 
#> Attaching package: 'camtraptor'
#> The following object is masked from 'package:base':
#> 
#>     contributors
```

The package includes two built-in example datasets used throughout this
vignette: `animal_positions` and `calibration_models`.

`animal_positions` is a data frame containing digitized pixel positions
of animals in camera trap images:

``` r

animal_positions
#> # A tibble: 42 × 6
#>    deploymentID eventID     x     y imageWidth imageHeight
#>    <chr>          <dbl> <dbl> <dbl>      <int>       <int>
#>  1 S01                0 2612. 1414.       2048        1536
#>  2 S01                0 1962. 1289.       2048        1536
#>  3 S01                0 1648. 1262.       2048        1536
#>  4 S01                0 1220. 1285.       2048        1536
#>  5 S01                1 1041. 1361.       2048        1536
#>  6 S01                1 1215. 1403.       2048        1536
#>  7 S01                1 1238. 1410.       2048        1536
#>  8 S01                1 1238. 1410.       2048        1536
#>  9 S01                1 1238. 1410.       2048        1536
#> 10 S01                1 1238. 1410.       2048        1536
#> # ℹ 32 more rows
```

`calibration_models` is a named list of site calibration models, where
each element corresponds to a deployment:

``` r

names(calibration_models)
#> [1] "S01"  "S02"  "S03a" "S03b"
```

## Calculate radius and angle

The function
[`calculate_individual_radius_angle()`](https://inbo.github.io/camtraptor/reference/calculate_individual_radius_angle.md)
takes the digitized pixel positions and the calibration models and
returns the radial distance and angular position of each animal relative
to the camera:

``` r

calculate_individual_radius_angle(animal_positions, calibration_models)
#> # A tibble: 42 × 9
#>    deploymentID eventID     x     y imageWidth imageHeight radius   angle
#>    <chr>          <dbl> <dbl> <dbl>      <int>       <int>  <dbl>   <dbl>
#>  1 S01                0 2612. 1414.       2048        1536   1.49 0.579  
#>  2 S01                0 1962. 1289.       2048        1536   2.16 0.342  
#>  3 S01                0 1648. 1262.       2048        1536   2.54 0.227  
#>  4 S01                0 1220. 1285.       2048        1536   2.88 0.0714 
#>  5 S01                1 1041. 1361.       2048        1536   2.59 0.00608
#>  6 S01                1 1215. 1403.       2048        1536   2.23 0.0697 
#>  7 S01                1 1238. 1410.       2048        1536   2.18 0.0780 
#>  8 S01                1 1238. 1410.       2048        1536   2.18 0.0780 
#>  9 S01                1 1238. 1410.       2048        1536   2.18 0.0780 
#> 10 S01                1 1238. 1410.       2048        1536   2.18 0.0780 
#> # ℹ 32 more rows
#> # ℹ 1 more variable: frame_count <int>
```

The function returns the original `animal_positions` data frame with
three additional columns:

| column name | description |
|----|----|
| `radius` | Radial distance from the camera. Units depend on the units used during site calibration. |
| `angle` | Angular distance from the camera centre line. |
| `frame_count` | Order of the frame within each sequence (event). |

## Specifying column names

By default, the function expects specific column names in
`animal_positions` (`deploymentID`, `eventID`, `x`, `y`, `imageWidth`,
`imageHeight`). If your data uses different column names, these can be
specified explicitly:

``` r

calculate_individual_radius_angle(
  animal_positions,
  calibration_models,
  deployment_id = "deploymentID",
  event_id = "eventID",
  x = "x",
  y = "y",
  image_width = "imageWidth",
  image_height = "imageHeight"
)
```
