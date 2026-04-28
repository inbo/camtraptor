# Calculate animal position

Calculates the position of animal relative to a camera based on image
pixel positions and site calibration models.

## Usage

``` r
calc_animal_pos(
  animal_pos,
  calib_models,
  dep_tag = "deploymentID",
  sequence_id = "sequenceID",
  x = "x",
  y = "y",
  image_width = "imageWidth",
  image_height = "imageHeight"
)
```

## Arguments

- animal_pos:

  Data frame (tibble) of animal position digitization data. It must
  contain (at least) the columns defined in args `dep_tag`,
  `sequence_id`, `x`, `y`, `image_width` and `image_height`.

- calib_models:

  Named list of deployment calibration models or site calibration models
  (`calibs` objects), produced using `cal.site()` (not yet included in
  this package). The deployment names are used as names.

- dep_tag:

  Column in `animal_pos` against which names of the elements can be
  matched to apply the right deployment calibration models. Default:
  `"deploymentID"`.

- sequence_id:

  Column in `animal_pos` containing the sequence ID the images belong
  to. Default: `"sequenceID"`.

- x:

  Column in `animal_pos` containing x pixel positions for each digitised
  point. Default: `"x"`.

- y:

  Column in `animal_pos` containing y pixel positions for each digitised
  point. Default: `"y"`.

- image_width:

  Column in `animal_pos` containing the pixel x dimension of each image.
  Default: `"imageWidth"`. Notice that the pixel x dimension must be
  consistent for each deployment.

- image_height:

  Column in `animal_pos` containing the pixel y dimension of each image.
  Default: `"imageHeight"`. Notice that the pixel y dimension must be
  consistent for each deployment.

## Value

Original tibble data frame as passed via `animal_pos` with additional
columns:

- `radius`: Radial distance from camera.

- `angle`: Angular distance from camera.

- `frame_count`: Indicator of the frame order within each sequence.

## Examples

``` r
# Use default values
calc_animal_pos(animal_positions, dep_calib_models)
#> # A tibble: 42 × 9
#>    deploymentID sequenceID     x     y imageWidth imageHeight radius   angle
#>    <chr>             <dbl> <dbl> <dbl>      <int>       <int>  <dbl>   <dbl>
#>  1 S01                   0 2612. 1414.       2048        1536   1.49 0.579  
#>  2 S01                   0 1962. 1289.       2048        1536   2.16 0.342  
#>  3 S01                   0 1648. 1262.       2048        1536   2.54 0.227  
#>  4 S01                   0 1220. 1285.       2048        1536   2.88 0.0714 
#>  5 S01                   1 1041. 1361.       2048        1536   2.59 0.00608
#>  6 S01                   1 1215. 1403.       2048        1536   2.23 0.0697 
#>  7 S01                   1 1238. 1410.       2048        1536   2.18 0.0780 
#>  8 S01                   1 1238. 1410.       2048        1536   2.18 0.0780 
#>  9 S01                   1 1238. 1410.       2048        1536   2.18 0.0780 
#> 10 S01                   1 1238. 1410.       2048        1536   2.18 0.0780 
#> # ℹ 32 more rows
#> # ℹ 1 more variable: frame_count <int>
```
