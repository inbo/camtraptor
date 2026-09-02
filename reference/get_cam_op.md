# Get the camera operation matrix

**\[superseded\]**

This function is superseded because camtrapR now supports reading Camera
Trap Data Packages. Use
[`camtrapR::readCamtrapDP()`](https://jniedballa.github.io/camtrapR/reference/readCamtrapDP.html)
and
[`camtrapR::cameraOperation()`](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html)
instead.

Creates the camera operation matrix as returned by
[`camtrapR::cameraOperation()`](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).

## Usage

``` r
get_cam_op(
  x,
  station_col = "locationName",
  camera_col = NULL,
  session_col = NULL,
  use_prefix = FALSE
)
```

## Arguments

- x:

  Camera trap data package object, as returned by
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

- station_col:

  Column name to use for identifying the stations. Default:
  `"locationName"`.

- camera_col:

  Column name of the column specifying Camera ID. Default: `NULL`.

- session_col:

  Column name to use for identifying the session. Default: `NULL`. Use
  it for creating multi-session / multi-season detection histories.

- use_prefix:

  Logical (`TRUE` or `FALSE`). If `TRUE` the returned row names will
  start with prefix `"Station"` as returned by
  [`camtrapR::cameraOperation()`](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).
  Default: `FALSE`.

## Value

A matrix. Row names always indicate the station ID. Column names are
dates.

## Details

The deployment data are by default grouped by `locationName` (station ID
in camtrapR jargon) or another column specified by the user via the
`station_col` argument. If multiple deployments are linked to the same
location, daily efforts higher than 1 occur.

Partially active days, e.g. the first or the last day of a deployment,
result in decimal effort values as in
[`camtrapR::cameraOperation()`](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).

## See also

Other deprecated camtrapR-derived functions:
[`get_detection_history()`](https://inbo.github.io/camtraptor/reference/get_detection_history.md),
[`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(stringr)

x <- example_dataset()
get_cam_op(x)
#>                               2020-05-30 2020-05-31 2020-06-01 2020-06-02
#> B_HS_val 2_processiepark       0.8766551          1          1          1
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-06-03 2020-06-04 2020-06-05 2020-06-06
#> B_HS_val 2_processiepark               1          1          1          1
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-06-07 2020-06-08 2020-06-09 2020-06-10
#> B_HS_val 2_processiepark               1          1          1          1
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-06-11 2020-06-12 2020-06-13 2020-06-14
#> B_HS_val 2_processiepark               1          1          1          1
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-06-15 2020-06-16 2020-06-17 2020-06-18
#> B_HS_val 2_processiepark               1          1          1          1
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-06-19 2020-06-20 2020-06-21 2020-06-22
#> B_HS_val 2_processiepark           1.000          1          1          1
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom              0.125          1          1          1
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-06-23 2020-06-24 2020-06-25 2020-06-26
#> B_HS_val 2_processiepark               1          1          1          1
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                  1          1          1          1
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-06-27 2020-06-28 2020-06-29 2020-06-30
#> B_HS_val 2_processiepark               1  1.0000000          1          1
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                  1  0.9815046         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-07-01 2020-07-02 2020-07-03 2020-07-04
#> B_HS_val 2_processiepark       0.4039468         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-07-05 2020-07-06 2020-07-07 2020-07-08
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-07-09 2020-07-10 2020-07-11 2020-07-12
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-07-13 2020-07-14 2020-07-15 2020-07-16
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-07-17 2020-07-18 2020-07-19 2020-07-20
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-07-21 2020-07-22 2020-07-23 2020-07-24
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-07-25 2020-07-26 2020-07-27 2020-07-28
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-07-29 2020-07-30 2020-07-31 2020-08-01
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver  0.7710532          1          1          1
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-08-02 2020-08-03 2020-08-04 2020-08-05
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver          1          1          1          1
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-08-06 2020-08-07 2020-08-08 2020-08-09
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver          1          1  0.1810185         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-08-10 2020-08-11 2020-08-12 2020-08-13
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-08-14 2020-08-15 2020-08-16 2020-08-17
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-08-18 2020-08-19 2020-08-20 2020-08-21
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-08-22 2020-08-23 2020-08-24 2020-08-25
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-08-26 2020-08-27 2020-08-28 2020-08-29
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-08-30 2020-08-31 2020-09-01 2020-09-02
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-09-03 2020-09-04 2020-09-05 2020-09-06
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-09-07 2020-09-08 2020-09-09 2020-09-10
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-09-11 2020-09-12 2020-09-13 2020-09-14
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-09-15 2020-09-16 2020-09-17 2020-09-18
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-09-19 2020-09-20 2020-09-21 2020-09-22
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-09-23 2020-09-24 2020-09-25 2020-09-26
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-09-27 2020-09-28 2020-09-29 2020-09-30
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-10-01 2020-10-02 2020-10-03 2020-10-04
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-10-05 2020-10-06 2020-10-07 2020-10-08
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-10-09 2020-10-10 2020-10-11 2020-10-12
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-10-13 2020-10-14 2020-10-15 2020-10-16
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-10-17 2020-10-18 2020-10-19 2020-10-20
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-10-21 2020-10-22 2020-10-23 2020-10-24
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-10-25 2020-10-26 2020-10-27 2020-10-28
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-10-29 2020-10-30 2020-10-31 2020-11-01
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-11-02 2020-11-03 2020-11-04 2020-11-05
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-11-06 2020-11-07 2020-11-08 2020-11-09
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-11-10 2020-11-11 2020-11-12 2020-11-13
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-11-14 2020-11-15 2020-11-16 2020-11-17
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-11-18 2020-11-19 2020-11-20 2020-11-21
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-11-22 2020-11-23 2020-11-24 2020-11-25
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-11-26 2020-11-27 2020-11-28 2020-11-29
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-11-30 2020-12-01 2020-12-02 2020-12-03
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-12-04 2020-12-05 2020-12-06 2020-12-07
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-12-08 2020-12-09 2020-12-10 2020-12-11
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-12-12 2020-12-13 2020-12-14 2020-12-15
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-12-16 2020-12-17 2020-12-18 2020-12-19
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-12-20 2020-12-21 2020-12-22 2020-12-23
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-12-24 2020-12-25 2020-12-26 2020-12-27
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2020-12-28 2020-12-29 2020-12-30 2020-12-31
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-01-01 2021-01-02 2021-01-03 2021-01-04
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-01-05 2021-01-06 2021-01-07 2021-01-08
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-01-09 2021-01-10 2021-01-11 2021-01-12
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-01-13 2021-01-14 2021-01-15 2021-01-16
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-01-17 2021-01-18 2021-01-19 2021-01-20
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-01-21 2021-01-22 2021-01-23 2021-01-24
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-01-25 2021-01-26 2021-01-27 2021-01-28
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-01-29 2021-01-30 2021-01-31 2021-02-01
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-02-02 2021-02-03 2021-02-04 2021-02-05
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-02-06 2021-02-07 2021-02-08 2021-02-09
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-02-10 2021-02-11 2021-02-12 2021-02-13
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-02-14 2021-02-15 2021-02-16 2021-02-17
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-02-18 2021-02-19 2021-02-20 2021-02-21
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-02-22 2021-02-23 2021-02-24 2021-02-25
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-02-26 2021-02-27 2021-02-28 2021-03-01
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-03-02 2021-03-03 2021-03-04 2021-03-05
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-03-06 2021-03-07 2021-03-08 2021-03-09
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-03-10 2021-03-11 2021-03-12 2021-03-13
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-03-14 2021-03-15 2021-03-16 2021-03-17
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-03-18 2021-03-19 2021-03-20 2021-03-21
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-03-22 2021-03-23 2021-03-24 2021-03-25
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#>                               2021-03-26 2021-03-27 2021-03-28 2021-03-29
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA  0.1400694          1          1
#>                               2021-03-30 2021-03-31 2021-04-01 2021-04-02
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                      1          1          1          1
#>                               2021-04-03 2021-04-04 2021-04-05 2021-04-06
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                      1          1          1          1
#>                               2021-04-07 2021-04-08 2021-04-09 2021-04-10
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                      1          1          1          1
#>                               2021-04-11 2021-04-12 2021-04-13 2021-04-14
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                      1          1          1          1
#>                               2021-04-15 2021-04-16 2021-04-17 2021-04-18
#> B_HS_val 2_processiepark              NA         NA         NA         NA
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                      1          1          1  0.8923611

# Specify column with station names
get_cam_op(x, station_col = "locationID")
#>          2020-05-30 2020-05-31 2020-06-01 2020-06-02 2020-06-03 2020-06-04
#> e254a13c  0.8766551          1          1          1          1          1
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-06-05 2020-06-06 2020-06-07 2020-06-08 2020-06-09 2020-06-10
#> e254a13c          1          1          1          1          1          1
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-06-11 2020-06-12 2020-06-13 2020-06-14 2020-06-15 2020-06-16
#> e254a13c          1          1          1          1          1          1
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-06-17 2020-06-18 2020-06-19 2020-06-20 2020-06-21 2020-06-22
#> e254a13c          1          1      1.000          1          1          1
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA      0.125          1          1          1
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-06-23 2020-06-24 2020-06-25 2020-06-26 2020-06-27 2020-06-28
#> e254a13c          1          1          1          1          1  1.0000000
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0          1          1          1          1          1  0.9815046
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-06-29 2020-06-30 2020-07-01 2020-07-02 2020-07-03 2020-07-04
#> e254a13c          1          1  0.4039468         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-07-05 2020-07-06 2020-07-07 2020-07-08 2020-07-09 2020-07-10
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-07-11 2020-07-12 2020-07-13 2020-07-14 2020-07-15 2020-07-16
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-07-17 2020-07-18 2020-07-19 2020-07-20 2020-07-21 2020-07-22
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-07-23 2020-07-24 2020-07-25 2020-07-26 2020-07-27 2020-07-28
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-07-29 2020-07-30 2020-07-31 2020-08-01 2020-08-02 2020-08-03
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b  0.7710532          1          1          1          1          1
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-08-04 2020-08-05 2020-08-06 2020-08-07 2020-08-08 2020-08-09
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b          1          1          1          1  0.1810185         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-08-10 2020-08-11 2020-08-12 2020-08-13 2020-08-14 2020-08-15
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-08-16 2020-08-17 2020-08-18 2020-08-19 2020-08-20 2020-08-21
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-08-22 2020-08-23 2020-08-24 2020-08-25 2020-08-26 2020-08-27
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-08-28 2020-08-29 2020-08-30 2020-08-31 2020-09-01 2020-09-02
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-09-03 2020-09-04 2020-09-05 2020-09-06 2020-09-07 2020-09-08
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-09-09 2020-09-10 2020-09-11 2020-09-12 2020-09-13 2020-09-14
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-09-15 2020-09-16 2020-09-17 2020-09-18 2020-09-19 2020-09-20
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-09-21 2020-09-22 2020-09-23 2020-09-24 2020-09-25 2020-09-26
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-09-27 2020-09-28 2020-09-29 2020-09-30 2020-10-01 2020-10-02
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-10-03 2020-10-04 2020-10-05 2020-10-06 2020-10-07 2020-10-08
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-10-09 2020-10-10 2020-10-11 2020-10-12 2020-10-13 2020-10-14
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-10-15 2020-10-16 2020-10-17 2020-10-18 2020-10-19 2020-10-20
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-10-21 2020-10-22 2020-10-23 2020-10-24 2020-10-25 2020-10-26
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-10-27 2020-10-28 2020-10-29 2020-10-30 2020-10-31 2020-11-01
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-11-02 2020-11-03 2020-11-04 2020-11-05 2020-11-06 2020-11-07
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-11-08 2020-11-09 2020-11-10 2020-11-11 2020-11-12 2020-11-13
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-11-14 2020-11-15 2020-11-16 2020-11-17 2020-11-18 2020-11-19
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-11-20 2020-11-21 2020-11-22 2020-11-23 2020-11-24 2020-11-25
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-11-26 2020-11-27 2020-11-28 2020-11-29 2020-11-30 2020-12-01
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-12-02 2020-12-03 2020-12-04 2020-12-05 2020-12-06 2020-12-07
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-12-08 2020-12-09 2020-12-10 2020-12-11 2020-12-12 2020-12-13
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-12-14 2020-12-15 2020-12-16 2020-12-17 2020-12-18 2020-12-19
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-12-20 2020-12-21 2020-12-22 2020-12-23 2020-12-24 2020-12-25
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2020-12-26 2020-12-27 2020-12-28 2020-12-29 2020-12-30 2020-12-31
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-01-01 2021-01-02 2021-01-03 2021-01-04 2021-01-05 2021-01-06
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-01-07 2021-01-08 2021-01-09 2021-01-10 2021-01-11 2021-01-12
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-01-13 2021-01-14 2021-01-15 2021-01-16 2021-01-17 2021-01-18
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-01-19 2021-01-20 2021-01-21 2021-01-22 2021-01-23 2021-01-24
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-01-25 2021-01-26 2021-01-27 2021-01-28 2021-01-29 2021-01-30
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-01-31 2021-02-01 2021-02-02 2021-02-03 2021-02-04 2021-02-05
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-02-06 2021-02-07 2021-02-08 2021-02-09 2021-02-10 2021-02-11
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-02-12 2021-02-13 2021-02-14 2021-02-15 2021-02-16 2021-02-17
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-02-18 2021-02-19 2021-02-20 2021-02-21 2021-02-22 2021-02-23
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-02-24 2021-02-25 2021-02-26 2021-02-27 2021-02-28 2021-03-01
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-03-02 2021-03-03 2021-03-04 2021-03-05 2021-03-06 2021-03-07
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-03-08 2021-03-09 2021-03-10 2021-03-11 2021-03-12 2021-03-13
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-03-14 2021-03-15 2021-03-16 2021-03-17 2021-03-18 2021-03-19
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-03-20 2021-03-21 2021-03-22 2021-03-23 2021-03-24 2021-03-25
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA         NA         NA         NA         NA         NA
#>          2021-03-26 2021-03-27 2021-03-28 2021-03-29 2021-03-30 2021-03-31
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced         NA  0.1400694          1          1          1          1
#>          2021-04-01 2021-04-02 2021-04-03 2021-04-04 2021-04-05 2021-04-06
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced          1          1          1          1          1          1
#>          2021-04-07 2021-04-08 2021-04-09 2021-04-10 2021-04-11 2021-04-12
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced          1          1          1          1          1          1
#>          2021-04-13 2021-04-14 2021-04-15 2021-04-16 2021-04-17 2021-04-18
#> e254a13c         NA         NA         NA         NA         NA         NA
#> 2df5259b         NA         NA         NA         NA         NA         NA
#> ff1535c0         NA         NA         NA         NA         NA         NA
#> ce943ced          1          1          1          1          1  0.8923611

# Specify column with session IDs
x_sessions <- x
deployments(x_sessions) <- deployments(x_sessions) %>%
  mutate(session = ifelse(
    str_starts(.data$locationName, "B_DL_"),
    "after2020",
    "before2020"
  ))
  
get_cam_op(x_sessions, session_col = "session")
#>                                               2020-05-30 2020-05-31 2020-06-01
#> B_HS_val 2_processiepark__SESS_before2020      0.8766551          1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-06-02 2020-06-03 2020-06-04
#> B_HS_val 2_processiepark__SESS_before2020              1          1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-06-05 2020-06-06 2020-06-07
#> B_HS_val 2_processiepark__SESS_before2020              1          1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-06-08 2020-06-09 2020-06-10
#> B_HS_val 2_processiepark__SESS_before2020              1          1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-06-11 2020-06-12 2020-06-13
#> B_HS_val 2_processiepark__SESS_before2020              1          1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-06-14 2020-06-15 2020-06-16
#> B_HS_val 2_processiepark__SESS_before2020              1          1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-06-17 2020-06-18 2020-06-19
#> B_HS_val 2_processiepark__SESS_before2020              1          1      1.000
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA      0.125
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-06-20 2020-06-21 2020-06-22
#> B_HS_val 2_processiepark__SESS_before2020              1          1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                  1          1          1
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-06-23 2020-06-24 2020-06-25
#> B_HS_val 2_processiepark__SESS_before2020              1          1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                  1          1          1
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-06-26 2020-06-27 2020-06-28
#> B_HS_val 2_processiepark__SESS_before2020              1          1  1.0000000
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                  1          1  0.9815046
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-06-29 2020-06-30 2020-07-01
#> B_HS_val 2_processiepark__SESS_before2020              1          1  0.4039468
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-07-02 2020-07-03 2020-07-04
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-07-05 2020-07-06 2020-07-07
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-07-08 2020-07-09 2020-07-10
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-07-11 2020-07-12 2020-07-13
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-07-14 2020-07-15 2020-07-16
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-07-17 2020-07-18 2020-07-19
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-07-20 2020-07-21 2020-07-22
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-07-23 2020-07-24 2020-07-25
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-07-26 2020-07-27 2020-07-28
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-07-29 2020-07-30 2020-07-31
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020  0.7710532          1          1
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-01 2020-08-02 2020-08-03
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020          1          1          1
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-04 2020-08-05 2020-08-06
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020          1          1          1
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-07 2020-08-08 2020-08-09
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020          1  0.1810185         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-10 2020-08-11 2020-08-12
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-13 2020-08-14 2020-08-15
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-16 2020-08-17 2020-08-18
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-19 2020-08-20 2020-08-21
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-22 2020-08-23 2020-08-24
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-25 2020-08-26 2020-08-27
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-28 2020-08-29 2020-08-30
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-08-31 2020-09-01 2020-09-02
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-09-03 2020-09-04 2020-09-05
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-09-06 2020-09-07 2020-09-08
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-09-09 2020-09-10 2020-09-11
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-09-12 2020-09-13 2020-09-14
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-09-15 2020-09-16 2020-09-17
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-09-18 2020-09-19 2020-09-20
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-09-21 2020-09-22 2020-09-23
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-09-24 2020-09-25 2020-09-26
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-09-27 2020-09-28 2020-09-29
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-09-30 2020-10-01 2020-10-02
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-10-03 2020-10-04 2020-10-05
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-10-06 2020-10-07 2020-10-08
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-10-09 2020-10-10 2020-10-11
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-10-12 2020-10-13 2020-10-14
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-10-15 2020-10-16 2020-10-17
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-10-18 2020-10-19 2020-10-20
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-10-21 2020-10-22 2020-10-23
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-10-24 2020-10-25 2020-10-26
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-10-27 2020-10-28 2020-10-29
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-10-30 2020-10-31 2020-11-01
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-11-02 2020-11-03 2020-11-04
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-11-05 2020-11-06 2020-11-07
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-11-08 2020-11-09 2020-11-10
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-11-11 2020-11-12 2020-11-13
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-11-14 2020-11-15 2020-11-16
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-11-17 2020-11-18 2020-11-19
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-11-20 2020-11-21 2020-11-22
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-11-23 2020-11-24 2020-11-25
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-11-26 2020-11-27 2020-11-28
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-11-29 2020-11-30 2020-12-01
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-12-02 2020-12-03 2020-12-04
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-12-05 2020-12-06 2020-12-07
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-12-08 2020-12-09 2020-12-10
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-12-11 2020-12-12 2020-12-13
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-12-14 2020-12-15 2020-12-16
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-12-17 2020-12-18 2020-12-19
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-12-20 2020-12-21 2020-12-22
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-12-23 2020-12-24 2020-12-25
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-12-26 2020-12-27 2020-12-28
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2020-12-29 2020-12-30 2020-12-31
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-01 2021-01-02 2021-01-03
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-04 2021-01-05 2021-01-06
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-07 2021-01-08 2021-01-09
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-10 2021-01-11 2021-01-12
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-13 2021-01-14 2021-01-15
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-16 2021-01-17 2021-01-18
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-19 2021-01-20 2021-01-21
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-22 2021-01-23 2021-01-24
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-25 2021-01-26 2021-01-27
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-28 2021-01-29 2021-01-30
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-01-31 2021-02-01 2021-02-02
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-02-03 2021-02-04 2021-02-05
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-02-06 2021-02-07 2021-02-08
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-02-09 2021-02-10 2021-02-11
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-02-12 2021-02-13 2021-02-14
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-02-15 2021-02-16 2021-02-17
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-02-18 2021-02-19 2021-02-20
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-02-21 2021-02-22 2021-02-23
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-02-24 2021-02-25 2021-02-26
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-02-27 2021-02-28 2021-03-01
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-03-02 2021-03-03 2021-03-04
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-03-05 2021-03-06 2021-03-07
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-03-08 2021-03-09 2021-03-10
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-03-11 2021-03-12 2021-03-13
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-03-14 2021-03-15 2021-03-16
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-03-17 2021-03-18 2021-03-19
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-03-20 2021-03-21 2021-03-22
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-03-23 2021-03-24 2021-03-25
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#>                                               2021-03-26 2021-03-27 2021-03-28
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA  0.1400694          1
#>                                               2021-03-29 2021-03-30 2021-03-31
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#>                                               2021-04-01 2021-04-02 2021-04-03
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#>                                               2021-04-04 2021-04-05 2021-04-06
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#>                                               2021-04-07 2021-04-08 2021-04-09
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#>                                               2021-04-10 2021-04-11 2021-04-12
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#>                                               2021-04-13 2021-04-14 2021-04-15
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#>                                               2021-04-16 2021-04-17 2021-04-18
#> B_HS_val 2_processiepark__SESS_before2020             NA         NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1  0.8923611

# Specify column with camera IDs
x_cameras <- x_sessions
deployments(x_cameras) <- deployments(x_cameras) %>%
  mutate(cameraID = c(1, 2, 3, 4))
get_cam_op(x_cameras, camera_col = "cameraID")
#>                                      2020-05-30 2020-05-31 2020-06-01
#> B_HS_val 2_processiepark__CAM_1       0.8766551          1          1
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-06-02 2020-06-03 2020-06-04
#> B_HS_val 2_processiepark__CAM_1               1          1          1
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-06-05 2020-06-06 2020-06-07
#> B_HS_val 2_processiepark__CAM_1               1          1          1
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-06-08 2020-06-09 2020-06-10
#> B_HS_val 2_processiepark__CAM_1               1          1          1
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-06-11 2020-06-12 2020-06-13
#> B_HS_val 2_processiepark__CAM_1               1          1          1
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-06-14 2020-06-15 2020-06-16
#> B_HS_val 2_processiepark__CAM_1               1          1          1
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-06-17 2020-06-18 2020-06-19
#> B_HS_val 2_processiepark__CAM_1               1          1      1.000
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA      0.125
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-06-20 2020-06-21 2020-06-22
#> B_HS_val 2_processiepark__CAM_1               1          1          1
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                  1          1          1
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-06-23 2020-06-24 2020-06-25
#> B_HS_val 2_processiepark__CAM_1               1          1          1
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                  1          1          1
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-06-26 2020-06-27 2020-06-28
#> B_HS_val 2_processiepark__CAM_1               1          1  1.0000000
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                  1          1  0.9815046
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-06-29 2020-06-30 2020-07-01
#> B_HS_val 2_processiepark__CAM_1               1          1  0.4039468
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-07-02 2020-07-03 2020-07-04
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-07-05 2020-07-06 2020-07-07
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-07-08 2020-07-09 2020-07-10
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-07-11 2020-07-12 2020-07-13
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-07-14 2020-07-15 2020-07-16
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-07-17 2020-07-18 2020-07-19
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-07-20 2020-07-21 2020-07-22
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-07-23 2020-07-24 2020-07-25
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-07-26 2020-07-27 2020-07-28
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-07-29 2020-07-30 2020-07-31
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2  0.7710532          1          1
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-01 2020-08-02 2020-08-03
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2          1          1          1
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-04 2020-08-05 2020-08-06
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2          1          1          1
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-07 2020-08-08 2020-08-09
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2          1  0.1810185         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-10 2020-08-11 2020-08-12
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-13 2020-08-14 2020-08-15
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-16 2020-08-17 2020-08-18
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-19 2020-08-20 2020-08-21
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-22 2020-08-23 2020-08-24
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-25 2020-08-26 2020-08-27
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-28 2020-08-29 2020-08-30
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-08-31 2020-09-01 2020-09-02
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-09-03 2020-09-04 2020-09-05
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-09-06 2020-09-07 2020-09-08
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-09-09 2020-09-10 2020-09-11
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-09-12 2020-09-13 2020-09-14
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-09-15 2020-09-16 2020-09-17
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-09-18 2020-09-19 2020-09-20
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-09-21 2020-09-22 2020-09-23
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-09-24 2020-09-25 2020-09-26
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-09-27 2020-09-28 2020-09-29
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-09-30 2020-10-01 2020-10-02
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-10-03 2020-10-04 2020-10-05
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-10-06 2020-10-07 2020-10-08
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-10-09 2020-10-10 2020-10-11
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-10-12 2020-10-13 2020-10-14
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-10-15 2020-10-16 2020-10-17
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-10-18 2020-10-19 2020-10-20
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-10-21 2020-10-22 2020-10-23
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-10-24 2020-10-25 2020-10-26
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-10-27 2020-10-28 2020-10-29
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-10-30 2020-10-31 2020-11-01
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-11-02 2020-11-03 2020-11-04
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-11-05 2020-11-06 2020-11-07
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-11-08 2020-11-09 2020-11-10
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-11-11 2020-11-12 2020-11-13
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-11-14 2020-11-15 2020-11-16
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-11-17 2020-11-18 2020-11-19
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-11-20 2020-11-21 2020-11-22
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-11-23 2020-11-24 2020-11-25
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-11-26 2020-11-27 2020-11-28
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-11-29 2020-11-30 2020-12-01
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-12-02 2020-12-03 2020-12-04
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-12-05 2020-12-06 2020-12-07
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-12-08 2020-12-09 2020-12-10
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-12-11 2020-12-12 2020-12-13
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-12-14 2020-12-15 2020-12-16
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-12-17 2020-12-18 2020-12-19
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-12-20 2020-12-21 2020-12-22
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-12-23 2020-12-24 2020-12-25
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-12-26 2020-12-27 2020-12-28
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2020-12-29 2020-12-30 2020-12-31
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-01 2021-01-02 2021-01-03
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-04 2021-01-05 2021-01-06
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-07 2021-01-08 2021-01-09
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-10 2021-01-11 2021-01-12
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-13 2021-01-14 2021-01-15
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-16 2021-01-17 2021-01-18
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-19 2021-01-20 2021-01-21
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-22 2021-01-23 2021-01-24
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-25 2021-01-26 2021-01-27
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-28 2021-01-29 2021-01-30
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-01-31 2021-02-01 2021-02-02
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-02-03 2021-02-04 2021-02-05
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-02-06 2021-02-07 2021-02-08
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-02-09 2021-02-10 2021-02-11
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-02-12 2021-02-13 2021-02-14
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-02-15 2021-02-16 2021-02-17
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-02-18 2021-02-19 2021-02-20
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-02-21 2021-02-22 2021-02-23
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-02-24 2021-02-25 2021-02-26
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-02-27 2021-02-28 2021-03-01
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-03-02 2021-03-03 2021-03-04
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-03-05 2021-03-06 2021-03-07
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-03-08 2021-03-09 2021-03-10
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-03-11 2021-03-12 2021-03-13
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-03-14 2021-03-15 2021-03-16
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-03-17 2021-03-18 2021-03-19
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-03-20 2021-03-21 2021-03-22
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-03-23 2021-03-24 2021-03-25
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA         NA         NA
#>                                      2021-03-26 2021-03-27 2021-03-28
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                     NA  0.1400694          1
#>                                      2021-03-29 2021-03-30 2021-03-31
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                      1          1          1
#>                                      2021-04-01 2021-04-02 2021-04-03
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                      1          1          1
#>                                      2021-04-04 2021-04-05 2021-04-06
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                      1          1          1
#>                                      2021-04-07 2021-04-08 2021-04-09
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                      1          1          1
#>                                      2021-04-10 2021-04-11 2021-04-12
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                      1          1          1
#>                                      2021-04-13 2021-04-14 2021-04-15
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                      1          1          1
#>                                      2021-04-16 2021-04-17 2021-04-18
#> B_HS_val 2_processiepark__CAM_1              NA         NA         NA
#> B_DL_val 5_beek kleine vijver__CAM_2         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_3                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_4                      1          1  0.8923611

# Specify both session and camera IDs
get_cam_op(
  x_cameras,
  camera_col = "cameraID",
  session_col = "session"
)
#>                                                      2020-05-30 2020-05-31
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1      0.8766551          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-01 2020-06-02
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-03 2020-06-04
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-05 2020-06-06
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-07 2020-06-08
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-09 2020-06-10
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-11 2020-06-12
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-13 2020-06-14
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-15 2020-06-16
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-17 2020-06-18
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-19 2020-06-20
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1          1.000          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3              0.125          1
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-21 2020-06-22
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                  1          1
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-23 2020-06-24
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                  1          1
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-25 2020-06-26
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                  1          1
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-27 2020-06-28
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1  1.0000000
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                  1  0.9815046
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-06-29 2020-06-30
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1              1          1
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-01 2020-07-02
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1      0.4039468         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-03 2020-07-04
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-05 2020-07-06
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-07 2020-07-08
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-09 2020-07-10
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-11 2020-07-12
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-13 2020-07-14
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-15 2020-07-16
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-17 2020-07-18
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-19 2020-07-20
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-21 2020-07-22
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-23 2020-07-24
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-25 2020-07-26
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-27 2020-07-28
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-29 2020-07-30
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2  0.7710532          1
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-07-31 2020-08-01
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2          1          1
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-02 2020-08-03
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2          1          1
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-04 2020-08-05
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2          1          1
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-06 2020-08-07
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2          1          1
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-08 2020-08-09
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2  0.1810185         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-10 2020-08-11
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-12 2020-08-13
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-14 2020-08-15
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-16 2020-08-17
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-18 2020-08-19
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-20 2020-08-21
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-22 2020-08-23
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-24 2020-08-25
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-26 2020-08-27
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-28 2020-08-29
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-08-30 2020-08-31
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-01 2020-09-02
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-03 2020-09-04
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-05 2020-09-06
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-07 2020-09-08
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-09 2020-09-10
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-11 2020-09-12
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-13 2020-09-14
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-15 2020-09-16
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-17 2020-09-18
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-19 2020-09-20
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-21 2020-09-22
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-23 2020-09-24
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-25 2020-09-26
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-27 2020-09-28
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-09-29 2020-09-30
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-01 2020-10-02
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-03 2020-10-04
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-05 2020-10-06
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-07 2020-10-08
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-09 2020-10-10
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-11 2020-10-12
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-13 2020-10-14
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-15 2020-10-16
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-17 2020-10-18
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-19 2020-10-20
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-21 2020-10-22
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-23 2020-10-24
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-25 2020-10-26
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-27 2020-10-28
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-29 2020-10-30
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-10-31 2020-11-01
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-02 2020-11-03
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-04 2020-11-05
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-06 2020-11-07
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-08 2020-11-09
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-10 2020-11-11
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-12 2020-11-13
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-14 2020-11-15
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-16 2020-11-17
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-18 2020-11-19
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-20 2020-11-21
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-22 2020-11-23
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-24 2020-11-25
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-26 2020-11-27
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-28 2020-11-29
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-11-30 2020-12-01
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-02 2020-12-03
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-04 2020-12-05
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-06 2020-12-07
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-08 2020-12-09
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-10 2020-12-11
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-12 2020-12-13
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-14 2020-12-15
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-16 2020-12-17
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-18 2020-12-19
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-20 2020-12-21
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-22 2020-12-23
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-24 2020-12-25
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-26 2020-12-27
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-28 2020-12-29
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2020-12-30 2020-12-31
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-01 2021-01-02
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-03 2021-01-04
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-05 2021-01-06
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-07 2021-01-08
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-09 2021-01-10
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-11 2021-01-12
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-13 2021-01-14
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-15 2021-01-16
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-17 2021-01-18
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-19 2021-01-20
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-21 2021-01-22
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-23 2021-01-24
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-25 2021-01-26
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-27 2021-01-28
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-29 2021-01-30
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-01-31 2021-02-01
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-02 2021-02-03
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-04 2021-02-05
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-06 2021-02-07
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-08 2021-02-09
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-10 2021-02-11
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-12 2021-02-13
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-14 2021-02-15
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-16 2021-02-17
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-18 2021-02-19
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-20 2021-02-21
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-22 2021-02-23
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-24 2021-02-25
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-26 2021-02-27
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-02-28 2021-03-01
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-02 2021-03-03
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-04 2021-03-05
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-06 2021-03-07
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-08 2021-03-09
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-10 2021-03-11
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-12 2021-03-13
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-14 2021-03-15
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-16 2021-03-17
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-18 2021-03-19
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-20 2021-03-21
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-22 2021-03-23
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-24 2021-03-25
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA         NA
#>                                                      2021-03-26 2021-03-27
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                    NA  0.1400694
#>                                                      2021-03-28 2021-03-29
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1          1
#>                                                      2021-03-30 2021-03-31
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1          1
#>                                                      2021-04-01 2021-04-02
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1          1
#>                                                      2021-04-03 2021-04-04
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1          1
#>                                                      2021-04-05 2021-04-06
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1          1
#>                                                      2021-04-07 2021-04-08
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1          1
#>                                                      2021-04-09 2021-04-10
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1          1
#>                                                      2021-04-11 2021-04-12
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1          1
#>                                                      2021-04-13 2021-04-14
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1          1
#>                                                      2021-04-15 2021-04-16
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1          1
#>                                                      2021-04-17 2021-04-18
#> B_HS_val 2_processiepark__SESS_before2020__CAM_1             NA         NA
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_2         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_3                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_4                     1  0.8923611

# Use prefix Station as in camtrapR's camera operation matrix
get_cam_op(x, use_prefix = TRUE)
#>                                      2020-05-30 2020-05-31 2020-06-01
#> StationB_HS_val 2_processiepark       0.8766551          1          1
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-06-02 2020-06-03 2020-06-04
#> StationB_HS_val 2_processiepark               1          1          1
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-06-05 2020-06-06 2020-06-07
#> StationB_HS_val 2_processiepark               1          1          1
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-06-08 2020-06-09 2020-06-10
#> StationB_HS_val 2_processiepark               1          1          1
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-06-11 2020-06-12 2020-06-13
#> StationB_HS_val 2_processiepark               1          1          1
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-06-14 2020-06-15 2020-06-16
#> StationB_HS_val 2_processiepark               1          1          1
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-06-17 2020-06-18 2020-06-19
#> StationB_HS_val 2_processiepark               1          1      1.000
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA      0.125
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-06-20 2020-06-21 2020-06-22
#> StationB_HS_val 2_processiepark               1          1          1
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                  1          1          1
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-06-23 2020-06-24 2020-06-25
#> StationB_HS_val 2_processiepark               1          1          1
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                  1          1          1
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-06-26 2020-06-27 2020-06-28
#> StationB_HS_val 2_processiepark               1          1  1.0000000
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                  1          1  0.9815046
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-06-29 2020-06-30 2020-07-01
#> StationB_HS_val 2_processiepark               1          1  0.4039468
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-07-02 2020-07-03 2020-07-04
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-07-05 2020-07-06 2020-07-07
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-07-08 2020-07-09 2020-07-10
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-07-11 2020-07-12 2020-07-13
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-07-14 2020-07-15 2020-07-16
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-07-17 2020-07-18 2020-07-19
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-07-20 2020-07-21 2020-07-22
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-07-23 2020-07-24 2020-07-25
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-07-26 2020-07-27 2020-07-28
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-07-29 2020-07-30 2020-07-31
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver  0.7710532          1          1
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-01 2020-08-02 2020-08-03
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver          1          1          1
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-04 2020-08-05 2020-08-06
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver          1          1          1
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-07 2020-08-08 2020-08-09
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver          1  0.1810185         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-10 2020-08-11 2020-08-12
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-13 2020-08-14 2020-08-15
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-16 2020-08-17 2020-08-18
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-19 2020-08-20 2020-08-21
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-22 2020-08-23 2020-08-24
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-25 2020-08-26 2020-08-27
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-28 2020-08-29 2020-08-30
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-08-31 2020-09-01 2020-09-02
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-09-03 2020-09-04 2020-09-05
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-09-06 2020-09-07 2020-09-08
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-09-09 2020-09-10 2020-09-11
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-09-12 2020-09-13 2020-09-14
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-09-15 2020-09-16 2020-09-17
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-09-18 2020-09-19 2020-09-20
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-09-21 2020-09-22 2020-09-23
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-09-24 2020-09-25 2020-09-26
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-09-27 2020-09-28 2020-09-29
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-09-30 2020-10-01 2020-10-02
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-10-03 2020-10-04 2020-10-05
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-10-06 2020-10-07 2020-10-08
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-10-09 2020-10-10 2020-10-11
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-10-12 2020-10-13 2020-10-14
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-10-15 2020-10-16 2020-10-17
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-10-18 2020-10-19 2020-10-20
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-10-21 2020-10-22 2020-10-23
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-10-24 2020-10-25 2020-10-26
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-10-27 2020-10-28 2020-10-29
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-10-30 2020-10-31 2020-11-01
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-11-02 2020-11-03 2020-11-04
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-11-05 2020-11-06 2020-11-07
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-11-08 2020-11-09 2020-11-10
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-11-11 2020-11-12 2020-11-13
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-11-14 2020-11-15 2020-11-16
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-11-17 2020-11-18 2020-11-19
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-11-20 2020-11-21 2020-11-22
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-11-23 2020-11-24 2020-11-25
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-11-26 2020-11-27 2020-11-28
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-11-29 2020-11-30 2020-12-01
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-12-02 2020-12-03 2020-12-04
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-12-05 2020-12-06 2020-12-07
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-12-08 2020-12-09 2020-12-10
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-12-11 2020-12-12 2020-12-13
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-12-14 2020-12-15 2020-12-16
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-12-17 2020-12-18 2020-12-19
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-12-20 2020-12-21 2020-12-22
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-12-23 2020-12-24 2020-12-25
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-12-26 2020-12-27 2020-12-28
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2020-12-29 2020-12-30 2020-12-31
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-01 2021-01-02 2021-01-03
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-04 2021-01-05 2021-01-06
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-07 2021-01-08 2021-01-09
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-10 2021-01-11 2021-01-12
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-13 2021-01-14 2021-01-15
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-16 2021-01-17 2021-01-18
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-19 2021-01-20 2021-01-21
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-22 2021-01-23 2021-01-24
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-25 2021-01-26 2021-01-27
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-28 2021-01-29 2021-01-30
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-01-31 2021-02-01 2021-02-02
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-02-03 2021-02-04 2021-02-05
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-02-06 2021-02-07 2021-02-08
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-02-09 2021-02-10 2021-02-11
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-02-12 2021-02-13 2021-02-14
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-02-15 2021-02-16 2021-02-17
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-02-18 2021-02-19 2021-02-20
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-02-21 2021-02-22 2021-02-23
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-02-24 2021-02-25 2021-02-26
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-02-27 2021-02-28 2021-03-01
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-03-02 2021-03-03 2021-03-04
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-03-05 2021-03-06 2021-03-07
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-03-08 2021-03-09 2021-03-10
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-03-11 2021-03-12 2021-03-13
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-03-14 2021-03-15 2021-03-16
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-03-17 2021-03-18 2021-03-19
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-03-20 2021-03-21 2021-03-22
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-03-23 2021-03-24 2021-03-25
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#>                                      2021-03-26 2021-03-27 2021-03-28
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA  0.1400694          1
#>                                      2021-03-29 2021-03-30 2021-03-31
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#>                                      2021-04-01 2021-04-02 2021-04-03
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#>                                      2021-04-04 2021-04-05 2021-04-06
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#>                                      2021-04-07 2021-04-08 2021-04-09
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#>                                      2021-04-10 2021-04-11 2021-04-12
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#>                                      2021-04-13 2021-04-14 2021-04-15
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#>                                      2021-04-16 2021-04-17 2021-04-18
#> StationB_HS_val 2_processiepark              NA         NA         NA
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1  0.8923611
```
