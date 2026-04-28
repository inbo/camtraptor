# Get camera operation matrix

Returns the [camera operation
matrix](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html)
as returned by
[camtrapR::cameraOperation()](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).

## Usage

``` r
get_cam_op(
  package = NULL,
  ...,
  station_col = "locationName",
  camera_col = NULL,
  session_col = NULL,
  use_prefix = FALSE,
  datapkg = lifecycle::deprecated()
)
```

## Arguments

- package:

  Camera trap data package object, as returned by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- ...:

  filter predicates for filtering on deployments.

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
  [camtrapR::cameraOperation()](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).
  Default: `FALSE`.

- datapkg:

  Deprecated. Use `package` instead.

## Value

A matrix. Row names always indicate the station ID. Column names are
dates.

## Details

The deployment data are by default grouped by `locationName` (station ID
in camtrapR jargon) or another column specified by the user via the
`station_col` argument. If multiple deployments are linked to same
location, daily efforts higher than 1 occur.

Partially active days, e.g. the first or the last day of a deployment,
result in decimal effort values as in
[camtrapR::cameraOperation()](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).

## See also

Other camtrapR-derived functions:
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
get_cam_op(mica)
#>                               2019-10-09 2019-10-10 2019-10-11 2019-10-12
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                     0.5290856          1          1          1
#>                               2019-10-13 2019-10-14 2019-10-15 2019-10-16
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                             1          1          1          1
#>                               2019-10-17 2019-10-18 2019-10-19 2019-10-20
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                             1          1          1          1
#>                               2019-10-21 2019-10-22 2019-10-23 2019-10-24
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                             1          1  0.4168519         NA
#>                               2019-10-25 2019-10-26 2019-10-27 2019-10-28
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-10-29 2019-10-30 2019-10-31 2019-11-01
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-11-02 2019-11-03 2019-11-04 2019-11-05
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-11-06 2019-11-07 2019-11-08 2019-11-09
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-11-10 2019-11-11 2019-11-12 2019-11-13
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-11-14 2019-11-15 2019-11-16 2019-11-17
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-11-18 2019-11-19 2019-11-20 2019-11-21
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-11-22 2019-11-23 2019-11-24 2019-11-25
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-11-26 2019-11-27 2019-11-28 2019-11-29
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-11-30 2019-12-01 2019-12-02 2019-12-03
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-12-04 2019-12-05 2019-12-06 2019-12-07
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-12-08 2019-12-09 2019-12-10 2019-12-11
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-12-12 2019-12-13 2019-12-14 2019-12-15
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-12-16 2019-12-17 2019-12-18 2019-12-19
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-12-20 2019-12-21 2019-12-22 2019-12-23
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-12-24 2019-12-25 2019-12-26 2019-12-27
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2019-12-28 2019-12-29 2019-12-30 2019-12-31
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-01-01 2020-01-02 2020-01-03 2020-01-04
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-01-05 2020-01-06 2020-01-07 2020-01-08
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-01-09 2020-01-10 2020-01-11 2020-01-12
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-01-13 2020-01-14 2020-01-15 2020-01-16
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-01-17 2020-01-18 2020-01-19 2020-01-20
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-01-21 2020-01-22 2020-01-23 2020-01-24
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-01-25 2020-01-26 2020-01-27 2020-01-28
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-01-29 2020-01-30 2020-01-31 2020-02-01
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-02-02 2020-02-03 2020-02-04 2020-02-05
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-02-06 2020-02-07 2020-02-08 2020-02-09
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-02-10 2020-02-11 2020-02-12 2020-02-13
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-02-14 2020-02-15 2020-02-16 2020-02-17
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-02-18 2020-02-19 2020-02-20 2020-02-21
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-02-22 2020-02-23 2020-02-24 2020-02-25
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-02-26 2020-02-27 2020-02-28 2020-02-29
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-03-01 2020-03-02 2020-03-03 2020-03-04
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-03-05 2020-03-06 2020-03-07 2020-03-08
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-03-09 2020-03-10 2020-03-11 2020-03-12
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-03-13 2020-03-14 2020-03-15 2020-03-16
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-03-17 2020-03-18 2020-03-19 2020-03-20
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-03-21 2020-03-22 2020-03-23 2020-03-24
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-03-25 2020-03-26 2020-03-27 2020-03-28
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-03-29 2020-03-30 2020-03-31 2020-04-01
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-04-02 2020-04-03 2020-04-04 2020-04-05
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-04-06 2020-04-07 2020-04-08 2020-04-09
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-04-10 2020-04-11 2020-04-12 2020-04-13
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-04-14 2020-04-15 2020-04-16 2020-04-17
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-04-18 2020-04-19 2020-04-20 2020-04-21
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-04-22 2020-04-23 2020-04-24 2020-04-25
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-04-26 2020-04-27 2020-04-28 2020-04-29
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-04-30 2020-05-01 2020-05-02 2020-05-03
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-05-04 2020-05-05 2020-05-06 2020-05-07
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-05-08 2020-05-09 2020-05-10 2020-05-11
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-05-12 2020-05-13 2020-05-14 2020-05-15
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-05-16 2020-05-17 2020-05-18 2020-05-19
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-05-20 2020-05-21 2020-05-22 2020-05-23
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-05-24 2020-05-25 2020-05-26 2020-05-27
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-05-28 2020-05-29 2020-05-30 2020-05-31
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-06-01 2020-06-02 2020-06-03 2020-06-04
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-06-05 2020-06-06 2020-06-07 2020-06-08
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-06-09 2020-06-10 2020-06-11 2020-06-12
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-06-13 2020-06-14 2020-06-15 2020-06-16
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-06-17 2020-06-18 2020-06-19 2020-06-20
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA      0.125          1
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-06-21 2020-06-22 2020-06-23 2020-06-24
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                  1          1          1          1
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-06-25 2020-06-26 2020-06-27 2020-06-28
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                  1          1          1  0.9815046
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-06-29 2020-06-30 2020-07-01 2020-07-02
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-07-03 2020-07-04 2020-07-05 2020-07-06
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-07-07 2020-07-08 2020-07-09 2020-07-10
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-07-11 2020-07-12 2020-07-13 2020-07-14
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-07-15 2020-07-16 2020-07-17 2020-07-18
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-07-19 2020-07-20 2020-07-21 2020-07-22
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-07-23 2020-07-24 2020-07-25 2020-07-26
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-07-27 2020-07-28 2020-07-29 2020-07-30
#> B_DL_val 5_beek kleine vijver         NA         NA  0.7710532          1
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-07-31 2020-08-01 2020-08-02 2020-08-03
#> B_DL_val 5_beek kleine vijver          1          1          1          1
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-08-04 2020-08-05 2020-08-06 2020-08-07
#> B_DL_val 5_beek kleine vijver          1          1          1          1
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-08-08 2020-08-09 2020-08-10 2020-08-11
#> B_DL_val 5_beek kleine vijver  0.1810185         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-08-12 2020-08-13 2020-08-14 2020-08-15
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-08-16 2020-08-17 2020-08-18 2020-08-19
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-08-20 2020-08-21 2020-08-22 2020-08-23
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-08-24 2020-08-25 2020-08-26 2020-08-27
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-08-28 2020-08-29 2020-08-30 2020-08-31
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-09-01 2020-09-02 2020-09-03 2020-09-04
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-09-05 2020-09-06 2020-09-07 2020-09-08
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-09-09 2020-09-10 2020-09-11 2020-09-12
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-09-13 2020-09-14 2020-09-15 2020-09-16
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-09-17 2020-09-18 2020-09-19 2020-09-20
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-09-21 2020-09-22 2020-09-23 2020-09-24
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-09-25 2020-09-26 2020-09-27 2020-09-28
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-09-29 2020-09-30 2020-10-01 2020-10-02
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-10-03 2020-10-04 2020-10-05 2020-10-06
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-10-07 2020-10-08 2020-10-09 2020-10-10
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-10-11 2020-10-12 2020-10-13 2020-10-14
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-10-15 2020-10-16 2020-10-17 2020-10-18
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-10-19 2020-10-20 2020-10-21 2020-10-22
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-10-23 2020-10-24 2020-10-25 2020-10-26
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-10-27 2020-10-28 2020-10-29 2020-10-30
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-10-31 2020-11-01 2020-11-02 2020-11-03
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-11-04 2020-11-05 2020-11-06 2020-11-07
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-11-08 2020-11-09 2020-11-10 2020-11-11
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-11-12 2020-11-13 2020-11-14 2020-11-15
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-11-16 2020-11-17 2020-11-18 2020-11-19
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-11-20 2020-11-21 2020-11-22 2020-11-23
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-11-24 2020-11-25 2020-11-26 2020-11-27
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-11-28 2020-11-29 2020-11-30 2020-12-01
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-12-02 2020-12-03 2020-12-04 2020-12-05
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-12-06 2020-12-07 2020-12-08 2020-12-09
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-12-10 2020-12-11 2020-12-12 2020-12-13
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-12-14 2020-12-15 2020-12-16 2020-12-17
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-12-18 2020-12-19 2020-12-20 2020-12-21
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-12-22 2020-12-23 2020-12-24 2020-12-25
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-12-26 2020-12-27 2020-12-28 2020-12-29
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2020-12-30 2020-12-31 2021-01-01 2021-01-02
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-01-03 2021-01-04 2021-01-05 2021-01-06
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-01-07 2021-01-08 2021-01-09 2021-01-10
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-01-11 2021-01-12 2021-01-13 2021-01-14
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-01-15 2021-01-16 2021-01-17 2021-01-18
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-01-19 2021-01-20 2021-01-21 2021-01-22
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-01-23 2021-01-24 2021-01-25 2021-01-26
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-01-27 2021-01-28 2021-01-29 2021-01-30
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-01-31 2021-02-01 2021-02-02 2021-02-03
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-02-04 2021-02-05 2021-02-06 2021-02-07
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-02-08 2021-02-09 2021-02-10 2021-02-11
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-02-12 2021-02-13 2021-02-14 2021-02-15
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-02-16 2021-02-17 2021-02-18 2021-02-19
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-02-20 2021-02-21 2021-02-22 2021-02-23
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-02-24 2021-02-25 2021-02-26 2021-02-27
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-02-28 2021-03-01 2021-03-02 2021-03-03
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-03-04 2021-03-05 2021-03-06 2021-03-07
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-03-08 2021-03-09 2021-03-10 2021-03-11
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-03-12 2021-03-13 2021-03-14 2021-03-15
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-03-16 2021-03-17 2021-03-18 2021-03-19
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-03-20 2021-03-21 2021-03-22 2021-03-23
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA         NA
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-03-24 2021-03-25 2021-03-26 2021-03-27
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                     NA         NA         NA  0.1400694
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-03-28 2021-03-29 2021-03-30 2021-03-31
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                      1          1          1          1
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-04-01 2021-04-02 2021-04-03 2021-04-04
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                      1          1          1          1
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-04-05 2021-04-06 2021-04-07 2021-04-08
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                      1          1          1          1
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-04-09 2021-04-10 2021-04-11 2021-04-12
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                      1          1          1          1
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-04-13 2021-04-14 2021-04-15 2021-04-16
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#> B_DM_val 4_'t WAD                      1          1          1          1
#> Mica Viane                            NA         NA         NA         NA
#>                               2021-04-17 2021-04-18
#> B_DL_val 5_beek kleine vijver         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA
#> B_DM_val 4_'t WAD                      1  0.8923611
#> Mica Viane                            NA         NA

# Applying filter(s) on deployments, e.g. deployments with latitude >= 51.18
get_cam_op(mica, pred_gte("latitude", 51.18))
#> df %>% dplyr::filter((latitude >= 51.18))
#>                               2020-06-19 2020-06-20 2020-06-21 2020-06-22
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom              0.125          1          1          1
#>                               2020-06-23 2020-06-24 2020-06-25 2020-06-26
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                  1          1          1          1
#>                               2020-06-27 2020-06-28 2020-06-29 2020-06-30
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                  1  0.9815046         NA         NA
#>                               2020-07-01 2020-07-02 2020-07-03 2020-07-04
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#>                               2020-07-05 2020-07-06 2020-07-07 2020-07-08
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#>                               2020-07-09 2020-07-10 2020-07-11 2020-07-12
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#>                               2020-07-13 2020-07-14 2020-07-15 2020-07-16
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#>                               2020-07-17 2020-07-18 2020-07-19 2020-07-20
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#>                               2020-07-21 2020-07-22 2020-07-23 2020-07-24
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#>                               2020-07-25 2020-07-26 2020-07-27 2020-07-28
#> B_DL_val 5_beek kleine vijver         NA         NA         NA         NA
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#>                               2020-07-29 2020-07-30 2020-07-31 2020-08-01
#> B_DL_val 5_beek kleine vijver  0.7710532          1          1          1
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#>                               2020-08-02 2020-08-03 2020-08-04 2020-08-05
#> B_DL_val 5_beek kleine vijver          1          1          1          1
#> B_DL_val 3_dikke boom                 NA         NA         NA         NA
#>                               2020-08-06 2020-08-07 2020-08-08
#> B_DL_val 5_beek kleine vijver          1          1  0.1810185
#> B_DL_val 3_dikke boom                 NA         NA         NA

# Specify column with station names
get_cam_op(mica, station_col = "locationID")
#>                                      2019-10-09 2019-10-10 2019-10-11
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1  0.5290856          1          1
#>                                      2019-10-12 2019-10-13 2019-10-14
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1          1          1          1
#>                                      2019-10-15 2019-10-16 2019-10-17
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1          1          1          1
#>                                      2019-10-18 2019-10-19 2019-10-20
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1          1          1          1
#>                                      2019-10-21 2019-10-22 2019-10-23
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1          1          1  0.4168519
#>                                      2019-10-24 2019-10-25 2019-10-26
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-10-27 2019-10-28 2019-10-29
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-10-30 2019-10-31 2019-11-01
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-11-02 2019-11-03 2019-11-04
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-11-05 2019-11-06 2019-11-07
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-11-08 2019-11-09 2019-11-10
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-11-11 2019-11-12 2019-11-13
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-11-14 2019-11-15 2019-11-16
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-11-17 2019-11-18 2019-11-19
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-11-20 2019-11-21 2019-11-22
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-11-23 2019-11-24 2019-11-25
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-11-26 2019-11-27 2019-11-28
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-11-29 2019-11-30 2019-12-01
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-12-02 2019-12-03 2019-12-04
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-12-05 2019-12-06 2019-12-07
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-12-08 2019-12-09 2019-12-10
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-12-11 2019-12-12 2019-12-13
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-12-14 2019-12-15 2019-12-16
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-12-17 2019-12-18 2019-12-19
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-12-20 2019-12-21 2019-12-22
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-12-23 2019-12-24 2019-12-25
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-12-26 2019-12-27 2019-12-28
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2019-12-29 2019-12-30 2019-12-31
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-01 2020-01-02 2020-01-03
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-04 2020-01-05 2020-01-06
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-07 2020-01-08 2020-01-09
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-10 2020-01-11 2020-01-12
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-13 2020-01-14 2020-01-15
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-16 2020-01-17 2020-01-18
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-19 2020-01-20 2020-01-21
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-22 2020-01-23 2020-01-24
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-25 2020-01-26 2020-01-27
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-28 2020-01-29 2020-01-30
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-01-31 2020-02-01 2020-02-02
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-02-03 2020-02-04 2020-02-05
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-02-06 2020-02-07 2020-02-08
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-02-09 2020-02-10 2020-02-11
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-02-12 2020-02-13 2020-02-14
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-02-15 2020-02-16 2020-02-17
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-02-18 2020-02-19 2020-02-20
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-02-21 2020-02-22 2020-02-23
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-02-24 2020-02-25 2020-02-26
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-02-27 2020-02-28 2020-02-29
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-01 2020-03-02 2020-03-03
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-04 2020-03-05 2020-03-06
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-07 2020-03-08 2020-03-09
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-10 2020-03-11 2020-03-12
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-13 2020-03-14 2020-03-15
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-16 2020-03-17 2020-03-18
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-19 2020-03-20 2020-03-21
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-22 2020-03-23 2020-03-24
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-25 2020-03-26 2020-03-27
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-28 2020-03-29 2020-03-30
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-03-31 2020-04-01 2020-04-02
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-04-03 2020-04-04 2020-04-05
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-04-06 2020-04-07 2020-04-08
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-04-09 2020-04-10 2020-04-11
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-04-12 2020-04-13 2020-04-14
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-04-15 2020-04-16 2020-04-17
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-04-18 2020-04-19 2020-04-20
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-04-21 2020-04-22 2020-04-23
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-04-24 2020-04-25 2020-04-26
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-04-27 2020-04-28 2020-04-29
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-04-30 2020-05-01 2020-05-02
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-05-03 2020-05-04 2020-05-05
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-05-06 2020-05-07 2020-05-08
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-05-09 2020-05-10 2020-05-11
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-05-12 2020-05-13 2020-05-14
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-05-15 2020-05-16 2020-05-17
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-05-18 2020-05-19 2020-05-20
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-05-21 2020-05-22 2020-05-23
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-05-24 2020-05-25 2020-05-26
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-05-27 2020-05-28 2020-05-29
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-05-30 2020-05-31 2020-06-01
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-06-02 2020-06-03 2020-06-04
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-06-05 2020-06-06 2020-06-07
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-06-08 2020-06-09 2020-06-10
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-06-11 2020-06-12 2020-06-13
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-06-14 2020-06-15 2020-06-16
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-06-17 2020-06-18 2020-06-19
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA      0.125
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-06-20 2020-06-21 2020-06-22
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74          1          1          1
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-06-23 2020-06-24 2020-06-25
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74          1          1          1
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-06-26 2020-06-27 2020-06-28
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74          1          1  0.9815046
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-06-29 2020-06-30 2020-07-01
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-07-02 2020-07-03 2020-07-04
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-07-05 2020-07-06 2020-07-07
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-07-08 2020-07-09 2020-07-10
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-07-11 2020-07-12 2020-07-13
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-07-14 2020-07-15 2020-07-16
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-07-17 2020-07-18 2020-07-19
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-07-20 2020-07-21 2020-07-22
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-07-23 2020-07-24 2020-07-25
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-07-26 2020-07-27 2020-07-28
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-07-29 2020-07-30 2020-07-31
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f  0.7710532          1          1
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-01 2020-08-02 2020-08-03
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f          1          1          1
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-04 2020-08-05 2020-08-06
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f          1          1          1
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-07 2020-08-08 2020-08-09
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f          1  0.1810185         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-10 2020-08-11 2020-08-12
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-13 2020-08-14 2020-08-15
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-16 2020-08-17 2020-08-18
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-19 2020-08-20 2020-08-21
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-22 2020-08-23 2020-08-24
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-25 2020-08-26 2020-08-27
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-28 2020-08-29 2020-08-30
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-08-31 2020-09-01 2020-09-02
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-09-03 2020-09-04 2020-09-05
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-09-06 2020-09-07 2020-09-08
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-09-09 2020-09-10 2020-09-11
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-09-12 2020-09-13 2020-09-14
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-09-15 2020-09-16 2020-09-17
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-09-18 2020-09-19 2020-09-20
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-09-21 2020-09-22 2020-09-23
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-09-24 2020-09-25 2020-09-26
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-09-27 2020-09-28 2020-09-29
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-09-30 2020-10-01 2020-10-02
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-10-03 2020-10-04 2020-10-05
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-10-06 2020-10-07 2020-10-08
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-10-09 2020-10-10 2020-10-11
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-10-12 2020-10-13 2020-10-14
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-10-15 2020-10-16 2020-10-17
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-10-18 2020-10-19 2020-10-20
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-10-21 2020-10-22 2020-10-23
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-10-24 2020-10-25 2020-10-26
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-10-27 2020-10-28 2020-10-29
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-10-30 2020-10-31 2020-11-01
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-11-02 2020-11-03 2020-11-04
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-11-05 2020-11-06 2020-11-07
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-11-08 2020-11-09 2020-11-10
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-11-11 2020-11-12 2020-11-13
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-11-14 2020-11-15 2020-11-16
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-11-17 2020-11-18 2020-11-19
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-11-20 2020-11-21 2020-11-22
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-11-23 2020-11-24 2020-11-25
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-11-26 2020-11-27 2020-11-28
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-11-29 2020-11-30 2020-12-01
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-12-02 2020-12-03 2020-12-04
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-12-05 2020-12-06 2020-12-07
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-12-08 2020-12-09 2020-12-10
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-12-11 2020-12-12 2020-12-13
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-12-14 2020-12-15 2020-12-16
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-12-17 2020-12-18 2020-12-19
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-12-20 2020-12-21 2020-12-22
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-12-23 2020-12-24 2020-12-25
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-12-26 2020-12-27 2020-12-28
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2020-12-29 2020-12-30 2020-12-31
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-01 2021-01-02 2021-01-03
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-04 2021-01-05 2021-01-06
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-07 2021-01-08 2021-01-09
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-10 2021-01-11 2021-01-12
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-13 2021-01-14 2021-01-15
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-16 2021-01-17 2021-01-18
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-19 2021-01-20 2021-01-21
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-22 2021-01-23 2021-01-24
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-25 2021-01-26 2021-01-27
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-28 2021-01-29 2021-01-30
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-01-31 2021-02-01 2021-02-02
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-02-03 2021-02-04 2021-02-05
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-02-06 2021-02-07 2021-02-08
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-02-09 2021-02-10 2021-02-11
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-02-12 2021-02-13 2021-02-14
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-02-15 2021-02-16 2021-02-17
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-02-18 2021-02-19 2021-02-20
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-02-21 2021-02-22 2021-02-23
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-02-24 2021-02-25 2021-02-26
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-02-27 2021-02-28 2021-03-01
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-03-02 2021-03-03 2021-03-04
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-03-05 2021-03-06 2021-03-07
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-03-08 2021-03-09 2021-03-10
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-03-11 2021-03-12 2021-03-13
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-03-14 2021-03-15 2021-03-16
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-03-17 2021-03-18 2021-03-19
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-03-20 2021-03-21 2021-03-22
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-03-23 2021-03-24 2021-03-25
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-03-26 2021-03-27 2021-03-28
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA  0.1400694          1
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-03-29 2021-03-30 2021-03-31
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6          1          1          1
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-04-01 2021-04-02 2021-04-03
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6          1          1          1
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-04-04 2021-04-05 2021-04-06
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6          1          1          1
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-04-07 2021-04-08 2021-04-09
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6          1          1          1
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-04-10 2021-04-11 2021-04-12
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6          1          1          1
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-04-13 2021-04-14 2021-04-15
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6          1          1          1
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA
#>                                      2021-04-16 2021-04-17 2021-04-18
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6          1          1  0.8923611
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1         NA         NA         NA

# Specify column with session IDs
mica_sessions <- mica
mica_sessions$data$deployments <- mica_sessions$data$deployments %>%
  dplyr::mutate(session = ifelse(
    stringr::str_starts(.data$locationName, "B_DL_"),
      "after2020",
      "before2020"
  )
)
get_cam_op(mica_sessions, session_col = "session")
#>                                               2019-10-09 2019-10-10 2019-10-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                    0.5290856          1          1
#>                                               2019-10-12 2019-10-13 2019-10-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                            1          1          1
#>                                               2019-10-15 2019-10-16 2019-10-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                            1          1          1
#>                                               2019-10-18 2019-10-19 2019-10-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                            1          1          1
#>                                               2019-10-21 2019-10-22 2019-10-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                            1          1  0.4168519
#>                                               2019-10-24 2019-10-25 2019-10-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-10-27 2019-10-28 2019-10-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-10-30 2019-10-31 2019-11-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-11-02 2019-11-03 2019-11-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-11-05 2019-11-06 2019-11-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-11-08 2019-11-09 2019-11-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-11-11 2019-11-12 2019-11-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-11-14 2019-11-15 2019-11-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-11-17 2019-11-18 2019-11-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-11-20 2019-11-21 2019-11-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-11-23 2019-11-24 2019-11-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-11-26 2019-11-27 2019-11-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-11-29 2019-11-30 2019-12-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-12-02 2019-12-03 2019-12-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-12-05 2019-12-06 2019-12-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-12-08 2019-12-09 2019-12-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-12-11 2019-12-12 2019-12-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-12-14 2019-12-15 2019-12-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-12-17 2019-12-18 2019-12-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-12-20 2019-12-21 2019-12-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-12-23 2019-12-24 2019-12-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-12-26 2019-12-27 2019-12-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2019-12-29 2019-12-30 2019-12-31
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-01 2020-01-02 2020-01-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-04 2020-01-05 2020-01-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-07 2020-01-08 2020-01-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-10 2020-01-11 2020-01-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-13 2020-01-14 2020-01-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-16 2020-01-17 2020-01-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-19 2020-01-20 2020-01-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-22 2020-01-23 2020-01-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-25 2020-01-26 2020-01-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-28 2020-01-29 2020-01-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-01-31 2020-02-01 2020-02-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-02-03 2020-02-04 2020-02-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-02-06 2020-02-07 2020-02-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-02-09 2020-02-10 2020-02-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-02-12 2020-02-13 2020-02-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-02-15 2020-02-16 2020-02-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-02-18 2020-02-19 2020-02-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-02-21 2020-02-22 2020-02-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-02-24 2020-02-25 2020-02-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-02-27 2020-02-28 2020-02-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-01 2020-03-02 2020-03-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-04 2020-03-05 2020-03-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-07 2020-03-08 2020-03-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-10 2020-03-11 2020-03-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-13 2020-03-14 2020-03-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-16 2020-03-17 2020-03-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-19 2020-03-20 2020-03-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-22 2020-03-23 2020-03-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-25 2020-03-26 2020-03-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-28 2020-03-29 2020-03-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-03-31 2020-04-01 2020-04-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-04-03 2020-04-04 2020-04-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-04-06 2020-04-07 2020-04-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-04-09 2020-04-10 2020-04-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-04-12 2020-04-13 2020-04-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-04-15 2020-04-16 2020-04-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-04-18 2020-04-19 2020-04-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-04-21 2020-04-22 2020-04-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-04-24 2020-04-25 2020-04-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-04-27 2020-04-28 2020-04-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-04-30 2020-05-01 2020-05-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-05-03 2020-05-04 2020-05-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-05-06 2020-05-07 2020-05-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-05-09 2020-05-10 2020-05-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-05-12 2020-05-13 2020-05-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-05-15 2020-05-16 2020-05-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-05-18 2020-05-19 2020-05-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-05-21 2020-05-22 2020-05-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-05-24 2020-05-25 2020-05-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-05-27 2020-05-28 2020-05-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-05-30 2020-05-31 2020-06-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-06-02 2020-06-03 2020-06-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-06-05 2020-06-06 2020-06-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-06-08 2020-06-09 2020-06-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-06-11 2020-06-12 2020-06-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-06-14 2020-06-15 2020-06-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-06-17 2020-06-18 2020-06-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA      0.125
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-06-20 2020-06-21 2020-06-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                  1          1          1
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-06-23 2020-06-24 2020-06-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                  1          1          1
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-06-26 2020-06-27 2020-06-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                  1          1  0.9815046
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-06-29 2020-06-30 2020-07-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-07-02 2020-07-03 2020-07-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-07-05 2020-07-06 2020-07-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-07-08 2020-07-09 2020-07-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-07-11 2020-07-12 2020-07-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-07-14 2020-07-15 2020-07-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-07-17 2020-07-18 2020-07-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-07-20 2020-07-21 2020-07-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-07-23 2020-07-24 2020-07-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-07-26 2020-07-27 2020-07-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-07-29 2020-07-30 2020-07-31
#> B_DL_val 5_beek kleine vijver__SESS_after2020  0.7710532          1          1
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-01 2020-08-02 2020-08-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020          1          1          1
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-04 2020-08-05 2020-08-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020          1          1          1
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-07 2020-08-08 2020-08-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020          1  0.1810185         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-10 2020-08-11 2020-08-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-13 2020-08-14 2020-08-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-16 2020-08-17 2020-08-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-19 2020-08-20 2020-08-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-22 2020-08-23 2020-08-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-25 2020-08-26 2020-08-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-28 2020-08-29 2020-08-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-08-31 2020-09-01 2020-09-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-09-03 2020-09-04 2020-09-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-09-06 2020-09-07 2020-09-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-09-09 2020-09-10 2020-09-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-09-12 2020-09-13 2020-09-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-09-15 2020-09-16 2020-09-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-09-18 2020-09-19 2020-09-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-09-21 2020-09-22 2020-09-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-09-24 2020-09-25 2020-09-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-09-27 2020-09-28 2020-09-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-09-30 2020-10-01 2020-10-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-10-03 2020-10-04 2020-10-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-10-06 2020-10-07 2020-10-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-10-09 2020-10-10 2020-10-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-10-12 2020-10-13 2020-10-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-10-15 2020-10-16 2020-10-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-10-18 2020-10-19 2020-10-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-10-21 2020-10-22 2020-10-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-10-24 2020-10-25 2020-10-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-10-27 2020-10-28 2020-10-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-10-30 2020-10-31 2020-11-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-11-02 2020-11-03 2020-11-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-11-05 2020-11-06 2020-11-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-11-08 2020-11-09 2020-11-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-11-11 2020-11-12 2020-11-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-11-14 2020-11-15 2020-11-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-11-17 2020-11-18 2020-11-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-11-20 2020-11-21 2020-11-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-11-23 2020-11-24 2020-11-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-11-26 2020-11-27 2020-11-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-11-29 2020-11-30 2020-12-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-12-02 2020-12-03 2020-12-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-12-05 2020-12-06 2020-12-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-12-08 2020-12-09 2020-12-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-12-11 2020-12-12 2020-12-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-12-14 2020-12-15 2020-12-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-12-17 2020-12-18 2020-12-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-12-20 2020-12-21 2020-12-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-12-23 2020-12-24 2020-12-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-12-26 2020-12-27 2020-12-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2020-12-29 2020-12-30 2020-12-31
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-01 2021-01-02 2021-01-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-04 2021-01-05 2021-01-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-07 2021-01-08 2021-01-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-10 2021-01-11 2021-01-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-13 2021-01-14 2021-01-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-16 2021-01-17 2021-01-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-19 2021-01-20 2021-01-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-22 2021-01-23 2021-01-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-25 2021-01-26 2021-01-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-28 2021-01-29 2021-01-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-01-31 2021-02-01 2021-02-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-02-03 2021-02-04 2021-02-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-02-06 2021-02-07 2021-02-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-02-09 2021-02-10 2021-02-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-02-12 2021-02-13 2021-02-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-02-15 2021-02-16 2021-02-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-02-18 2021-02-19 2021-02-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-02-21 2021-02-22 2021-02-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-02-24 2021-02-25 2021-02-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-02-27 2021-02-28 2021-03-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-03-02 2021-03-03 2021-03-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-03-05 2021-03-06 2021-03-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-03-08 2021-03-09 2021-03-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-03-11 2021-03-12 2021-03-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-03-14 2021-03-15 2021-03-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-03-17 2021-03-18 2021-03-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-03-20 2021-03-21 2021-03-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-03-23 2021-03-24 2021-03-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA         NA         NA
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-03-26 2021-03-27 2021-03-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                    NA  0.1400694          1
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-03-29 2021-03-30 2021-03-31
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-04-01 2021-04-02 2021-04-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-04-04 2021-04-05 2021-04-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-04-07 2021-04-08 2021-04-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-04-10 2021-04-11 2021-04-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-04-13 2021-04-14 2021-04-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1          1
#> Mica Viane__SESS_before2020                           NA         NA         NA
#>                                               2021-04-16 2021-04-17 2021-04-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020         NA         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020                 NA         NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020                     1          1  0.8923611
#> Mica Viane__SESS_before2020                           NA         NA         NA

# Specify column with camera IDs
mica_cameras <- mica_sessions
mica_cameras$data$deployments$cameraID <- c(1, 2, 3, 4)
get_cam_op(mica_cameras, camera_col = "cameraID")
#>                                      2019-10-09 2019-10-10 2019-10-11
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                     0.5290856          1          1
#>                                      2019-10-12 2019-10-13 2019-10-14
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                             1          1          1
#>                                      2019-10-15 2019-10-16 2019-10-17
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                             1          1          1
#>                                      2019-10-18 2019-10-19 2019-10-20
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                             1          1          1
#>                                      2019-10-21 2019-10-22 2019-10-23
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                             1          1  0.4168519
#>                                      2019-10-24 2019-10-25 2019-10-26
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-10-27 2019-10-28 2019-10-29
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-10-30 2019-10-31 2019-11-01
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-11-02 2019-11-03 2019-11-04
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-11-05 2019-11-06 2019-11-07
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-11-08 2019-11-09 2019-11-10
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-11-11 2019-11-12 2019-11-13
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-11-14 2019-11-15 2019-11-16
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-11-17 2019-11-18 2019-11-19
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-11-20 2019-11-21 2019-11-22
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-11-23 2019-11-24 2019-11-25
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-11-26 2019-11-27 2019-11-28
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-11-29 2019-11-30 2019-12-01
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-12-02 2019-12-03 2019-12-04
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-12-05 2019-12-06 2019-12-07
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-12-08 2019-12-09 2019-12-10
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-12-11 2019-12-12 2019-12-13
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-12-14 2019-12-15 2019-12-16
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-12-17 2019-12-18 2019-12-19
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-12-20 2019-12-21 2019-12-22
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-12-23 2019-12-24 2019-12-25
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-12-26 2019-12-27 2019-12-28
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2019-12-29 2019-12-30 2019-12-31
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-01 2020-01-02 2020-01-03
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-04 2020-01-05 2020-01-06
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-07 2020-01-08 2020-01-09
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-10 2020-01-11 2020-01-12
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-13 2020-01-14 2020-01-15
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-16 2020-01-17 2020-01-18
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-19 2020-01-20 2020-01-21
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-22 2020-01-23 2020-01-24
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-25 2020-01-26 2020-01-27
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-28 2020-01-29 2020-01-30
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-01-31 2020-02-01 2020-02-02
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-02-03 2020-02-04 2020-02-05
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-02-06 2020-02-07 2020-02-08
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-02-09 2020-02-10 2020-02-11
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-02-12 2020-02-13 2020-02-14
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-02-15 2020-02-16 2020-02-17
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-02-18 2020-02-19 2020-02-20
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-02-21 2020-02-22 2020-02-23
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-02-24 2020-02-25 2020-02-26
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-02-27 2020-02-28 2020-02-29
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-01 2020-03-02 2020-03-03
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-04 2020-03-05 2020-03-06
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-07 2020-03-08 2020-03-09
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-10 2020-03-11 2020-03-12
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-13 2020-03-14 2020-03-15
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-16 2020-03-17 2020-03-18
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-19 2020-03-20 2020-03-21
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-22 2020-03-23 2020-03-24
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-25 2020-03-26 2020-03-27
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-28 2020-03-29 2020-03-30
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-03-31 2020-04-01 2020-04-02
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-04-03 2020-04-04 2020-04-05
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-04-06 2020-04-07 2020-04-08
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-04-09 2020-04-10 2020-04-11
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-04-12 2020-04-13 2020-04-14
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-04-15 2020-04-16 2020-04-17
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-04-18 2020-04-19 2020-04-20
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-04-21 2020-04-22 2020-04-23
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-04-24 2020-04-25 2020-04-26
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-04-27 2020-04-28 2020-04-29
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-04-30 2020-05-01 2020-05-02
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-05-03 2020-05-04 2020-05-05
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-05-06 2020-05-07 2020-05-08
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-05-09 2020-05-10 2020-05-11
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-05-12 2020-05-13 2020-05-14
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-05-15 2020-05-16 2020-05-17
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-05-18 2020-05-19 2020-05-20
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-05-21 2020-05-22 2020-05-23
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-05-24 2020-05-25 2020-05-26
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-05-27 2020-05-28 2020-05-29
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-05-30 2020-05-31 2020-06-01
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-06-02 2020-06-03 2020-06-04
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-06-05 2020-06-06 2020-06-07
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-06-08 2020-06-09 2020-06-10
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-06-11 2020-06-12 2020-06-13
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-06-14 2020-06-15 2020-06-16
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-06-17 2020-06-18 2020-06-19
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA      0.125
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-06-20 2020-06-21 2020-06-22
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                  1          1          1
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-06-23 2020-06-24 2020-06-25
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                  1          1          1
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-06-26 2020-06-27 2020-06-28
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                  1          1  0.9815046
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-06-29 2020-06-30 2020-07-01
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-07-02 2020-07-03 2020-07-04
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-07-05 2020-07-06 2020-07-07
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-07-08 2020-07-09 2020-07-10
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-07-11 2020-07-12 2020-07-13
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-07-14 2020-07-15 2020-07-16
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-07-17 2020-07-18 2020-07-19
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-07-20 2020-07-21 2020-07-22
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-07-23 2020-07-24 2020-07-25
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-07-26 2020-07-27 2020-07-28
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-07-29 2020-07-30 2020-07-31
#> B_DL_val 5_beek kleine vijver__CAM_1  0.7710532          1          1
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-01 2020-08-02 2020-08-03
#> B_DL_val 5_beek kleine vijver__CAM_1          1          1          1
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-04 2020-08-05 2020-08-06
#> B_DL_val 5_beek kleine vijver__CAM_1          1          1          1
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-07 2020-08-08 2020-08-09
#> B_DL_val 5_beek kleine vijver__CAM_1          1  0.1810185         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-10 2020-08-11 2020-08-12
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-13 2020-08-14 2020-08-15
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-16 2020-08-17 2020-08-18
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-19 2020-08-20 2020-08-21
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-22 2020-08-23 2020-08-24
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-25 2020-08-26 2020-08-27
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-28 2020-08-29 2020-08-30
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-08-31 2020-09-01 2020-09-02
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-09-03 2020-09-04 2020-09-05
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-09-06 2020-09-07 2020-09-08
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-09-09 2020-09-10 2020-09-11
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-09-12 2020-09-13 2020-09-14
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-09-15 2020-09-16 2020-09-17
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-09-18 2020-09-19 2020-09-20
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-09-21 2020-09-22 2020-09-23
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-09-24 2020-09-25 2020-09-26
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-09-27 2020-09-28 2020-09-29
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-09-30 2020-10-01 2020-10-02
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-10-03 2020-10-04 2020-10-05
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-10-06 2020-10-07 2020-10-08
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-10-09 2020-10-10 2020-10-11
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-10-12 2020-10-13 2020-10-14
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-10-15 2020-10-16 2020-10-17
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-10-18 2020-10-19 2020-10-20
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-10-21 2020-10-22 2020-10-23
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-10-24 2020-10-25 2020-10-26
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-10-27 2020-10-28 2020-10-29
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-10-30 2020-10-31 2020-11-01
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-11-02 2020-11-03 2020-11-04
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-11-05 2020-11-06 2020-11-07
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-11-08 2020-11-09 2020-11-10
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-11-11 2020-11-12 2020-11-13
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-11-14 2020-11-15 2020-11-16
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-11-17 2020-11-18 2020-11-19
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-11-20 2020-11-21 2020-11-22
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-11-23 2020-11-24 2020-11-25
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-11-26 2020-11-27 2020-11-28
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-11-29 2020-11-30 2020-12-01
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-12-02 2020-12-03 2020-12-04
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-12-05 2020-12-06 2020-12-07
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-12-08 2020-12-09 2020-12-10
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-12-11 2020-12-12 2020-12-13
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-12-14 2020-12-15 2020-12-16
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-12-17 2020-12-18 2020-12-19
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-12-20 2020-12-21 2020-12-22
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-12-23 2020-12-24 2020-12-25
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-12-26 2020-12-27 2020-12-28
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2020-12-29 2020-12-30 2020-12-31
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-01 2021-01-02 2021-01-03
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-04 2021-01-05 2021-01-06
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-07 2021-01-08 2021-01-09
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-10 2021-01-11 2021-01-12
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-13 2021-01-14 2021-01-15
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-16 2021-01-17 2021-01-18
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-19 2021-01-20 2021-01-21
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-22 2021-01-23 2021-01-24
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-25 2021-01-26 2021-01-27
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-28 2021-01-29 2021-01-30
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-01-31 2021-02-01 2021-02-02
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-02-03 2021-02-04 2021-02-05
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-02-06 2021-02-07 2021-02-08
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-02-09 2021-02-10 2021-02-11
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-02-12 2021-02-13 2021-02-14
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-02-15 2021-02-16 2021-02-17
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-02-18 2021-02-19 2021-02-20
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-02-21 2021-02-22 2021-02-23
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-02-24 2021-02-25 2021-02-26
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-02-27 2021-02-28 2021-03-01
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-03-02 2021-03-03 2021-03-04
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-03-05 2021-03-06 2021-03-07
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-03-08 2021-03-09 2021-03-10
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-03-11 2021-03-12 2021-03-13
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-03-14 2021-03-15 2021-03-16
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-03-17 2021-03-18 2021-03-19
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-03-20 2021-03-21 2021-03-22
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-03-23 2021-03-24 2021-03-25
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA         NA         NA
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-03-26 2021-03-27 2021-03-28
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                     NA  0.1400694          1
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-03-29 2021-03-30 2021-03-31
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                      1          1          1
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-04-01 2021-04-02 2021-04-03
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                      1          1          1
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-04-04 2021-04-05 2021-04-06
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                      1          1          1
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-04-07 2021-04-08 2021-04-09
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                      1          1          1
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-04-10 2021-04-11 2021-04-12
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                      1          1          1
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-04-13 2021-04-14 2021-04-15
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                      1          1          1
#> Mica Viane__CAM_4                            NA         NA         NA
#>                                      2021-04-16 2021-04-17 2021-04-18
#> B_DL_val 5_beek kleine vijver__CAM_1         NA         NA         NA
#> B_DL_val 3_dikke boom__CAM_2                 NA         NA         NA
#> B_DM_val 4_'t WAD__CAM_3                      1          1  0.8923611
#> Mica Viane__CAM_4                            NA         NA         NA

# Specify both session and camera IDs
get_cam_op(mica_cameras, camera_col = "cameraID", session_col = "session")
#>                                                      2019-10-09 2019-10-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                    0.5290856          1
#>                                                      2019-10-11 2019-10-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                            1          1
#>                                                      2019-10-13 2019-10-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                            1          1
#>                                                      2019-10-15 2019-10-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                            1          1
#>                                                      2019-10-17 2019-10-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                            1          1
#>                                                      2019-10-19 2019-10-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                            1          1
#>                                                      2019-10-21 2019-10-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                            1          1
#>                                                      2019-10-23 2019-10-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                    0.4168519         NA
#>                                                      2019-10-25 2019-10-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-10-27 2019-10-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-10-29 2019-10-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-10-31 2019-11-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-02 2019-11-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-04 2019-11-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-06 2019-11-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-08 2019-11-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-10 2019-11-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-12 2019-11-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-14 2019-11-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-16 2019-11-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-18 2019-11-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-20 2019-11-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-22 2019-11-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-24 2019-11-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-26 2019-11-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-28 2019-11-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-11-30 2019-12-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-02 2019-12-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-04 2019-12-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-06 2019-12-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-08 2019-12-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-10 2019-12-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-12 2019-12-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-14 2019-12-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-16 2019-12-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-18 2019-12-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-20 2019-12-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-22 2019-12-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-24 2019-12-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-26 2019-12-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-28 2019-12-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2019-12-30 2019-12-31
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-01 2020-01-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-03 2020-01-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-05 2020-01-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-07 2020-01-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-09 2020-01-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-11 2020-01-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-13 2020-01-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-15 2020-01-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-17 2020-01-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-19 2020-01-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-21 2020-01-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-23 2020-01-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-25 2020-01-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-27 2020-01-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-29 2020-01-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-01-31 2020-02-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-02 2020-02-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-04 2020-02-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-06 2020-02-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-08 2020-02-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-10 2020-02-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-12 2020-02-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-14 2020-02-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-16 2020-02-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-18 2020-02-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-20 2020-02-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-22 2020-02-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-24 2020-02-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-26 2020-02-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-02-28 2020-02-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-01 2020-03-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-03 2020-03-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-05 2020-03-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-07 2020-03-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-09 2020-03-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-11 2020-03-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-13 2020-03-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-15 2020-03-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-17 2020-03-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-19 2020-03-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-21 2020-03-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-23 2020-03-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-25 2020-03-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-27 2020-03-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-29 2020-03-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-03-31 2020-04-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-02 2020-04-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-04 2020-04-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-06 2020-04-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-08 2020-04-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-10 2020-04-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-12 2020-04-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-14 2020-04-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-16 2020-04-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-18 2020-04-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-20 2020-04-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-22 2020-04-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-24 2020-04-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-26 2020-04-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-28 2020-04-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-04-30 2020-05-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-02 2020-05-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-04 2020-05-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-06 2020-05-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-08 2020-05-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-10 2020-05-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-12 2020-05-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-14 2020-05-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-16 2020-05-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-18 2020-05-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-20 2020-05-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-22 2020-05-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-24 2020-05-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-26 2020-05-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-28 2020-05-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-05-30 2020-05-31
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-01 2020-06-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-03 2020-06-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-05 2020-06-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-07 2020-06-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-09 2020-06-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-11 2020-06-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-13 2020-06-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-15 2020-06-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-17 2020-06-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-19 2020-06-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2              0.125          1
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-21 2020-06-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                  1          1
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-23 2020-06-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                  1          1
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-25 2020-06-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                  1          1
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-27 2020-06-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                  1  0.9815046
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-06-29 2020-06-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-01 2020-07-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-03 2020-07-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-05 2020-07-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-07 2020-07-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-09 2020-07-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-11 2020-07-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-13 2020-07-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-15 2020-07-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-17 2020-07-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-19 2020-07-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-21 2020-07-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-23 2020-07-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-25 2020-07-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-27 2020-07-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-29 2020-07-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1  0.7710532          1
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-07-31 2020-08-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1          1          1
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-02 2020-08-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1          1          1
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-04 2020-08-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1          1          1
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-06 2020-08-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1          1          1
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-08 2020-08-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1  0.1810185         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-10 2020-08-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-12 2020-08-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-14 2020-08-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-16 2020-08-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-18 2020-08-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-20 2020-08-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-22 2020-08-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-24 2020-08-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-26 2020-08-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-28 2020-08-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-08-30 2020-08-31
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-01 2020-09-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-03 2020-09-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-05 2020-09-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-07 2020-09-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-09 2020-09-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-11 2020-09-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-13 2020-09-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-15 2020-09-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-17 2020-09-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-19 2020-09-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-21 2020-09-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-23 2020-09-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-25 2020-09-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-27 2020-09-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-09-29 2020-09-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-01 2020-10-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-03 2020-10-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-05 2020-10-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-07 2020-10-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-09 2020-10-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-11 2020-10-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-13 2020-10-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-15 2020-10-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-17 2020-10-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-19 2020-10-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-21 2020-10-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-23 2020-10-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-25 2020-10-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-27 2020-10-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-29 2020-10-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-10-31 2020-11-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-02 2020-11-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-04 2020-11-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-06 2020-11-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-08 2020-11-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-10 2020-11-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-12 2020-11-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-14 2020-11-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-16 2020-11-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-18 2020-11-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-20 2020-11-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-22 2020-11-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-24 2020-11-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-26 2020-11-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-28 2020-11-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-11-30 2020-12-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-02 2020-12-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-04 2020-12-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-06 2020-12-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-08 2020-12-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-10 2020-12-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-12 2020-12-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-14 2020-12-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-16 2020-12-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-18 2020-12-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-20 2020-12-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-22 2020-12-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-24 2020-12-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-26 2020-12-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-28 2020-12-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2020-12-30 2020-12-31
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-01 2021-01-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-03 2021-01-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-05 2021-01-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-07 2021-01-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-09 2021-01-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-11 2021-01-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-13 2021-01-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-15 2021-01-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-17 2021-01-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-19 2021-01-20
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-21 2021-01-22
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-23 2021-01-24
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-25 2021-01-26
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-27 2021-01-28
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-29 2021-01-30
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-01-31 2021-02-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-02 2021-02-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-04 2021-02-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-06 2021-02-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-08 2021-02-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-10 2021-02-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-12 2021-02-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-14 2021-02-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-16 2021-02-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-18 2021-02-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-20 2021-02-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-22 2021-02-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-24 2021-02-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-26 2021-02-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-02-28 2021-03-01
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-02 2021-03-03
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-04 2021-03-05
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-06 2021-03-07
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-08 2021-03-09
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-10 2021-03-11
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-12 2021-03-13
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-14 2021-03-15
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-16 2021-03-17
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-18 2021-03-19
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-20 2021-03-21
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-22 2021-03-23
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-24 2021-03-25
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA         NA
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-26 2021-03-27
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                    NA  0.1400694
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-28 2021-03-29
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1          1
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-03-30 2021-03-31
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1          1
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-04-01 2021-04-02
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1          1
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-04-03 2021-04-04
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1          1
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-04-05 2021-04-06
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1          1
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-04-07 2021-04-08
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1          1
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-04-09 2021-04-10
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1          1
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-04-11 2021-04-12
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1          1
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-04-13 2021-04-14
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1          1
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-04-15 2021-04-16
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1          1
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA
#>                                                      2021-04-17 2021-04-18
#> B_DL_val 5_beek kleine vijver__SESS_after2020__CAM_1         NA         NA
#> B_DL_val 3_dikke boom__SESS_after2020__CAM_2                 NA         NA
#> B_DM_val 4_'t WAD__SESS_before2020__CAM_3                     1  0.8923611
#> Mica Viane__SESS_before2020__CAM_4                           NA         NA

# Use prefix Station as in camtrapR's camera operation matrix
get_cam_op(mica, use_prefix = TRUE)
#>                                      2019-10-09 2019-10-10 2019-10-11
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                     0.5290856          1          1
#>                                      2019-10-12 2019-10-13 2019-10-14
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                             1          1          1
#>                                      2019-10-15 2019-10-16 2019-10-17
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                             1          1          1
#>                                      2019-10-18 2019-10-19 2019-10-20
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                             1          1          1
#>                                      2019-10-21 2019-10-22 2019-10-23
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                             1          1  0.4168519
#>                                      2019-10-24 2019-10-25 2019-10-26
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-10-27 2019-10-28 2019-10-29
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-10-30 2019-10-31 2019-11-01
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-11-02 2019-11-03 2019-11-04
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-11-05 2019-11-06 2019-11-07
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-11-08 2019-11-09 2019-11-10
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-11-11 2019-11-12 2019-11-13
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-11-14 2019-11-15 2019-11-16
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-11-17 2019-11-18 2019-11-19
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-11-20 2019-11-21 2019-11-22
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-11-23 2019-11-24 2019-11-25
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-11-26 2019-11-27 2019-11-28
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-11-29 2019-11-30 2019-12-01
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-12-02 2019-12-03 2019-12-04
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-12-05 2019-12-06 2019-12-07
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-12-08 2019-12-09 2019-12-10
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-12-11 2019-12-12 2019-12-13
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-12-14 2019-12-15 2019-12-16
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-12-17 2019-12-18 2019-12-19
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-12-20 2019-12-21 2019-12-22
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-12-23 2019-12-24 2019-12-25
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-12-26 2019-12-27 2019-12-28
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2019-12-29 2019-12-30 2019-12-31
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-01 2020-01-02 2020-01-03
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-04 2020-01-05 2020-01-06
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-07 2020-01-08 2020-01-09
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-10 2020-01-11 2020-01-12
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-13 2020-01-14 2020-01-15
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-16 2020-01-17 2020-01-18
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-19 2020-01-20 2020-01-21
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-22 2020-01-23 2020-01-24
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-25 2020-01-26 2020-01-27
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-28 2020-01-29 2020-01-30
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-01-31 2020-02-01 2020-02-02
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-02-03 2020-02-04 2020-02-05
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-02-06 2020-02-07 2020-02-08
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-02-09 2020-02-10 2020-02-11
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-02-12 2020-02-13 2020-02-14
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-02-15 2020-02-16 2020-02-17
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-02-18 2020-02-19 2020-02-20
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-02-21 2020-02-22 2020-02-23
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-02-24 2020-02-25 2020-02-26
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-02-27 2020-02-28 2020-02-29
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-01 2020-03-02 2020-03-03
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-04 2020-03-05 2020-03-06
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-07 2020-03-08 2020-03-09
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-10 2020-03-11 2020-03-12
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-13 2020-03-14 2020-03-15
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-16 2020-03-17 2020-03-18
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-19 2020-03-20 2020-03-21
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-22 2020-03-23 2020-03-24
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-25 2020-03-26 2020-03-27
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-28 2020-03-29 2020-03-30
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-03-31 2020-04-01 2020-04-02
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-04-03 2020-04-04 2020-04-05
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-04-06 2020-04-07 2020-04-08
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-04-09 2020-04-10 2020-04-11
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-04-12 2020-04-13 2020-04-14
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-04-15 2020-04-16 2020-04-17
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-04-18 2020-04-19 2020-04-20
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-04-21 2020-04-22 2020-04-23
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-04-24 2020-04-25 2020-04-26
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-04-27 2020-04-28 2020-04-29
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-04-30 2020-05-01 2020-05-02
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-05-03 2020-05-04 2020-05-05
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-05-06 2020-05-07 2020-05-08
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-05-09 2020-05-10 2020-05-11
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-05-12 2020-05-13 2020-05-14
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-05-15 2020-05-16 2020-05-17
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-05-18 2020-05-19 2020-05-20
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-05-21 2020-05-22 2020-05-23
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-05-24 2020-05-25 2020-05-26
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-05-27 2020-05-28 2020-05-29
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-05-30 2020-05-31 2020-06-01
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-06-02 2020-06-03 2020-06-04
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-06-05 2020-06-06 2020-06-07
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-06-08 2020-06-09 2020-06-10
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-06-11 2020-06-12 2020-06-13
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-06-14 2020-06-15 2020-06-16
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-06-17 2020-06-18 2020-06-19
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA      0.125
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-06-20 2020-06-21 2020-06-22
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                  1          1          1
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-06-23 2020-06-24 2020-06-25
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                  1          1          1
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-06-26 2020-06-27 2020-06-28
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                  1          1  0.9815046
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-06-29 2020-06-30 2020-07-01
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-07-02 2020-07-03 2020-07-04
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-07-05 2020-07-06 2020-07-07
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-07-08 2020-07-09 2020-07-10
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-07-11 2020-07-12 2020-07-13
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-07-14 2020-07-15 2020-07-16
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-07-17 2020-07-18 2020-07-19
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-07-20 2020-07-21 2020-07-22
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-07-23 2020-07-24 2020-07-25
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-07-26 2020-07-27 2020-07-28
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-07-29 2020-07-30 2020-07-31
#> StationB_DL_val 5_beek kleine vijver  0.7710532          1          1
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-01 2020-08-02 2020-08-03
#> StationB_DL_val 5_beek kleine vijver          1          1          1
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-04 2020-08-05 2020-08-06
#> StationB_DL_val 5_beek kleine vijver          1          1          1
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-07 2020-08-08 2020-08-09
#> StationB_DL_val 5_beek kleine vijver          1  0.1810185         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-10 2020-08-11 2020-08-12
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-13 2020-08-14 2020-08-15
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-16 2020-08-17 2020-08-18
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-19 2020-08-20 2020-08-21
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-22 2020-08-23 2020-08-24
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-25 2020-08-26 2020-08-27
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-28 2020-08-29 2020-08-30
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-08-31 2020-09-01 2020-09-02
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-09-03 2020-09-04 2020-09-05
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-09-06 2020-09-07 2020-09-08
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-09-09 2020-09-10 2020-09-11
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-09-12 2020-09-13 2020-09-14
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-09-15 2020-09-16 2020-09-17
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-09-18 2020-09-19 2020-09-20
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-09-21 2020-09-22 2020-09-23
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-09-24 2020-09-25 2020-09-26
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-09-27 2020-09-28 2020-09-29
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-09-30 2020-10-01 2020-10-02
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-10-03 2020-10-04 2020-10-05
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-10-06 2020-10-07 2020-10-08
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-10-09 2020-10-10 2020-10-11
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-10-12 2020-10-13 2020-10-14
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-10-15 2020-10-16 2020-10-17
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-10-18 2020-10-19 2020-10-20
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-10-21 2020-10-22 2020-10-23
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-10-24 2020-10-25 2020-10-26
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-10-27 2020-10-28 2020-10-29
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-10-30 2020-10-31 2020-11-01
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-11-02 2020-11-03 2020-11-04
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-11-05 2020-11-06 2020-11-07
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-11-08 2020-11-09 2020-11-10
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-11-11 2020-11-12 2020-11-13
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-11-14 2020-11-15 2020-11-16
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-11-17 2020-11-18 2020-11-19
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-11-20 2020-11-21 2020-11-22
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-11-23 2020-11-24 2020-11-25
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-11-26 2020-11-27 2020-11-28
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-11-29 2020-11-30 2020-12-01
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-12-02 2020-12-03 2020-12-04
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-12-05 2020-12-06 2020-12-07
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-12-08 2020-12-09 2020-12-10
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-12-11 2020-12-12 2020-12-13
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-12-14 2020-12-15 2020-12-16
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-12-17 2020-12-18 2020-12-19
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-12-20 2020-12-21 2020-12-22
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-12-23 2020-12-24 2020-12-25
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-12-26 2020-12-27 2020-12-28
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2020-12-29 2020-12-30 2020-12-31
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-01 2021-01-02 2021-01-03
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-04 2021-01-05 2021-01-06
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-07 2021-01-08 2021-01-09
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-10 2021-01-11 2021-01-12
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-13 2021-01-14 2021-01-15
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-16 2021-01-17 2021-01-18
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-19 2021-01-20 2021-01-21
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-22 2021-01-23 2021-01-24
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-25 2021-01-26 2021-01-27
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-28 2021-01-29 2021-01-30
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-01-31 2021-02-01 2021-02-02
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-02-03 2021-02-04 2021-02-05
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-02-06 2021-02-07 2021-02-08
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-02-09 2021-02-10 2021-02-11
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-02-12 2021-02-13 2021-02-14
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-02-15 2021-02-16 2021-02-17
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-02-18 2021-02-19 2021-02-20
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-02-21 2021-02-22 2021-02-23
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-02-24 2021-02-25 2021-02-26
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-02-27 2021-02-28 2021-03-01
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-03-02 2021-03-03 2021-03-04
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-03-05 2021-03-06 2021-03-07
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-03-08 2021-03-09 2021-03-10
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-03-11 2021-03-12 2021-03-13
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-03-14 2021-03-15 2021-03-16
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-03-17 2021-03-18 2021-03-19
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-03-20 2021-03-21 2021-03-22
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-03-23 2021-03-24 2021-03-25
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA         NA
#> StationMica Viane                            NA         NA         NA
#>                                      2021-03-26 2021-03-27 2021-03-28
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                     NA  0.1400694          1
#> StationMica Viane                            NA         NA         NA
#>                                      2021-03-29 2021-03-30 2021-03-31
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#> StationMica Viane                            NA         NA         NA
#>                                      2021-04-01 2021-04-02 2021-04-03
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#> StationMica Viane                            NA         NA         NA
#>                                      2021-04-04 2021-04-05 2021-04-06
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#> StationMica Viane                            NA         NA         NA
#>                                      2021-04-07 2021-04-08 2021-04-09
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#> StationMica Viane                            NA         NA         NA
#>                                      2021-04-10 2021-04-11 2021-04-12
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#> StationMica Viane                            NA         NA         NA
#>                                      2021-04-13 2021-04-14 2021-04-15
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1          1
#> StationMica Viane                            NA         NA         NA
#>                                      2021-04-16 2021-04-17 2021-04-18
#> StationB_DL_val 5_beek kleine vijver         NA         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA         NA
#> StationB_DM_val 4_'t WAD                      1          1  0.8923611
#> StationMica Viane                            NA         NA         NA
```
