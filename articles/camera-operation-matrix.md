# Camera operation matrix

This functionality has been superseded because camtrapR supports reading
Camera Trap Data Packages, see the function
[`camtrapR::readCamtrapDP()`](https://jniedballa.github.io/camtrapR/reference/readCamtrapDP.html).

This vignette shows how to get a **camera operation matrix** from a
Camera Trap Data Package dataset, equivalent to the matrix returned by
camtrapR’s function
[`camtrapR::cameraOperation()`](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).

## Setup

Load the packages that will be used in this example:

``` r

library(camtraptor)
#> 
#> Attaching package: 'camtraptor'
#> The following object is masked from 'package:base':
#> 
#>     contributors
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

For this example the function
[`example_dataset()`](https://inbo.github.io/camtraptor/reference/example_dataset.md)
is used to load an example Camera Trap Data Package dataset. The dataset
is derived from a study on detecting invasive muskrat and coypu
populations using camera traps.

``` r

x <- example_dataset()
```

## Generating the camera operation matrix

The camera operation matrix can be generated with
[`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md):

``` r

cam_op <- get_cam_op(x)
```

For readability, let’s only print the first 45 columns of the camera
operation matrix (i.e. the first 45 days) instead of all 324:

``` r

cam_op[, 1:45]
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
#>                               2020-07-13
#> B_HS_val 2_processiepark              NA
#> B_DL_val 5_beek kleine vijver         NA
#> B_DL_val 3_dikke boom                 NA
#> B_DM_val 4_'t WAD                     NA
```

To build this matrix, the function reads the `deployments` slot of the
Camera Trap Data Package. Row names are station names, by default taken
from the `locationName` column of `deployments`. Column names are dates.
The matrix values represent the daily effort:

- `NA`: the camera was not set up on that day.
- `1`: the camera was fully active for the entire day.
- A decimal between 0 and 1: the camera was partially active (e.g. on
  the first or last day of a deployment).
- Greater than `1`: multiple deployments at the same location were
  active on that day.

In the example above, no location has overlapping deployments, so no
values greater than 1 occur in the matrix. To demonstrate the scenario
where values are greater than `1`, the example below assigns all four
deployments to the same location (`B_HS_val 2_processiepark`):

``` r

x1 <- x
# Assigning all four deployments to the first location
deployments(x1)$locationName[] <- deployments(x1)$locationName[1]
deployments(x1)$deploymentStart[] <- deployments(x1)$deploymentStart[1]
deployments(x1)$deploymentEnd[] <- deployments(x1)$deploymentEnd[1]
# Visualize camera operation matrix
get_cam_op(x1)
#>                          2020-05-30 2020-05-31 2020-06-01 2020-06-02 2020-06-03
#> B_HS_val 2_processiepark    3.50662          4          4          4          4
#>                          2020-06-04 2020-06-05 2020-06-06 2020-06-07 2020-06-08
#> B_HS_val 2_processiepark          4          4          4          4          4
#>                          2020-06-09 2020-06-10 2020-06-11 2020-06-12 2020-06-13
#> B_HS_val 2_processiepark          4          4          4          4          4
#>                          2020-06-14 2020-06-15 2020-06-16 2020-06-17 2020-06-18
#> B_HS_val 2_processiepark          4          4          4          4          4
#>                          2020-06-19 2020-06-20 2020-06-21 2020-06-22 2020-06-23
#> B_HS_val 2_processiepark          4          4          4          4          4
#>                          2020-06-24 2020-06-25 2020-06-26 2020-06-27 2020-06-28
#> B_HS_val 2_processiepark          4          4          4          4          4
#>                          2020-06-29 2020-06-30 2020-07-01
#> B_HS_val 2_processiepark          4          4   1.615787
```

To demonstrate the scenario where a single location is linked to
multiple (non-overlapping) deployments, the example below assigns
location `B_HS_val 2_processiepark` to two separate deployments:

``` r

x2 <- x
# Assigning the first location to two separate deployments
deployments(x2)$locationName[2] <- deployments(x2)$locationName[1]
deployments(x2)$deploymentStart[2] <- deployments(x2)$deploymentEnd[1] + 
  ddays(5)
deployments(x2)$deploymentEnd[2] <- deployments(x2)$deploymentStart[2] + 
  ddays(5)
# Visualize deployments
deployments(x2) %>% select(locationName, deploymentStart, deploymentEnd)
#> # A tibble: 4 × 3
#>   locationName             deploymentStart     deploymentEnd      
#>   <chr>                    <dttm>              <dttm>             
#> 1 B_HS_val 2_processiepark 2020-05-30 02:57:37 2020-07-01 09:41:41
#> 2 B_HS_val 2_processiepark 2020-07-06 09:41:41 2020-07-11 09:41:41
#> 3 B_DL_val 3_dikke boom    2020-06-19 21:00:00 2020-06-28 23:33:22
#> 4 B_DM_val 4_'t WAD        2021-03-27 20:38:18 2021-04-18 21:25:00

# Visualize camera operation matrix
get_cam_op(x2)[1, 1:45]
#> 2020-05-30 2020-05-31 2020-06-01 2020-06-02 2020-06-03 2020-06-04 2020-06-05 
#>  0.8766551  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000 
#> 2020-06-06 2020-06-07 2020-06-08 2020-06-09 2020-06-10 2020-06-11 2020-06-12 
#>  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000 
#> 2020-06-13 2020-06-14 2020-06-15 2020-06-16 2020-06-17 2020-06-18 2020-06-19 
#>  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000 
#> 2020-06-20 2020-06-21 2020-06-22 2020-06-23 2020-06-24 2020-06-25 2020-06-26 
#>  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000 
#> 2020-06-27 2020-06-28 2020-06-29 2020-06-30 2020-07-01 2020-07-02 2020-07-03 
#>  1.0000000  1.0000000  1.0000000  1.0000000  0.4039468         NA         NA 
#> 2020-07-04 2020-07-05 2020-07-06 2020-07-07 2020-07-08 2020-07-09 2020-07-10 
#>         NA         NA  0.5960532  1.0000000  1.0000000  1.0000000  1.0000000 
#> 2020-07-11 2020-07-12 2020-07-13 
#>  0.4039468         NA         NA
```

### Station names

By default, row names are taken from the `locationName` column of
`deployments`. You can specify a different column of `deployments` using
`station_col`. The example below uses `locationID`:

``` r

cam_op_with_locationID <- get_cam_op(
  x,
  station_col = "locationID"
)
# Since the full matrix would be too wide to display here, we only inspect
# the row names:
row.names(cam_op_with_locationID)
#> [1] "e254a13c" "2df5259b" "ff1535c0" "ce943ced"
```

### Session and camera IDs

Let’s first extend the dataset so `deployments` contains a column for
`cameraID` and `sessionID`.

``` r

x_extended <- x

deployments(x_extended) <- deployments(x) %>% 
  mutate(sessionID = c(1, 2, 3, 4)) %>% 
  mutate(cameraID = c(1, 2, 3, 4))
```

You can specify the column containing the session IDs to be added to the
station names using the `session_col` argument. The sessionID will be
displayed following camtrapR’s convention: `Station__SESS_sessionID`:

``` r

cam_op_with_session_ids <- get_cam_op(
  x_extended,
  session_col = "sessionID"
)
# Since the full matrix would be too wide to display here, we only inspect
# the row names:
row.names(cam_op_with_session_ids)
#> [1] "B_HS_val 2_processiepark__SESS_1"     
#> [2] "B_DL_val 5_beek kleine vijver__SESS_2"
#> [3] "B_DL_val 3_dikke boom__SESS_3"        
#> [4] "B_DM_val 4_'t WAD__SESS_4"
```

You can also specify the column containing the camera IDs to be added to
the station names following camtrapR’s convention:
`Station__CAM_CameraID` using the `camera_col` argument:

``` r

cam_op_with_camera_ids <- get_cam_op(
  x_extended,
  camera_col = "cameraID"
)
# Since the full matrix would be too wide to display here, we only inspect
# the row names:
row.names(cam_op_with_camera_ids)
#> [1] "B_HS_val 2_processiepark__CAM_1"     
#> [2] "B_DL_val 5_beek kleine vijver__CAM_2"
#> [3] "B_DL_val 3_dikke boom__CAM_3"        
#> [4] "B_DM_val 4_'t WAD__CAM_4"
```

To use both camera and session IDs, the camtrapR’s convention
`Station__SESS_SessionID__CAM_CameraID` is followed:

``` r

cam_op_with_session_and_camera_ids <- get_cam_op(
  x_extended,
  camera_col = "cameraID",
  session_col = "sessionID"
)
# Since the full matrix would be too wide to display here, we only inspect
# the row names:
row.names(cam_op_with_session_and_camera_ids)
#> [1] "B_HS_val 2_processiepark__SESS_1__CAM_1"     
#> [2] "B_DL_val 5_beek kleine vijver__SESS_2__CAM_2"
#> [3] "B_DL_val 3_dikke boom__SESS_3__CAM_3"        
#> [4] "B_DM_val 4_'t WAD__SESS_4__CAM_4"
```

You can also use the prefix `"Station"` in the station names as done by
camtrapR’s `cameraOperation()` by setting `use_prefix = TRUE`:

``` r

cam_op_with_session_and_camera_ids_prefix <- get_cam_op(
  x_extended,
  camera_col = "cameraID",
  session_col = "sessionID",
  use_prefix = TRUE
)
# Since the full matrix would be too wide to display here, we only inspect
# the row names:
row.names(cam_op_with_session_and_camera_ids_prefix)
#> [1] "StationB_HS_val 2_processiepark__SESS_1__CAM_1"     
#> [2] "StationB_DL_val 5_beek kleine vijver__SESS_2__CAM_2"
#> [3] "StationB_DL_val 3_dikke boom__SESS_3__CAM_3"        
#> [4] "StationB_DM_val 4_'t WAD__SESS_4__CAM_4"
```
