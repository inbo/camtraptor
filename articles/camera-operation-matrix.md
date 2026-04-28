# Camera operation matrix

This vignette shows how to get a *camera trap station operability
matrix*, shortly called *camera operation matrix* as returned by
camtrapR’s function
[cameraOperation](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html).

## Setup

Load packages:

``` r
library(camtraptor)
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

By loading package `camtraptor`, a camera trap data package called
`mica` is made available. This data package contains camera trap data of
musk rats and coypus. We will use this variable from now on.

## Camera operation matrix

You can create a camera operation matrix by passing a camera trap data
package:

``` r
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
```

The function will read the `deployments` slot of the data package to
create the matrix.

The matrix rows are the location names, by default the values of field
`locationName` of `deployments`. Column names are dates. The matrix
values are: 0 if station is not active, an integer (1 or more) if one or
more deployments linked to the same station are fully active during the
whole day and a decimal value for partially active stations.

In the example above there was a one-to-one relation between deployments
and locations: the daily effort is between 0 and 1.

In the example below we show what happens if all four deployments are
set to the same location (`B_DL_val 5_beek kleine vijver`) and they are
active during the same days:

``` r
mica1 <- mica
mica1$data$deployments$locationName <- mica1$data$deployments$locationName[1]
mica1$data$deployments$start <- mica1$data$deployments$start[1]
mica1$data$deployments$end <- mica1$data$deployments$end[1]
multiple_deploys_matrix <- get_cam_op(mica1)
multiple_deploys_matrix
#>                               2020-07-29 2020-07-30 2020-07-31 2020-08-01
#> B_DL_val 5_beek kleine vijver   3.084213          4          4          4
#>                               2020-08-02 2020-08-03 2020-08-04 2020-08-05
#> B_DL_val 5_beek kleine vijver          4          4          4          4
#>                               2020-08-06 2020-08-07 2020-08-08
#> B_DL_val 5_beek kleine vijver          4          4  0.7240741
```

In the example below we simulate a location (`B_DM_val 4_'t WAD`) linked
to two deployments active in two different periods:

``` r
mica2 <- mica
mica2$data$deployments$locationName[4] <- mica2$data$deployments$locationName[3]
mica2$data$deployments$start[4] <- mica2$data$deployments$end[3] + ddays(5)
mica2$data$deployments$end[4] <- mica2$data$deployments$start[4] + ddays(5)
mica2$data$deployments %>% dplyr::select(locationName, start, end)
#> # A tibble: 4 × 3
#>   locationName                  start               end                
#>   <chr>                         <dttm>              <dttm>             
#> 1 B_DL_val 5_beek kleine vijver 2020-07-29 05:29:41 2020-08-08 04:20:40
#> 2 B_DL_val 3_dikke boom         2020-06-19 21:00:00 2020-06-28 23:33:22
#> 3 B_DM_val 4_'t WAD             2021-03-27 20:38:18 2021-04-18 21:25:00
#> 4 B_DM_val 4_'t WAD             2021-04-23 21:25:00 2021-04-28 21:25:00
```

This results in the following camera operation matrix:

``` r
two_deploys_two_periods_matrix <- get_cam_op(mica2)
days_to_show <- seq(as.Date(mica2$data$deployments$start[3]),
                    as.Date(mica2$data$deployments$end[4]),
                    by = "days")
two_deploys_two_periods_matrix[3,as.character(days_to_show)]
#> 2021-03-27 2021-03-28 2021-03-29 2021-03-30 2021-03-31 2021-04-01 2021-04-02 
#>  0.1400694  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000 
#> 2021-04-03 2021-04-04 2021-04-05 2021-04-06 2021-04-07 2021-04-08 2021-04-09 
#>  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000 
#> 2021-04-10 2021-04-11 2021-04-12 2021-04-13 2021-04-14 2021-04-15 2021-04-16 
#>  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000  1.0000000 
#> 2021-04-17 2021-04-18 2021-04-19 2021-04-20 2021-04-21 2021-04-22 2021-04-23 
#>  1.0000000  0.8923611         NA         NA         NA         NA  0.1076389 
#> 2021-04-24 2021-04-25 2021-04-26 2021-04-27 2021-04-28 
#>  1.0000000  1.0000000  1.0000000  1.0000000  0.8923611
```

### Station names

You can specify the column containing the station names instead of using
the default `locationName`. In the example below we use the column
`locationID`. A small preview of the matrix is shown:

``` r
cam_op_with_locationID <- get_cam_op(mica, station_col = "locationID")
cam_op_with_locationID[1:4, 1:2]
#>                                      2019-10-09 2019-10-10
#> 2df5259b-b4b4-4f43-8cf7-effcced06d6f         NA         NA
#> ff1535c0-6b5d-44be-b3ef-c4d4204dad74         NA         NA
#> ce943ced-1bcf-4140-9a2e-e8ee5e8c10e6         NA         NA
#> 3232bcfd-5dfa-496e-b7ab-14593bb1b7f1  0.5290856          1
```

You can also decide to use prefix `"Station"` in the station names as
done by camtrapR’s `cameraOperation()` by setting `use_prefix = TRUE`. A
small preview of the entire matrix is shown:

``` r
cam_op_with_prefix <- get_cam_op(mica, use_prefix = TRUE)
cam_op_with_prefix[1:4,1:2]
#>                                      2019-10-09 2019-10-10
#> StationB_DL_val 5_beek kleine vijver         NA         NA
#> StationB_DL_val 3_dikke boom                 NA         NA
#> StationB_DM_val 4_'t WAD                     NA         NA
#> StationMica Viane                     0.5290856          1
```

### Session and camera IDs

You can specify the column containing the camera IDs to be added to the
station names following the camtrapR’s convention:
`Station__CAM_CameraID`. Only the row names are shown:

``` r
mica_cameras <- mica
mica_cameras$data$deployments$cameraID <- c(1, 2, 3, 4)
cam_op_with_camera_ids <- get_cam_op(mica_cameras, camera_col = "cameraID")
row.names(cam_op_with_camera_ids)
#> [1] "B_DL_val 5_beek kleine vijver__CAM_1"
#> [2] "B_DL_val 3_dikke boom__CAM_2"        
#> [3] "B_DM_val 4_'t WAD__CAM_3"            
#> [4] "Mica Viane__CAM_4"
```

You cans also add the session IDs using `session_col` argument,
following the camtrapR’s convention: `Station__SESS_sessionID`:

``` r
mica_sessions <- mica
mica_sessions$data$deployments$session <- c(1, 2, 3, 4)
cam_op_with_session_ids <- get_cam_op(mica_sessions, session_col = "session")
row.names(cam_op_with_session_ids)
#> [1] "B_DL_val 5_beek kleine vijver__SESS_1"
#> [2] "B_DL_val 3_dikke boom__SESS_2"        
#> [3] "B_DM_val 4_'t WAD__SESS_3"            
#> [4] "Mica Viane__SESS_4"
```

To use both camera and session IDs, the camtrapR’s convention
`Station__SESS_SessionID__CAM_CameraID` is followed:

``` r
mica_sessions$data$deployments$cameraID <- c(1, 2, 3, 4)
cam_op_with_session_and_camera_ids <- get_cam_op(
  mica_sessions, 
  camera_col = "cameraID",
  session_col = "session"
)
row.names(cam_op_with_session_and_camera_ids)
#> [1] "B_DL_val 5_beek kleine vijver__SESS_1__CAM_1"
#> [2] "B_DL_val 3_dikke boom__SESS_2__CAM_2"        
#> [3] "B_DM_val 4_'t WAD__SESS_3__CAM_3"            
#> [4] "Mica Viane__SESS_4__CAM_4"
```
