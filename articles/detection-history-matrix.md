# Detection history matrix

This functionality has been superseded because camtrapR supports reading
Camera Trap Data Packages, see the function
[`camtrapR::readCamtrapDP()`](https://jniedballa.github.io/camtrapR/reference/readCamtrapDP.html).

This vignette shows how to get a **detection history matrix** from a
Camera Trap Data Package dataset, equivalent to the matrix returned by
camtrapR’s function
[`camtrapR::detectionHistory()`](https://jniedballa.github.io/camtrapR/reference/detectionHistory.html).

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
library(stringr)
```

For this example the function
[`example_dataset()`](https://inbo.github.io/camtraptor/reference/example_dataset.md)
is used to load an example Camera Trap Data Package dataset. The dataset
is derived from a study on detecting invasive muskrat and coypu
populations using camera traps.

``` r

x <- example_dataset()
```

The detection history is calculated based on a camera operation matrix
and a record table. Both are derived from the Camera Trap Data Package
dataset:

``` r

cam_op <- get_cam_op(x)
recordTable <- get_record_table(x)
```

See the vignettes
[`vignette("camera-operation-matrix")`](https://inbo.github.io/camtraptor/articles/camera-operation-matrix.md)
and
[`vignette("record-table")`](https://inbo.github.io/camtraptor/articles/record-table.md)
for details on how to build these inputs.

## Detection history

### Output types

[`get_detection_history()`](https://inbo.github.io/camtraptor/reference/get_detection_history.md)
returns a list with three elements: `detection_history`, `effort`, and
`dates`. The `output` argument controls what the detection history
matrix contains. Three options are available:

**Binary** (`output = "binary"`): 1 if the species was detected at a
station during an occasion, 0 if not:

``` r

get_detection_history(
  recordTable,
  cam_op,
  species = "Anas platyrhynchos",
  output = "binary"
)
#> $detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_HS_val 2_processiepark       1  1  0  0  0  0  0  1  0   0   1   0   0   1
#> B_DL_val 5_beek kleine vijver  1  1  1  0  1  1  1  0  0   0   0  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0  0  0   0  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26
#> B_HS_val 2_processiepark        0   0   0   0   0   0   0   0   0   1   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0   0   0  NA  NA  NA
#>                               o27 o28 o29 o30 o31 o32 o33
#> B_HS_val 2_processiepark        0   0   0   0   0   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              NA  NA  NA  NA  NA  NA  NA
#> 
#> $effort
#>                                      o1 o2 o3 o4 o5 o6 o7 o8 o9       o10
#> B_HS_val 2_processiepark      0.8766551  1  1  1  1  1  1  1  1 1.0000000
#> B_DL_val 5_beek kleine vijver 0.7710532  1  1  1  1  1  1  1  1 1.0000000
#> B_DL_val 3_dikke boom         0.1250000  1  1  1  1  1  1  1  1 0.9815046
#> B_DM_val 4_'t WAD             0.1400694  1  1  1  1  1  1  1  1 1.0000000
#>                                     o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21
#> B_HS_val 2_processiepark      1.0000000   1   1   1   1   1   1   1   1   1   1
#> B_DL_val 5_beek kleine vijver 0.1810185  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom                NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD             1.0000000   1   1   1   1   1   1   1   1   1   1
#>                               o22       o23 o24 o25 o26 o27 o28 o29 o30 o31 o32
#> B_HS_val 2_processiepark        1 1.0000000   1   1   1   1   1   1   1   1   1
#> B_DL_val 5_beek kleine vijver  NA        NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA        NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               1 0.8923611  NA  NA  NA  NA  NA  NA  NA  NA  NA
#>                                     o33
#> B_HS_val 2_processiepark      0.4039468
#> B_DL_val 5_beek kleine vijver        NA
#> B_DL_val 3_dikke boom                NA
#> B_DM_val 4_'t WAD                    NA
#> 
#> $dates
#>                               o1           o2           o3          
#> B_HS_val 2_processiepark      "2020-05-30" "2020-05-31" "2020-06-01"
#> B_DL_val 5_beek kleine vijver "2020-07-29" "2020-07-30" "2020-07-31"
#> B_DL_val 3_dikke boom         "2020-06-19" "2020-06-20" "2020-06-21"
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-03-28" "2021-03-29"
#>                               o4           o5           o6          
#> B_HS_val 2_processiepark      "2020-06-02" "2020-06-03" "2020-06-04"
#> B_DL_val 5_beek kleine vijver "2020-08-01" "2020-08-02" "2020-08-03"
#> B_DL_val 3_dikke boom         "2020-06-22" "2020-06-23" "2020-06-24"
#> B_DM_val 4_'t WAD             "2021-03-30" "2021-03-31" "2021-04-01"
#>                               o7           o8           o9          
#> B_HS_val 2_processiepark      "2020-06-05" "2020-06-06" "2020-06-07"
#> B_DL_val 5_beek kleine vijver "2020-08-04" "2020-08-05" "2020-08-06"
#> B_DL_val 3_dikke boom         "2020-06-25" "2020-06-26" "2020-06-27"
#> B_DM_val 4_'t WAD             "2021-04-02" "2021-04-03" "2021-04-04"
#>                               o10          o11          o12         
#> B_HS_val 2_processiepark      "2020-06-08" "2020-06-09" "2020-06-10"
#> B_DL_val 5_beek kleine vijver "2020-08-07" "2020-08-08" NA          
#> B_DL_val 3_dikke boom         "2020-06-28" NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-05" "2021-04-06" "2021-04-07"
#>                               o13          o14          o15         
#> B_HS_val 2_processiepark      "2020-06-11" "2020-06-12" "2020-06-13"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-08" "2021-04-09" "2021-04-10"
#>                               o16          o17          o18         
#> B_HS_val 2_processiepark      "2020-06-14" "2020-06-15" "2020-06-16"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-11" "2021-04-12" "2021-04-13"
#>                               o19          o20          o21         
#> B_HS_val 2_processiepark      "2020-06-17" "2020-06-18" "2020-06-19"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-14" "2021-04-15" "2021-04-16"
#>                               o22          o23          o24         
#> B_HS_val 2_processiepark      "2020-06-20" "2020-06-21" "2020-06-22"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-17" "2021-04-18" NA          
#>                               o25          o26          o27         
#> B_HS_val 2_processiepark      "2020-06-23" "2020-06-24" "2020-06-25"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             NA           NA           NA          
#>                               o28          o29          o30         
#> B_HS_val 2_processiepark      "2020-06-26" "2020-06-27" "2020-06-28"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             NA           NA           NA          
#>                               o31          o32          o33         
#> B_HS_val 2_processiepark      "2020-06-29" "2020-06-30" "2020-07-01"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             NA           NA           NA
```

**Number of observations** (`output = "n_observations"`): the number of
records per station per occasion. With the default
[`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)
settings, duplicate records on the same day at the same station are
removed, so this is equivalent to `"binary"` in most cases:

``` r

det_hist_n_obs <- get_detection_history(
  recordTable,
  cam_op,
  species = "Anas platyrhynchos",
  output = "n_observations"
)
```

For readability, only the `detection_history` element is shown here:

``` r

det_hist_n_obs$detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_HS_val 2_processiepark       1  1  0  0  0  0  0  1  0   0   1   0   0   1
#> B_DL_val 5_beek kleine vijver  1  1  1  0  1  1  1  0  0   0   0  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0  0  0   0  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26
#> B_HS_val 2_processiepark        0   0   0   0   0   0   0   0   0   1   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0   0   0  NA  NA  NA
#>                               o27 o28 o29 o30 o31 o32 o33
#> B_HS_val 2_processiepark        0   0   0   0   0   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              NA  NA  NA  NA  NA  NA  NA
```

To allow counts above 1 for `"n_observations"`, build a record table
without removing duplicates:

``` r

recordTable_multiple <- get_record_table(x, removeDuplicateRecords = FALSE)

det_hist_n_obs_multiple <- get_detection_history(
  recordTable_multiple,
  cam_op,
  species = "Anas platyrhynchos",
  output = "n_observations"
)
det_hist_n_obs_multiple$detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_HS_val 2_processiepark       1  1  0  0  0  0  0  2  0   0   1   0   0   2
#> B_DL_val 5_beek kleine vijver  1  1  1  0  1  1  1  0  0   0   0  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0  0  0   0  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26
#> B_HS_val 2_processiepark        0   0   0   0   0   0   0   0   0   2   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0   0   0  NA  NA  NA
#>                               o27 o28 o29 o30 o31 o32 o33
#> B_HS_val 2_processiepark        0   0   0   0   0   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              NA  NA  NA  NA  NA  NA  NA
```

**Number of individuals** (`output = "n_individuals"`): the number of
individuals detected per station per occasion, summed across records:

``` r

det_hist_n_ind <- get_detection_history(
  recordTable,
  cam_op,
  species = "Anas platyrhynchos",
  output = "n_individuals"
)
det_hist_n_ind$detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_HS_val 2_processiepark       1  2  0  0  0  0  0  1  0   0   4   0   0   1
#> B_DL_val 5_beek kleine vijver  2  2  2  0  5  3  3  0  0   0   0  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0  0  0   0  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26
#> B_HS_val 2_processiepark        0   0   0   0   0   0   0   0   0   3   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0   0   0  NA  NA  NA
#>                               o27 o28 o29 o30 o31 o32 o33
#> B_HS_val 2_processiepark        0   0   0   0   0   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              NA  NA  NA  NA  NA  NA  NA
```

### Occasion length

By default, each column in the detection history represents a single day
(`occasionLength = 1`). Use `occasionLength` to aggregate records into
longer periods. The example below uses weekly occasions:

``` r

det_hist_weekly <- get_detection_history(
  recordTable,
  cam_op,
  species = "Anas platyrhynchos",
  output = "binary",
  occasionLength = 7
)
det_hist_weekly$detection_history
#>                               o1 o2 o3 o4 o5
#> B_HS_val 2_processiepark       1  1  0  1  0
#> B_DL_val 5_beek kleine vijver  1  0 NA NA NA
#> B_DL_val 3_dikke boom          0  0 NA NA NA
#> B_DM_val 4_'t WAD              0  0  0  0 NA
```

### Minimum active days per occasion

Occasions with few active trap days may be unreliable. Use
`minActiveDaysPerOccasion` to set a minimum: occasions with fewer active
days are replaced by `NA`. The argument must be smaller than or equal to
`occasionLength`:

``` r

det_hist_min_active <- get_detection_history(
  recordTable,
  cam_op,
  species = "Anas platyrhynchos",
  output = "binary",
  occasionLength = 7,
  minActiveDaysPerOccasion = 5
)
det_hist_min_active$detection_history
#>                               o1 o2 o3 o4 o5
#> B_HS_val 2_processiepark       1  1  0  1 NA
#> B_DL_val 5_beek kleine vijver  1 NA NA NA NA
#> B_DL_val 3_dikke boom          0 NA NA NA NA
#> B_DM_val 4_'t WAD              0  0  0 NA NA
```

### Maximum number of days

Use `maxNumberDays` to limit the detection history to a fixed number of
trap days per station, counted from the first active day of each
station. Stations with more active days are truncated:

``` r

det_hist_max_days <- get_detection_history(
  recordTable,
  cam_op,
  species = "Anas platyrhynchos",
  output = "binary",
  maxNumberDays = 30
)
det_hist_max_days$detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_HS_val 2_processiepark       1  1  0  0  0  0  0  1  0   0   1   0   0   1
#> B_DL_val 5_beek kleine vijver  1  1  1  0  1  1  1  0  0   0   0  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0  0  0   0  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26
#> B_HS_val 2_processiepark        0   0   0   0   0   0   0   0   0   1   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0   0   0  NA  NA  NA
#>                               o27 o28 o29 o30
#> B_HS_val 2_processiepark        0   0   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA
#> B_DM_val 4_'t WAD              NA  NA  NA  NA
```

### Start date

By default, occasions begin on the first active day of each station
(`day1 = "station"`). You can instead specify a fixed start date shared
across all stations. Records before this date are excluded and a warning
is raised if any are removed:

``` r

det_hist_day1 <- get_detection_history(
  recordTable,
  cam_op,
  species = "Anas platyrhynchos",
  output = "binary",
  day1 = "2020-06-22"
)
#> Warning in get_detection_history(recordTable, cam_op, species = "Anas platyrhynchos", : 5 record(s) (out of 12) are removed because they were taken before `day1` (2020-06-22), e.g.:
#> B_HS_val 2_processiepark: 2020-05-30.
det_hist_day1$detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_HS_val 2_processiepark       1  0  0  0  0  0  0  0  0   0  NA  NA  NA  NA
#> B_DL_val 5_beek kleine vijver  1  1  1  0  1  1  1  0  0   0   0  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0 NA NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23
#> B_HS_val 2_processiepark       NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0   0   0
```

### Buffer

Use `buffer` to exclude a number of days at the start of each station’s
deployment from the detection history. This can be useful to exclude a
settling-in period after camera setup. `buffer` can only be used in
combination with `day1 = "station"`. A warning is raised if any records
fall within the buffer period:

``` r

det_hist_buffer <- get_detection_history(
  recordTable,
  cam_op,
  species = "Anas platyrhynchos",
  output = "binary",
  buffer = 7
)
#> Warning in get_detection_history(recordTable, cam_op, species = "Anas platyrhynchos", : 8 record(s) (out of 12) are removed because they were taken during the buffer period of 7 day(s), e.g.:
#> B_DL_val 5_beek kleine vijver: 2020-07-29.
det_hist_buffer$detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_HS_val 2_processiepark       1  0  0  1  0  0  1  0  0   0   0   0   0   0
#> B_DL_val 5_beek kleine vijver  0  0  0  0 NA NA NA NA NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0 NA NA NA NA NA NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26
#> B_HS_val 2_processiepark        0   0   1   0   0   0   0   0   0   0   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
```

## Multi-season detection history

If the camera operation matrix was built with a session column (via
`session_col` in
[`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md)),
the session structure is detected automatically. Set
`unmarkedMultFrameInput = TRUE` to reshape the output for use as the `y`
argument in
[`unmarked::unmarkedMultFrame()`](https://ecoverseR.github.io/unmarked/reference/unmarkedMultFrame.html).

First, add a session column to the deployments and rebuild the inputs:

``` r

x_sessions <- x
deployments(x_sessions) <- deployments(x_sessions) %>%
  mutate(session = ifelse(
    str_starts(.data$locationName, stringr::fixed("B_DL_")),
    "after2020",
    "before2020"
  ))

cam_op_sessions <- get_cam_op(x_sessions, session_col = "session")
recordTable_sessions <- get_record_table(x_sessions)
```

Then generate the multi-season detection history:

``` r

det_hist_sessions <- get_detection_history(
  recordTable_sessions,
  cam_op_sessions,
  species = "Anas platyrhynchos",
  output = "n_individuals",
  unmarkedMultFrameInput = TRUE
)
```

For readability, only the `detection_history` element and the first 8
columns are shown here:

``` r

det_hist_sessions$detection_history[, 1:8]
#>                               o1__SESS_before2020 o2__SESS_before2020
#> B_HS_val 2_processiepark                        1                   2
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                               0                   0
#>                               o3__SESS_before2020 o4__SESS_before2020
#> B_HS_val 2_processiepark                        0                   0
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                               0                   0
#>                               o5__SESS_before2020 o6__SESS_before2020
#> B_HS_val 2_processiepark                        0                   0
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                               0                   0
#>                               o7__SESS_before2020 o8__SESS_before2020
#> B_HS_val 2_processiepark                        0                   1
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                               0                   0
```

Each row corresponds to a site. Columns are in season-major,
occasion-minor order: `o1__SESS_before2020`, `o2__SESS_before2020`,
`o1__SESS_after2020`, etc. Note that `unmarkedMultFrameInput = TRUE` is
only compatible with `day1 = "station"` (the default).
