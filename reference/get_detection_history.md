# Get the detection history of a species

**\[superseded\]**

This function is superseded because camtrapR now supports reading Camera
Trap Data Packages. Use
[`camtrapR::readCamtrapDP()`](https://jniedballa.github.io/camtrapR/reference/readCamtrapDP.html)
and
[`camtrapR::detectionHistory()`](https://jniedballa.github.io/camtrapR/reference/detectionHistory.html)
instead.

Creates the detection history matrix of a species based on the record
table and the camera operation matrix. The detection history is a
concept developed within the camtrapR package, see the function
documentation for
[`camtrapR::detectionHistory()`](https://jniedballa.github.io/camtrapR/reference/detectionHistory.html).

The detection history matrix is a binary matrix where rows represent
camera stations and columns represent occasions. The matrix is filled
with 1s and 0s, where 1 indicates that the species was detected at a
station on a given occasion and 0 indicates that the species was not
detected. The function also returns the effort matrix, which contains
the number of days that each station was active on each occasion, and
the dates matrix, which contains the dates of the occasions.

## Usage

``` r
get_detection_history(
  recordTable,
  camOp,
  species,
  output,
  occasionLength = 1,
  minActiveDaysPerOccasion = NULL,
  maxNumberDays = NULL,
  day1 = "station",
  buffer = NULL,
  unmarkedMultFrameInput = FALSE
)
```

## Arguments

- recordTable:

  A data frame with the camera trap records. The data frame should
  contain the columns 'Station', 'Date', 'Species' and 'n'. 'Station' is
  the camera station ID, 'Date' is the date of the record, 'Species' is
  the species name, 'n' is the number of observations, and 'n_ind' is
  the number of individuals detected.

- camOp:

  A matrix with camera operation data. Rows represent camera stations
  and columns represent occasions. The matrix should contain the number
  of days that each station was active on each occasion.

- species:

  Character. The species name.

- output:

  Character. The type of output. Choose one of: `"binary"`,
  `"n_observations"`, `"n_individuals"`.

- occasionLength:

  Integer. The length of the occasions in days. No decimals allowed.
  Default: `1`.

- minActiveDaysPerOccasion:

  Integer. Minimum number of active trap days for occasions to be
  included. Default: `NULL`. If used, it must be smaller than or equal
  to `occasionLength`.

- maxNumberDays:

  Integer. Maximum number of trap days per station. Default: `NULL`. If
  used, it must be greater than or equal to `occasionLength`.

- day1:

  Character. Day occasions should begin: station setup date
  (`"station"`) or a specific date (e.g. `"2015-12-31"`). For
  multi-season detection history (`unmarkedMultFrameInput` = `TRUE`),
  only `day1` = `"station"` is allowed. Default: "station".

- buffer:

  Integer. It makes the first occasion begin a number of days after
  station setup. `buffer` can be used only in combination with `day1` =
  `"station"`. Default: `NULL`. A warning is returned if some records
  are removed because taken during the buffer period.

- unmarkedMultFrameInput:

  Logical. If `TRUE`, the function will return the input for
  multi-season occupancy models in unmarked R package (argument `y` in
  [`unmarked::unmarkedMultFrame()`](https://ecoverseR.github.io/unmarked/reference/unmarkedMultFrame.html)).
  Default: `FALSE`.

## Value

A list with three elements:

- `detection_history`: the detection history matrix

- `effort`: the effort matrix

- `dates`: the dates matrix

## Details

This function doesn't take as input a Camera Trap Data Package object,
but a camera operation matrix and a record table, which are both
calculated based on a Camera Trap Data Package object. For more
information, see the
[`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md)
and
[`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)
functions.

If the camera operation matrix (`camOp`) was created for a multi-season
study (via argument `session_col` in
[`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md)),
the session will be detected automatically. You can then set
`unmarkedMultFrameInput` = `TRUE` to generate a multi-season detection
history. Each row corresponds to a site, and the columns are in
season-major, occasion-minor order, e.g. `o1_SESS_A`, `o2_SESS_A`,
`o1_SESS_B`, `o2_SESS_B`, etc.

## See also

Other deprecated camtrapR-derived functions:
[`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md),
[`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)

## Examples

``` r
library(dplyr)
library(stringr)

x <- example_dataset()
camOp <- get_cam_op(x)
recordTable <- get_record_table(x)
# More observations of the same species on the same day at the same station
# are left.
recordTable_mulitple <- get_record_table(
  x,
  removeDuplicateRecords = FALSE
)

# Binary output
get_detection_history(
  recordTable,
  camOp,
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
#> 

# Number of observations output: same as binary with default
# `get_record_table(x)`
get_detection_history(
 recordTable,
 camOp,
 species = "Anas platyrhynchos",
 output = "n_observations"
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
#> 

# Number of observations output: more than 1 if more than 1 observation of
# the species on the same day at the same station
get_detection_history(
 recordTable_mulitple,
 camOp,
 species = "Anas platyrhynchos",
 output = "n_observations"
)
#> $detection_history
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
#> 

# Number of individuals output
get_detection_history(
 recordTable,
 camOp,
 species = "Anas platyrhynchos",
 output = "n_individuals"
)
#> $detection_history
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
#> 

# Occasion length of 7 days
get_detection_history(
 recordTable,
 camOp,
 species = "Anas platyrhynchos",
 output = "n_individuals",
 occasionLength = 7
)
#> $detection_history
#>                               o1 o2 o3 o4 o5
#> B_HS_val 2_processiepark       3  6  0  3  0
#> B_DL_val 5_beek kleine vijver 17  0 NA NA NA
#> B_DL_val 3_dikke boom          0  0 NA NA NA
#> B_DM_val 4_'t WAD              0  0  0  0 NA
#> 
#> $effort
#>                                     o1       o2 o3       o4       o5
#> B_HS_val 2_processiepark      6.876655 7.000000  7 7.000000 4.403947
#> B_DL_val 5_beek kleine vijver 6.771053 3.181019 NA       NA       NA
#> B_DL_val 3_dikke boom         6.125000 2.981505 NA       NA       NA
#> B_DM_val 4_'t WAD             6.140069 7.000000  7 1.892361       NA
#> 
#> $dates
#>                               o1           o2           o3          
#> B_HS_val 2_processiepark      "2020-05-30" "2020-06-06" "2020-06-13"
#> B_DL_val 5_beek kleine vijver "2020-07-29" "2020-08-05" NA          
#> B_DL_val 3_dikke boom         "2020-06-19" "2020-06-26" NA          
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-04-03" "2021-04-10"
#>                               o4           o5          
#> B_HS_val 2_processiepark      "2020-06-20" "2020-06-27"
#> B_DL_val 5_beek kleine vijver NA           NA          
#> B_DL_val 3_dikke boom         NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-17" NA          
#> 

# Use a `minActiveDaysPerOccasion` of 5 days
get_detection_history(
 recordTable,
 camOp,
 species = "Anas platyrhynchos",
 output = "n_individuals",
 occasionLength = 7,
 minActiveDaysPerOccasion = 5
)
#> $detection_history
#>                               o1 o2 o3 o4 o5
#> B_HS_val 2_processiepark       3  6  0  3 NA
#> B_DL_val 5_beek kleine vijver 17 NA NA NA NA
#> B_DL_val 3_dikke boom          0 NA NA NA NA
#> B_DM_val 4_'t WAD              0  0  0 NA NA
#> 
#> $effort
#>                                     o1 o2 o3 o4 o5
#> B_HS_val 2_processiepark      6.876655  7  7  7 NA
#> B_DL_val 5_beek kleine vijver 6.771053 NA NA NA NA
#> B_DL_val 3_dikke boom         6.125000 NA NA NA NA
#> B_DM_val 4_'t WAD             6.140069  7  7 NA NA
#> 
#> $dates
#>                               o1           o2           o3          
#> B_HS_val 2_processiepark      "2020-05-30" "2020-06-06" "2020-06-13"
#> B_DL_val 5_beek kleine vijver "2020-07-29" NA           NA          
#> B_DL_val 3_dikke boom         "2020-06-19" NA           NA          
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-04-03" "2021-04-10"
#>                               o4           o5
#> B_HS_val 2_processiepark      "2020-06-20" NA
#> B_DL_val 5_beek kleine vijver NA           NA
#> B_DL_val 3_dikke boom         NA           NA
#> B_DM_val 4_'t WAD             NA           NA
#> 

# Use a `maxNumberDays` of 5 days
get_detection_history(
 recordTable,
 camOp,
 species = "Anas platyrhynchos",
 output = "n_individuals",
 maxNumberDays = 5
)
#> Warning: 6 record(s) (out of 12) are removed because they were taken after `maxNumberDays` (5 days) the first day of each station, e.g.:
#> B_DL_val 5_beek kleine vijver: 2020-08-03.
#> $detection_history
#>                               o1 o2 o3 o4 o5
#> B_HS_val 2_processiepark       1  2  0  0  0
#> B_DL_val 5_beek kleine vijver  2  2  2  0  5
#> B_DL_val 3_dikke boom          0  0  0  0  0
#> B_DM_val 4_'t WAD              0  0  0  0  0
#> 
#> $effort
#>                                      o1 o2 o3 o4 o5
#> B_HS_val 2_processiepark      0.8766551  1  1  1  1
#> B_DL_val 5_beek kleine vijver 0.7710532  1  1  1  1
#> B_DL_val 3_dikke boom         0.1250000  1  1  1  1
#> B_DM_val 4_'t WAD             0.1400694  1  1  1  1
#> 
#> $dates
#>                               o1           o2           o3          
#> B_HS_val 2_processiepark      "2020-05-30" "2020-05-31" "2020-06-01"
#> B_DL_val 5_beek kleine vijver "2020-07-29" "2020-07-30" "2020-07-31"
#> B_DL_val 3_dikke boom         "2020-06-19" "2020-06-20" "2020-06-21"
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-03-28" "2021-03-29"
#>                               o4           o5          
#> B_HS_val 2_processiepark      "2020-06-02" "2020-06-03"
#> B_DL_val 5_beek kleine vijver "2020-08-01" "2020-08-02"
#> B_DL_val 3_dikke boom         "2020-06-22" "2020-06-23"
#> B_DM_val 4_'t WAD             "2021-03-30" "2021-03-31"
#> 

# Specify start date via `day1`
get_detection_history(
  recordTable,
  camOp,
  species = "Anas platyrhynchos",
  output = "binary",
  day1 = "2020-06-22"
)
#> Warning: 5 record(s) (out of 12) are removed because they were taken before `day1` (2020-06-22), e.g.:
#> B_HS_val 2_processiepark: 2020-05-30.
#> $detection_history
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
#> 
#> $effort
#>                                      o1 o2 o3 o4 o5 o6        o7 o8 o9
#> B_HS_val 2_processiepark      1.0000000  1  1  1  1  1 1.0000000  1  1
#> B_DL_val 5_beek kleine vijver 0.7710532  1  1  1  1  1 1.0000000  1  1
#> B_DL_val 3_dikke boom         1.0000000  1  1  1  1  1 0.9815046 NA NA
#> B_DM_val 4_'t WAD             0.1400694  1  1  1  1  1 1.0000000  1  1
#>                                     o10       o11 o12 o13 o14 o15 o16 o17 o18
#> B_HS_val 2_processiepark      0.4039468        NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 5_beek kleine vijver 1.0000000 0.1810185  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom                NA        NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD             1.0000000 1.0000000   1   1   1   1   1   1   1
#>                               o19 o20 o21 o22       o23
#> B_HS_val 2_processiepark       NA  NA  NA  NA        NA
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA        NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA        NA
#> B_DM_val 4_'t WAD               1   1   1   1 0.8923611
#> 
#> $dates
#>                               o1           o2           o3          
#> B_HS_val 2_processiepark      "2020-06-22" "2020-06-23" "2020-06-24"
#> B_DL_val 5_beek kleine vijver "2020-07-29" "2020-07-30" "2020-07-31"
#> B_DL_val 3_dikke boom         "2020-06-22" "2020-06-23" "2020-06-24"
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-03-28" "2021-03-29"
#>                               o4           o5           o6          
#> B_HS_val 2_processiepark      "2020-06-25" "2020-06-26" "2020-06-27"
#> B_DL_val 5_beek kleine vijver "2020-08-01" "2020-08-02" "2020-08-03"
#> B_DL_val 3_dikke boom         "2020-06-25" "2020-06-26" "2020-06-27"
#> B_DM_val 4_'t WAD             "2021-03-30" "2021-03-31" "2021-04-01"
#>                               o7           o8           o9          
#> B_HS_val 2_processiepark      "2020-06-28" "2020-06-29" "2020-06-30"
#> B_DL_val 5_beek kleine vijver "2020-08-04" "2020-08-05" "2020-08-06"
#> B_DL_val 3_dikke boom         "2020-06-28" NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-02" "2021-04-03" "2021-04-04"
#>                               o10          o11          o12         
#> B_HS_val 2_processiepark      "2020-07-01" NA           NA          
#> B_DL_val 5_beek kleine vijver "2020-08-07" "2020-08-08" NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-05" "2021-04-06" "2021-04-07"
#>                               o13          o14          o15         
#> B_HS_val 2_processiepark      NA           NA           NA          
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-08" "2021-04-09" "2021-04-10"
#>                               o16          o17          o18         
#> B_HS_val 2_processiepark      NA           NA           NA          
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-11" "2021-04-12" "2021-04-13"
#>                               o19          o20          o21         
#> B_HS_val 2_processiepark      NA           NA           NA          
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-14" "2021-04-15" "2021-04-16"
#>                               o22          o23         
#> B_HS_val 2_processiepark      NA           NA          
#> B_DL_val 5_beek kleine vijver NA           NA          
#> B_DL_val 3_dikke boom         NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-17" "2021-04-18"
#> 

# Use a `buffer` of 2 days
get_detection_history(
 recordTable,
 camOp,
 species = "Anas platyrhynchos",
 output = "n_individuals",
 buffer = 2
)
#> Warning: 4 record(s) (out of 12) are removed because they were taken during the buffer period of 2 day(s), e.g.:
#> B_DL_val 5_beek kleine vijver: 2020-07-29.
#> $detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_HS_val 2_processiepark       0  0  0  0  0  1  0  0  4   0   0   1   0   0
#> B_DL_val 5_beek kleine vijver  2  0  5  3  3  0  0  0  0  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0  0 NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23 o24 o25 o26
#> B_HS_val 2_processiepark        0   0   0   0   0   0   0   3   0   0   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0  NA  NA  NA  NA  NA
#>                               o27 o28 o29 o30 o31
#> B_HS_val 2_processiepark        0   0   0   0   0
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              NA  NA  NA  NA  NA
#> 
#> $effort
#>                               o1 o2 o3 o4 o5 o6 o7        o8        o9 o10 o11
#> B_HS_val 2_processiepark       1  1  1  1  1  1  1 1.0000000 1.0000000   1   1
#> B_DL_val 5_beek kleine vijver  1  1  1  1  1  1  1 1.0000000 0.1810185  NA  NA
#> B_DL_val 3_dikke boom          1  1  1  1  1  1  1 0.9815046        NA  NA  NA
#> B_DM_val 4_'t WAD              1  1  1  1  1  1  1 1.0000000 1.0000000   1   1
#>                               o12 o13 o14 o15 o16 o17 o18 o19 o20       o21 o22
#> B_HS_val 2_processiepark        1   1   1   1   1   1   1   1   1 1.0000000   1
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA        NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA        NA  NA
#> B_DM_val 4_'t WAD               1   1   1   1   1   1   1   1   1 0.8923611  NA
#>                               o23 o24 o25 o26 o27 o28 o29 o30       o31
#> B_HS_val 2_processiepark        1   1   1   1   1   1   1   1 0.4039468
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA        NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA        NA
#> B_DM_val 4_'t WAD              NA  NA  NA  NA  NA  NA  NA  NA        NA
#> 
#> $dates
#>                               o1           o2           o3          
#> B_HS_val 2_processiepark      "2020-06-01" "2020-06-02" "2020-06-03"
#> B_DL_val 5_beek kleine vijver "2020-07-31" "2020-08-01" "2020-08-02"
#> B_DL_val 3_dikke boom         "2020-06-21" "2020-06-22" "2020-06-23"
#> B_DM_val 4_'t WAD             "2021-03-29" "2021-03-30" "2021-03-31"
#>                               o4           o5           o6          
#> B_HS_val 2_processiepark      "2020-06-04" "2020-06-05" "2020-06-06"
#> B_DL_val 5_beek kleine vijver "2020-08-03" "2020-08-04" "2020-08-05"
#> B_DL_val 3_dikke boom         "2020-06-24" "2020-06-25" "2020-06-26"
#> B_DM_val 4_'t WAD             "2021-04-01" "2021-04-02" "2021-04-03"
#>                               o7           o8           o9          
#> B_HS_val 2_processiepark      "2020-06-07" "2020-06-08" "2020-06-09"
#> B_DL_val 5_beek kleine vijver "2020-08-06" "2020-08-07" "2020-08-08"
#> B_DL_val 3_dikke boom         "2020-06-27" "2020-06-28" NA          
#> B_DM_val 4_'t WAD             "2021-04-04" "2021-04-05" "2021-04-06"
#>                               o10          o11          o12         
#> B_HS_val 2_processiepark      "2020-06-10" "2020-06-11" "2020-06-12"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-07" "2021-04-08" "2021-04-09"
#>                               o13          o14          o15         
#> B_HS_val 2_processiepark      "2020-06-13" "2020-06-14" "2020-06-15"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-10" "2021-04-11" "2021-04-12"
#>                               o16          o17          o18         
#> B_HS_val 2_processiepark      "2020-06-16" "2020-06-17" "2020-06-18"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-13" "2021-04-14" "2021-04-15"
#>                               o19          o20          o21         
#> B_HS_val 2_processiepark      "2020-06-19" "2020-06-20" "2020-06-21"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-16" "2021-04-17" "2021-04-18"
#>                               o22          o23          o24         
#> B_HS_val 2_processiepark      "2020-06-22" "2020-06-23" "2020-06-24"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             NA           NA           NA          
#>                               o25          o26          o27         
#> B_HS_val 2_processiepark      "2020-06-25" "2020-06-26" "2020-06-27"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             NA           NA           NA          
#>                               o28          o29          o30         
#> B_HS_val 2_processiepark      "2020-06-28" "2020-06-29" "2020-06-30"
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             NA           NA           NA          
#>                               o31         
#> B_HS_val 2_processiepark      "2020-07-01"
#> B_DL_val 5_beek kleine vijver NA          
#> B_DL_val 3_dikke boom         NA          
#> B_DM_val 4_'t WAD             NA          
#> 

# Multi-season detection history
x_sessions <- x
deployments(x_sessions) <- deployments(x_sessions) %>%
  mutate(session = ifelse(
    str_starts(.data$locationName, "B_DL_"),
      "after2020",
      "before2020"
  )
)
camOp_sessions <- get_cam_op(
  x_sessions,
  session_col = "session"
)
recordTable_sessions <- get_record_table(x_sessions)

# Create a multi-season detection history
get_detection_history(
  recordTable_sessions,
  camOp_sessions,
  species = "Anas platyrhynchos",
  output = "n_individuals",
  unmarkedMultFrameInput = TRUE
)
#> $detection_history
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
#>                               o9__SESS_before2020 o10__SESS_before2020
#> B_HS_val 2_processiepark                        0                    0
#> B_DL_val 5_beek kleine vijver                  NA                   NA
#> B_DL_val 3_dikke boom                          NA                   NA
#> B_DM_val 4_'t WAD                               0                    0
#>                               o11__SESS_before2020 o12__SESS_before2020
#> B_HS_val 2_processiepark                         4                    0
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                0                    0
#>                               o13__SESS_before2020 o14__SESS_before2020
#> B_HS_val 2_processiepark                         0                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                0                    0
#>                               o15__SESS_before2020 o16__SESS_before2020
#> B_HS_val 2_processiepark                         0                    0
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                0                    0
#>                               o17__SESS_before2020 o18__SESS_before2020
#> B_HS_val 2_processiepark                         0                    0
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                0                    0
#>                               o19__SESS_before2020 o20__SESS_before2020
#> B_HS_val 2_processiepark                         0                    0
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                0                    0
#>                               o21__SESS_before2020 o22__SESS_before2020
#> B_HS_val 2_processiepark                         0                    0
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                0                    0
#>                               o23__SESS_before2020 o24__SESS_before2020
#> B_HS_val 2_processiepark                         0                    3
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                0                   NA
#>                               o25__SESS_before2020 o26__SESS_before2020
#> B_HS_val 2_processiepark                         0                    0
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                               NA                   NA
#>                               o27__SESS_before2020 o28__SESS_before2020
#> B_HS_val 2_processiepark                         0                    0
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                               NA                   NA
#>                               o29__SESS_before2020 o30__SESS_before2020
#> B_HS_val 2_processiepark                         0                    0
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                               NA                   NA
#>                               o31__SESS_before2020 o32__SESS_before2020
#> B_HS_val 2_processiepark                         0                    0
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                               NA                   NA
#>                               o33__SESS_before2020 o1__SESS_after2020
#> B_HS_val 2_processiepark                         0                 NA
#> B_DL_val 5_beek kleine vijver                   NA                  2
#> B_DL_val 3_dikke boom                           NA                  0
#> B_DM_val 4_'t WAD                               NA                 NA
#>                               o2__SESS_after2020 o3__SESS_after2020
#> B_HS_val 2_processiepark                      NA                 NA
#> B_DL_val 5_beek kleine vijver                  2                  2
#> B_DL_val 3_dikke boom                          0                  0
#> B_DM_val 4_'t WAD                             NA                 NA
#>                               o4__SESS_after2020 o5__SESS_after2020
#> B_HS_val 2_processiepark                      NA                 NA
#> B_DL_val 5_beek kleine vijver                  0                  5
#> B_DL_val 3_dikke boom                          0                  0
#> B_DM_val 4_'t WAD                             NA                 NA
#>                               o6__SESS_after2020 o7__SESS_after2020
#> B_HS_val 2_processiepark                      NA                 NA
#> B_DL_val 5_beek kleine vijver                  3                  3
#> B_DL_val 3_dikke boom                          0                  0
#> B_DM_val 4_'t WAD                             NA                 NA
#>                               o8__SESS_after2020 o9__SESS_after2020
#> B_HS_val 2_processiepark                      NA                 NA
#> B_DL_val 5_beek kleine vijver                  0                  0
#> B_DL_val 3_dikke boom                          0                  0
#> B_DM_val 4_'t WAD                             NA                 NA
#>                               o10__SESS_after2020 o11__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                   0                   0
#> B_DL_val 3_dikke boom                           0                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o12__SESS_after2020 o13__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o14__SESS_after2020 o15__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o16__SESS_after2020 o17__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o18__SESS_after2020 o19__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o20__SESS_after2020 o21__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o22__SESS_after2020 o23__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o24__SESS_after2020 o25__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o26__SESS_after2020 o27__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o28__SESS_after2020 o29__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o30__SESS_after2020 o31__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o32__SESS_after2020 o33__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#> 
#> $effort
#>                               o1__SESS_before2020 o2__SESS_before2020
#> B_HS_val 2_processiepark                0.8766551                   1
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                       0.1400694                   1
#>                               o3__SESS_before2020 o4__SESS_before2020
#> B_HS_val 2_processiepark                        1                   1
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                               1                   1
#>                               o5__SESS_before2020 o6__SESS_before2020
#> B_HS_val 2_processiepark                        1                   1
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                               1                   1
#>                               o7__SESS_before2020 o8__SESS_before2020
#> B_HS_val 2_processiepark                        1                   1
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                               1                   1
#>                               o9__SESS_before2020 o10__SESS_before2020
#> B_HS_val 2_processiepark                        1                    1
#> B_DL_val 5_beek kleine vijver                  NA                   NA
#> B_DL_val 3_dikke boom                          NA                   NA
#> B_DM_val 4_'t WAD                               1                    1
#>                               o11__SESS_before2020 o12__SESS_before2020
#> B_HS_val 2_processiepark                         1                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                1                    1
#>                               o13__SESS_before2020 o14__SESS_before2020
#> B_HS_val 2_processiepark                         1                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                1                    1
#>                               o15__SESS_before2020 o16__SESS_before2020
#> B_HS_val 2_processiepark                         1                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                1                    1
#>                               o17__SESS_before2020 o18__SESS_before2020
#> B_HS_val 2_processiepark                         1                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                1                    1
#>                               o19__SESS_before2020 o20__SESS_before2020
#> B_HS_val 2_processiepark                         1                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                1                    1
#>                               o21__SESS_before2020 o22__SESS_before2020
#> B_HS_val 2_processiepark                         1                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                                1                    1
#>                               o23__SESS_before2020 o24__SESS_before2020
#> B_HS_val 2_processiepark                 1.0000000                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                        0.8923611                   NA
#>                               o25__SESS_before2020 o26__SESS_before2020
#> B_HS_val 2_processiepark                         1                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                               NA                   NA
#>                               o27__SESS_before2020 o28__SESS_before2020
#> B_HS_val 2_processiepark                         1                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                               NA                   NA
#>                               o29__SESS_before2020 o30__SESS_before2020
#> B_HS_val 2_processiepark                         1                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                               NA                   NA
#>                               o31__SESS_before2020 o32__SESS_before2020
#> B_HS_val 2_processiepark                         1                    1
#> B_DL_val 5_beek kleine vijver                   NA                   NA
#> B_DL_val 3_dikke boom                           NA                   NA
#> B_DM_val 4_'t WAD                               NA                   NA
#>                               o33__SESS_before2020 o1__SESS_after2020
#> B_HS_val 2_processiepark                 0.4039468                 NA
#> B_DL_val 5_beek kleine vijver                   NA          0.7710532
#> B_DL_val 3_dikke boom                           NA          0.1250000
#> B_DM_val 4_'t WAD                               NA                 NA
#>                               o2__SESS_after2020 o3__SESS_after2020
#> B_HS_val 2_processiepark                      NA                 NA
#> B_DL_val 5_beek kleine vijver                  1                  1
#> B_DL_val 3_dikke boom                          1                  1
#> B_DM_val 4_'t WAD                             NA                 NA
#>                               o4__SESS_after2020 o5__SESS_after2020
#> B_HS_val 2_processiepark                      NA                 NA
#> B_DL_val 5_beek kleine vijver                  1                  1
#> B_DL_val 3_dikke boom                          1                  1
#> B_DM_val 4_'t WAD                             NA                 NA
#>                               o6__SESS_after2020 o7__SESS_after2020
#> B_HS_val 2_processiepark                      NA                 NA
#> B_DL_val 5_beek kleine vijver                  1                  1
#> B_DL_val 3_dikke boom                          1                  1
#> B_DM_val 4_'t WAD                             NA                 NA
#>                               o8__SESS_after2020 o9__SESS_after2020
#> B_HS_val 2_processiepark                      NA                 NA
#> B_DL_val 5_beek kleine vijver                  1                  1
#> B_DL_val 3_dikke boom                          1                  1
#> B_DM_val 4_'t WAD                             NA                 NA
#>                               o10__SESS_after2020 o11__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver           1.0000000           0.1810185
#> B_DL_val 3_dikke boom                   0.9815046                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o12__SESS_after2020 o13__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o14__SESS_after2020 o15__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o16__SESS_after2020 o17__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o18__SESS_after2020 o19__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o20__SESS_after2020 o21__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o22__SESS_after2020 o23__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o24__SESS_after2020 o25__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o26__SESS_after2020 o27__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o28__SESS_after2020 o29__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o30__SESS_after2020 o31__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#>                               o32__SESS_after2020 o33__SESS_after2020
#> B_HS_val 2_processiepark                       NA                  NA
#> B_DL_val 5_beek kleine vijver                  NA                  NA
#> B_DL_val 3_dikke boom                          NA                  NA
#> B_DM_val 4_'t WAD                              NA                  NA
#> 
#> $dates
#>                               o1__SESS_before2020 o2__SESS_before2020
#> B_HS_val 2_processiepark      "2020-05-30"        "2020-05-31"       
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             "2021-03-27"        "2021-03-28"       
#>                               o3__SESS_before2020 o4__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-01"        "2020-06-02"       
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             "2021-03-29"        "2021-03-30"       
#>                               o5__SESS_before2020 o6__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-03"        "2020-06-04"       
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             "2021-03-31"        "2021-04-01"       
#>                               o7__SESS_before2020 o8__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-05"        "2020-06-06"       
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             "2021-04-02"        "2021-04-03"       
#>                               o9__SESS_before2020 o10__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-07"        "2020-06-08"        
#> B_DL_val 5_beek kleine vijver NA                  NA                  
#> B_DL_val 3_dikke boom         NA                  NA                  
#> B_DM_val 4_'t WAD             "2021-04-04"        "2021-04-05"        
#>                               o11__SESS_before2020 o12__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-09"         "2020-06-10"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             "2021-04-06"         "2021-04-07"        
#>                               o13__SESS_before2020 o14__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-11"         "2020-06-12"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             "2021-04-08"         "2021-04-09"        
#>                               o15__SESS_before2020 o16__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-13"         "2020-06-14"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             "2021-04-10"         "2021-04-11"        
#>                               o17__SESS_before2020 o18__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-15"         "2020-06-16"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             "2021-04-12"         "2021-04-13"        
#>                               o19__SESS_before2020 o20__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-17"         "2020-06-18"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             "2021-04-14"         "2021-04-15"        
#>                               o21__SESS_before2020 o22__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-19"         "2020-06-20"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             "2021-04-16"         "2021-04-17"        
#>                               o23__SESS_before2020 o24__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-21"         "2020-06-22"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             "2021-04-18"         NA                  
#>                               o25__SESS_before2020 o26__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-23"         "2020-06-24"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             NA                   NA                  
#>                               o27__SESS_before2020 o28__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-25"         "2020-06-26"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             NA                   NA                  
#>                               o29__SESS_before2020 o30__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-27"         "2020-06-28"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             NA                   NA                  
#>                               o31__SESS_before2020 o32__SESS_before2020
#> B_HS_val 2_processiepark      "2020-06-29"         "2020-06-30"        
#> B_DL_val 5_beek kleine vijver NA                   NA                  
#> B_DL_val 3_dikke boom         NA                   NA                  
#> B_DM_val 4_'t WAD             NA                   NA                  
#>                               o33__SESS_before2020 o1__SESS_after2020
#> B_HS_val 2_processiepark      "2020-07-01"         NA                
#> B_DL_val 5_beek kleine vijver NA                   "2020-07-29"      
#> B_DL_val 3_dikke boom         NA                   "2020-06-19"      
#> B_DM_val 4_'t WAD             NA                   NA                
#>                               o2__SESS_after2020 o3__SESS_after2020
#> B_HS_val 2_processiepark      NA                 NA                
#> B_DL_val 5_beek kleine vijver "2020-07-30"       "2020-07-31"      
#> B_DL_val 3_dikke boom         "2020-06-20"       "2020-06-21"      
#> B_DM_val 4_'t WAD             NA                 NA                
#>                               o4__SESS_after2020 o5__SESS_after2020
#> B_HS_val 2_processiepark      NA                 NA                
#> B_DL_val 5_beek kleine vijver "2020-08-01"       "2020-08-02"      
#> B_DL_val 3_dikke boom         "2020-06-22"       "2020-06-23"      
#> B_DM_val 4_'t WAD             NA                 NA                
#>                               o6__SESS_after2020 o7__SESS_after2020
#> B_HS_val 2_processiepark      NA                 NA                
#> B_DL_val 5_beek kleine vijver "2020-08-03"       "2020-08-04"      
#> B_DL_val 3_dikke boom         "2020-06-24"       "2020-06-25"      
#> B_DM_val 4_'t WAD             NA                 NA                
#>                               o8__SESS_after2020 o9__SESS_after2020
#> B_HS_val 2_processiepark      NA                 NA                
#> B_DL_val 5_beek kleine vijver "2020-08-05"       "2020-08-06"      
#> B_DL_val 3_dikke boom         "2020-06-26"       "2020-06-27"      
#> B_DM_val 4_'t WAD             NA                 NA                
#>                               o10__SESS_after2020 o11__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver "2020-08-07"        "2020-08-08"       
#> B_DL_val 3_dikke boom         "2020-06-28"        NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o12__SESS_after2020 o13__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o14__SESS_after2020 o15__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o16__SESS_after2020 o17__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o18__SESS_after2020 o19__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o20__SESS_after2020 o21__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o22__SESS_after2020 o23__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o24__SESS_after2020 o25__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o26__SESS_after2020 o27__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o28__SESS_after2020 o29__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o30__SESS_after2020 o31__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#>                               o32__SESS_after2020 o33__SESS_after2020
#> B_HS_val 2_processiepark      NA                  NA                 
#> B_DL_val 5_beek kleine vijver NA                  NA                 
#> B_DL_val 3_dikke boom         NA                  NA                 
#> B_DM_val 4_'t WAD             NA                  NA                 
#> 
```
