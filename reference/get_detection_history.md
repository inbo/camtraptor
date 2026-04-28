# Get the detection history of a species

This function creates a detection history matrix for a species based on
the record table and the camera operation matrix. The detection history
matrix is a binary matrix where rows represent camera stations and
columns represent occasions. The matrix is filled with 1s and 0s, where
1 indicates that the species was detected at a station on a given
occasion and 0 indicates that the species was not detected. The function
also returns the effort matrix, which contains the number of days that
each station was active on each occasion, and the dates matrix, which
contains the dates of the occasions.

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
  `unmarked::unmarkedMultFrame()`). Default: `FALSE`.

## Value

A list with three elements:

- `detection_history`: the detection history matrix

- `effort`: the effort matrix

- `dates`: the dates matrix

## Details

This function doesn't take as input a camera trap data package object,
but a camera operation matrix and a record table, which are both
calculated based on a camera trap data package object. For more
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

Other camtrapR-derived functions:
[`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md),
[`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)

## Examples

``` r
library(dplyr)
camOp <- get_cam_op(mica)
recordTable <- get_record_table(mica)

# Binary output
get_detection_history(
  recordTable,
  camOp,
  species = "Anas platyrhynchos",
  output = "binary"
)
#> $detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_DL_val 5_beek kleine vijver  0  0  1  0  1  1  1  0  0   0   0  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0  0  0   0  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#> Mica Viane                     0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0   0   0
#> Mica Viane                      0  NA  NA  NA  NA  NA  NA  NA  NA
#> 
#> $effort
#>                                      o1 o2 o3 o4 o5 o6 o7 o8 o9       o10
#> B_DL_val 5_beek kleine vijver 0.7710532  1  1  1  1  1  1  1  1 1.0000000
#> B_DL_val 3_dikke boom         0.1250000  1  1  1  1  1  1  1  1 0.9815046
#> B_DM_val 4_'t WAD             0.1400694  1  1  1  1  1  1  1  1 1.0000000
#> Mica Viane                    0.5290856  1  1  1  1  1  1  1  1 1.0000000
#>                                     o11 o12 o13 o14       o15 o16 o17 o18 o19
#> B_DL_val 5_beek kleine vijver 0.1810185  NA  NA  NA        NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom                NA  NA  NA  NA        NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD             1.0000000   1   1   1 1.0000000   1   1   1   1
#> Mica Viane                    1.0000000   1   1   1 0.4168519  NA  NA  NA  NA
#>                               o20 o21 o22       o23
#> B_DL_val 5_beek kleine vijver  NA  NA  NA        NA
#> B_DL_val 3_dikke boom          NA  NA  NA        NA
#> B_DM_val 4_'t WAD               1   1   1 0.8923611
#> Mica Viane                     NA  NA  NA        NA
#> 
#> $dates
#>                               o1           o2           o3          
#> B_DL_val 5_beek kleine vijver "2020-07-29" "2020-07-30" "2020-07-31"
#> B_DL_val 3_dikke boom         "2020-06-19" "2020-06-20" "2020-06-21"
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-03-28" "2021-03-29"
#> Mica Viane                    "2019-10-09" "2019-10-10" "2019-10-11"
#>                               o4           o5           o6          
#> B_DL_val 5_beek kleine vijver "2020-08-01" "2020-08-02" "2020-08-03"
#> B_DL_val 3_dikke boom         "2020-06-22" "2020-06-23" "2020-06-24"
#> B_DM_val 4_'t WAD             "2021-03-30" "2021-03-31" "2021-04-01"
#> Mica Viane                    "2019-10-12" "2019-10-13" "2019-10-14"
#>                               o7           o8           o9          
#> B_DL_val 5_beek kleine vijver "2020-08-04" "2020-08-05" "2020-08-06"
#> B_DL_val 3_dikke boom         "2020-06-25" "2020-06-26" "2020-06-27"
#> B_DM_val 4_'t WAD             "2021-04-02" "2021-04-03" "2021-04-04"
#> Mica Viane                    "2019-10-15" "2019-10-16" "2019-10-17"
#>                               o10          o11          o12         
#> B_DL_val 5_beek kleine vijver "2020-08-07" "2020-08-08" NA          
#> B_DL_val 3_dikke boom         "2020-06-28" NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-05" "2021-04-06" "2021-04-07"
#> Mica Viane                    "2019-10-18" "2019-10-19" "2019-10-20"
#>                               o13          o14          o15         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-08" "2021-04-09" "2021-04-10"
#> Mica Viane                    "2019-10-21" "2019-10-22" "2019-10-23"
#>                               o16          o17          o18         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-11" "2021-04-12" "2021-04-13"
#> Mica Viane                    NA           NA           NA          
#>                               o19          o20          o21         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-14" "2021-04-15" "2021-04-16"
#> Mica Viane                    NA           NA           NA          
#>                               o22          o23         
#> B_DL_val 5_beek kleine vijver NA           NA          
#> B_DL_val 3_dikke boom         NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-17" "2021-04-18"
#> Mica Viane                    NA           NA          
#> 

# Number of observations output
get_detection_history(
 recordTable,
 camOp,
 species = "Anas platyrhynchos",
 output = "n_observations"
)
#> $detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_DL_val 5_beek kleine vijver  0  0  1  0  1  1  1  0  0   0   0  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0  0  0   0  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#> Mica Viane                     0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0   0   0
#> Mica Viane                      0  NA  NA  NA  NA  NA  NA  NA  NA
#> 
#> $effort
#>                                      o1 o2 o3 o4 o5 o6 o7 o8 o9       o10
#> B_DL_val 5_beek kleine vijver 0.7710532  1  1  1  1  1  1  1  1 1.0000000
#> B_DL_val 3_dikke boom         0.1250000  1  1  1  1  1  1  1  1 0.9815046
#> B_DM_val 4_'t WAD             0.1400694  1  1  1  1  1  1  1  1 1.0000000
#> Mica Viane                    0.5290856  1  1  1  1  1  1  1  1 1.0000000
#>                                     o11 o12 o13 o14       o15 o16 o17 o18 o19
#> B_DL_val 5_beek kleine vijver 0.1810185  NA  NA  NA        NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom                NA  NA  NA  NA        NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD             1.0000000   1   1   1 1.0000000   1   1   1   1
#> Mica Viane                    1.0000000   1   1   1 0.4168519  NA  NA  NA  NA
#>                               o20 o21 o22       o23
#> B_DL_val 5_beek kleine vijver  NA  NA  NA        NA
#> B_DL_val 3_dikke boom          NA  NA  NA        NA
#> B_DM_val 4_'t WAD               1   1   1 0.8923611
#> Mica Viane                     NA  NA  NA        NA
#> 
#> $dates
#>                               o1           o2           o3          
#> B_DL_val 5_beek kleine vijver "2020-07-29" "2020-07-30" "2020-07-31"
#> B_DL_val 3_dikke boom         "2020-06-19" "2020-06-20" "2020-06-21"
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-03-28" "2021-03-29"
#> Mica Viane                    "2019-10-09" "2019-10-10" "2019-10-11"
#>                               o4           o5           o6          
#> B_DL_val 5_beek kleine vijver "2020-08-01" "2020-08-02" "2020-08-03"
#> B_DL_val 3_dikke boom         "2020-06-22" "2020-06-23" "2020-06-24"
#> B_DM_val 4_'t WAD             "2021-03-30" "2021-03-31" "2021-04-01"
#> Mica Viane                    "2019-10-12" "2019-10-13" "2019-10-14"
#>                               o7           o8           o9          
#> B_DL_val 5_beek kleine vijver "2020-08-04" "2020-08-05" "2020-08-06"
#> B_DL_val 3_dikke boom         "2020-06-25" "2020-06-26" "2020-06-27"
#> B_DM_val 4_'t WAD             "2021-04-02" "2021-04-03" "2021-04-04"
#> Mica Viane                    "2019-10-15" "2019-10-16" "2019-10-17"
#>                               o10          o11          o12         
#> B_DL_val 5_beek kleine vijver "2020-08-07" "2020-08-08" NA          
#> B_DL_val 3_dikke boom         "2020-06-28" NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-05" "2021-04-06" "2021-04-07"
#> Mica Viane                    "2019-10-18" "2019-10-19" "2019-10-20"
#>                               o13          o14          o15         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-08" "2021-04-09" "2021-04-10"
#> Mica Viane                    "2019-10-21" "2019-10-22" "2019-10-23"
#>                               o16          o17          o18         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-11" "2021-04-12" "2021-04-13"
#> Mica Viane                    NA           NA           NA          
#>                               o19          o20          o21         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-14" "2021-04-15" "2021-04-16"
#> Mica Viane                    NA           NA           NA          
#>                               o22          o23         
#> B_DL_val 5_beek kleine vijver NA           NA          
#> B_DL_val 3_dikke boom         NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-17" "2021-04-18"
#> Mica Viane                    NA           NA          
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
#> B_DL_val 5_beek kleine vijver  0  0  2  0  5  3  3  0  0   0   0  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0  0  0   0  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#> Mica Viane                     0  0  0  0  0  0  0  0  0   0   0   0   0   0
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0   0   0
#> Mica Viane                      0  NA  NA  NA  NA  NA  NA  NA  NA
#> 
#> $effort
#>                                      o1 o2 o3 o4 o5 o6 o7 o8 o9       o10
#> B_DL_val 5_beek kleine vijver 0.7710532  1  1  1  1  1  1  1  1 1.0000000
#> B_DL_val 3_dikke boom         0.1250000  1  1  1  1  1  1  1  1 0.9815046
#> B_DM_val 4_'t WAD             0.1400694  1  1  1  1  1  1  1  1 1.0000000
#> Mica Viane                    0.5290856  1  1  1  1  1  1  1  1 1.0000000
#>                                     o11 o12 o13 o14       o15 o16 o17 o18 o19
#> B_DL_val 5_beek kleine vijver 0.1810185  NA  NA  NA        NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom                NA  NA  NA  NA        NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD             1.0000000   1   1   1 1.0000000   1   1   1   1
#> Mica Viane                    1.0000000   1   1   1 0.4168519  NA  NA  NA  NA
#>                               o20 o21 o22       o23
#> B_DL_val 5_beek kleine vijver  NA  NA  NA        NA
#> B_DL_val 3_dikke boom          NA  NA  NA        NA
#> B_DM_val 4_'t WAD               1   1   1 0.8923611
#> Mica Viane                     NA  NA  NA        NA
#> 
#> $dates
#>                               o1           o2           o3          
#> B_DL_val 5_beek kleine vijver "2020-07-29" "2020-07-30" "2020-07-31"
#> B_DL_val 3_dikke boom         "2020-06-19" "2020-06-20" "2020-06-21"
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-03-28" "2021-03-29"
#> Mica Viane                    "2019-10-09" "2019-10-10" "2019-10-11"
#>                               o4           o5           o6          
#> B_DL_val 5_beek kleine vijver "2020-08-01" "2020-08-02" "2020-08-03"
#> B_DL_val 3_dikke boom         "2020-06-22" "2020-06-23" "2020-06-24"
#> B_DM_val 4_'t WAD             "2021-03-30" "2021-03-31" "2021-04-01"
#> Mica Viane                    "2019-10-12" "2019-10-13" "2019-10-14"
#>                               o7           o8           o9          
#> B_DL_val 5_beek kleine vijver "2020-08-04" "2020-08-05" "2020-08-06"
#> B_DL_val 3_dikke boom         "2020-06-25" "2020-06-26" "2020-06-27"
#> B_DM_val 4_'t WAD             "2021-04-02" "2021-04-03" "2021-04-04"
#> Mica Viane                    "2019-10-15" "2019-10-16" "2019-10-17"
#>                               o10          o11          o12         
#> B_DL_val 5_beek kleine vijver "2020-08-07" "2020-08-08" NA          
#> B_DL_val 3_dikke boom         "2020-06-28" NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-05" "2021-04-06" "2021-04-07"
#> Mica Viane                    "2019-10-18" "2019-10-19" "2019-10-20"
#>                               o13          o14          o15         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-08" "2021-04-09" "2021-04-10"
#> Mica Viane                    "2019-10-21" "2019-10-22" "2019-10-23"
#>                               o16          o17          o18         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-11" "2021-04-12" "2021-04-13"
#> Mica Viane                    NA           NA           NA          
#>                               o19          o20          o21         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-14" "2021-04-15" "2021-04-16"
#> Mica Viane                    NA           NA           NA          
#>                               o22          o23         
#> B_DL_val 5_beek kleine vijver NA           NA          
#> B_DL_val 3_dikke boom         NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-17" "2021-04-18"
#> Mica Viane                    NA           NA          
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
#>                               o1 o2 o3 o4
#> B_DL_val 5_beek kleine vijver 13  0 NA NA
#> B_DL_val 3_dikke boom          0  0 NA NA
#> B_DM_val 4_'t WAD              0  0  0  0
#> Mica Viane                     0  0  0 NA
#> 
#> $effort
#>                                     o1       o2        o3       o4
#> B_DL_val 5_beek kleine vijver 6.771053 3.181019        NA       NA
#> B_DL_val 3_dikke boom         6.125000 2.981505        NA       NA
#> B_DM_val 4_'t WAD             6.140069 7.000000 7.0000000 1.892361
#> Mica Viane                    6.529086 7.000000 0.4168519       NA
#> 
#> $dates
#>                               o1           o2           o3          
#> B_DL_val 5_beek kleine vijver "2020-07-29" "2020-08-05" NA          
#> B_DL_val 3_dikke boom         "2020-06-19" "2020-06-26" NA          
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-04-03" "2021-04-10"
#> Mica Viane                    "2019-10-09" "2019-10-16" "2019-10-23"
#>                               o4          
#> B_DL_val 5_beek kleine vijver NA          
#> B_DL_val 3_dikke boom         NA          
#> B_DM_val 4_'t WAD             "2021-04-17"
#> Mica Viane                    NA          
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
#>                               o1 o2 o3 o4
#> B_DL_val 5_beek kleine vijver 13 NA NA NA
#> B_DL_val 3_dikke boom          0 NA NA NA
#> B_DM_val 4_'t WAD              0  0  0 NA
#> Mica Viane                     0  0 NA NA
#> 
#> $effort
#>                                     o1 o2 o3 o4
#> B_DL_val 5_beek kleine vijver 6.771053 NA NA NA
#> B_DL_val 3_dikke boom         6.125000 NA NA NA
#> B_DM_val 4_'t WAD             6.140069  7  7 NA
#> Mica Viane                    6.529086  7 NA NA
#> 
#> $dates
#>                               o1           o2           o3           o4
#> B_DL_val 5_beek kleine vijver "2020-07-29" NA           NA           NA
#> B_DL_val 3_dikke boom         "2020-06-19" NA           NA           NA
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-04-03" "2021-04-10" NA
#> Mica Viane                    "2019-10-09" "2019-10-16" NA           NA
#> 

# Use a `maxNumberDays` of 5 days
get_detection_history(
 recordTable,
 camOp,
 species = "Anas platyrhynchos",
 output = "n_individuals",
 maxNumberDays = 5
)
#> Warning: 2 record(s) (out of 4) are removed because they were taken after `maxNumberDays` (5 days) the first day of each station, e.g.:
#> B_DL_val 5_beek kleine vijver: 2020-08-03.
#> $detection_history
#>                               o1 o2 o3 o4 o5
#> B_DL_val 5_beek kleine vijver  0  0  2  0  5
#> B_DL_val 3_dikke boom          0  0  0  0  0
#> B_DM_val 4_'t WAD              0  0  0  0  0
#> Mica Viane                     0  0  0  0  0
#> 
#> $effort
#>                                      o1 o2 o3 o4 o5
#> B_DL_val 5_beek kleine vijver 0.7710532  1  1  1  1
#> B_DL_val 3_dikke boom         0.1250000  1  1  1  1
#> B_DM_val 4_'t WAD             0.1400694  1  1  1  1
#> Mica Viane                    0.5290856  1  1  1  1
#> 
#> $dates
#>                               o1           o2           o3          
#> B_DL_val 5_beek kleine vijver "2020-07-29" "2020-07-30" "2020-07-31"
#> B_DL_val 3_dikke boom         "2020-06-19" "2020-06-20" "2020-06-21"
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-03-28" "2021-03-29"
#> Mica Viane                    "2019-10-09" "2019-10-10" "2019-10-11"
#>                               o4           o5          
#> B_DL_val 5_beek kleine vijver "2020-08-01" "2020-08-02"
#> B_DL_val 3_dikke boom         "2020-06-22" "2020-06-23"
#> B_DM_val 4_'t WAD             "2021-03-30" "2021-03-31"
#> Mica Viane                    "2019-10-12" "2019-10-13"
#> 

# Specify start date via `day1`
get_detection_history(
  recordTable,
  camOp,
  species = "Anas platyrhynchos",
  output = "binary",
  day1 = "2020-06-22"
)
#> $detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_DL_val 5_beek kleine vijver  0  0  1  0  1  1  1  0  0   0   0  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0 NA NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#> Mica Viane                    NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA
#>                               o15 o16 o17 o18 o19 o20 o21 o22 o23
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0   0   0
#> Mica Viane                     NA  NA  NA  NA  NA  NA  NA  NA  NA
#> 
#> $effort
#>                                      o1 o2 o3 o4 o5 o6        o7 o8 o9 o10
#> B_DL_val 5_beek kleine vijver 0.7710532  1  1  1  1  1 1.0000000  1  1   1
#> B_DL_val 3_dikke boom         1.0000000  1  1  1  1  1 0.9815046 NA NA  NA
#> B_DM_val 4_'t WAD             0.1400694  1  1  1  1  1 1.0000000  1  1   1
#> Mica Viane                           NA NA NA NA NA NA        NA NA NA  NA
#>                                     o11 o12 o13 o14 o15 o16 o17 o18 o19 o20 o21
#> B_DL_val 5_beek kleine vijver 0.1810185  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom                NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD             1.0000000   1   1   1   1   1   1   1   1   1   1
#> Mica Viane                           NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#>                               o22       o23
#> B_DL_val 5_beek kleine vijver  NA        NA
#> B_DL_val 3_dikke boom          NA        NA
#> B_DM_val 4_'t WAD               1 0.8923611
#> Mica Viane                     NA        NA
#> 
#> $dates
#>                               o1           o2           o3          
#> B_DL_val 5_beek kleine vijver "2020-07-29" "2020-07-30" "2020-07-31"
#> B_DL_val 3_dikke boom         "2020-06-22" "2020-06-23" "2020-06-24"
#> B_DM_val 4_'t WAD             "2021-03-27" "2021-03-28" "2021-03-29"
#> Mica Viane                    NA           NA           NA          
#>                               o4           o5           o6          
#> B_DL_val 5_beek kleine vijver "2020-08-01" "2020-08-02" "2020-08-03"
#> B_DL_val 3_dikke boom         "2020-06-25" "2020-06-26" "2020-06-27"
#> B_DM_val 4_'t WAD             "2021-03-30" "2021-03-31" "2021-04-01"
#> Mica Viane                    NA           NA           NA          
#>                               o7           o8           o9          
#> B_DL_val 5_beek kleine vijver "2020-08-04" "2020-08-05" "2020-08-06"
#> B_DL_val 3_dikke boom         "2020-06-28" NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-02" "2021-04-03" "2021-04-04"
#> Mica Viane                    NA           NA           NA          
#>                               o10          o11          o12         
#> B_DL_val 5_beek kleine vijver "2020-08-07" "2020-08-08" NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-05" "2021-04-06" "2021-04-07"
#> Mica Viane                    NA           NA           NA          
#>                               o13          o14          o15         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-08" "2021-04-09" "2021-04-10"
#> Mica Viane                    NA           NA           NA          
#>                               o16          o17          o18         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-11" "2021-04-12" "2021-04-13"
#> Mica Viane                    NA           NA           NA          
#>                               o19          o20          o21         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-14" "2021-04-15" "2021-04-16"
#> Mica Viane                    NA           NA           NA          
#>                               o22          o23         
#> B_DL_val 5_beek kleine vijver NA           NA          
#> B_DL_val 3_dikke boom         NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-17" "2021-04-18"
#> Mica Viane                    NA           NA          
#> 

# Use a `buffer` of 2 days
get_detection_history(
 recordTable,
 camOp,
 species = "Anas platyrhynchos",
 output = "n_individuals",
 buffer = 2
)
#> $detection_history
#>                               o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 o11 o12 o13 o14
#> B_DL_val 5_beek kleine vijver  2  0  5  3  3  0  0  0  0  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          0  0  0  0  0  0  0  0 NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD              0  0  0  0  0  0  0  0  0   0   0   0   0   0
#> Mica Viane                     0  0  0  0  0  0  0  0  0   0   0   0   0  NA
#>                               o15 o16 o17 o18 o19 o20 o21
#> B_DL_val 5_beek kleine vijver  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               0   0   0   0   0   0   0
#> Mica Viane                     NA  NA  NA  NA  NA  NA  NA
#> 
#> $effort
#>                               o1 o2 o3 o4 o5 o6 o7        o8        o9 o10 o11
#> B_DL_val 5_beek kleine vijver  1  1  1  1  1  1  1 1.0000000 0.1810185  NA  NA
#> B_DL_val 3_dikke boom          1  1  1  1  1  1  1 0.9815046        NA  NA  NA
#> B_DM_val 4_'t WAD              1  1  1  1  1  1  1 1.0000000 1.0000000   1   1
#> Mica Viane                     1  1  1  1  1  1  1 1.0000000 1.0000000   1   1
#>                               o12       o13 o14 o15 o16 o17 o18 o19 o20
#> B_DL_val 5_beek kleine vijver  NA        NA  NA  NA  NA  NA  NA  NA  NA
#> B_DL_val 3_dikke boom          NA        NA  NA  NA  NA  NA  NA  NA  NA
#> B_DM_val 4_'t WAD               1 1.0000000   1   1   1   1   1   1   1
#> Mica Viane                      1 0.4168519  NA  NA  NA  NA  NA  NA  NA
#>                                     o21
#> B_DL_val 5_beek kleine vijver        NA
#> B_DL_val 3_dikke boom                NA
#> B_DM_val 4_'t WAD             0.8923611
#> Mica Viane                           NA
#> 
#> $dates
#>                               o1           o2           o3          
#> B_DL_val 5_beek kleine vijver "2020-07-31" "2020-08-01" "2020-08-02"
#> B_DL_val 3_dikke boom         "2020-06-21" "2020-06-22" "2020-06-23"
#> B_DM_val 4_'t WAD             "2021-03-29" "2021-03-30" "2021-03-31"
#> Mica Viane                    "2019-10-11" "2019-10-12" "2019-10-13"
#>                               o4           o5           o6          
#> B_DL_val 5_beek kleine vijver "2020-08-03" "2020-08-04" "2020-08-05"
#> B_DL_val 3_dikke boom         "2020-06-24" "2020-06-25" "2020-06-26"
#> B_DM_val 4_'t WAD             "2021-04-01" "2021-04-02" "2021-04-03"
#> Mica Viane                    "2019-10-14" "2019-10-15" "2019-10-16"
#>                               o7           o8           o9          
#> B_DL_val 5_beek kleine vijver "2020-08-06" "2020-08-07" "2020-08-08"
#> B_DL_val 3_dikke boom         "2020-06-27" "2020-06-28" NA          
#> B_DM_val 4_'t WAD             "2021-04-04" "2021-04-05" "2021-04-06"
#> Mica Viane                    "2019-10-17" "2019-10-18" "2019-10-19"
#>                               o10          o11          o12         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-07" "2021-04-08" "2021-04-09"
#> Mica Viane                    "2019-10-20" "2019-10-21" "2019-10-22"
#>                               o13          o14          o15         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-10" "2021-04-11" "2021-04-12"
#> Mica Viane                    "2019-10-23" NA           NA          
#>                               o16          o17          o18         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-13" "2021-04-14" "2021-04-15"
#> Mica Viane                    NA           NA           NA          
#>                               o19          o20          o21         
#> B_DL_val 5_beek kleine vijver NA           NA           NA          
#> B_DL_val 3_dikke boom         NA           NA           NA          
#> B_DM_val 4_'t WAD             "2021-04-16" "2021-04-17" "2021-04-18"
#> Mica Viane                    NA           NA           NA          
#> 

# Multi-season detection history

# Create a multi-season camera operation matrix / record table
mica_sessions <- mica
mica_sessions$data$deployments$session <- c("2020", "2020", "2021", "2021")
mica_sessions$data$deployments$locationID <- c(
  mica_sessions$data$deployments$locationID[1:2],
  mica_sessions$data$deployments$locationID[1:2]
)
mica_sessions$data$deployments$locationName <- c(
  mica_sessions$data$deployments$locationName[1:2],
  mica_sessions$data$deployments$locationName[1:2]
)
delta <- lubridate::duration(2, units = "years")
mica_sessions$data$deployments$start[4] <- mica_sessions$data$deployments$start[4] + delta
mica_sessions$data$deployments$end[4] <- mica_sessions$data$deployments$end[4] + delta
mica_sessions$data$observations <- mica_sessions$data$observations %>%
dplyr::mutate(timestamp = dplyr::if_else(
  deploymentID %in% mica_sessions$data$deployments$deploymentID[4],
  timestamp + delta,
  timestamp
  )
)
mica_sessions$data$media <- mica_sessions$data$media %>%
dplyr::mutate(
 timestamp = dplyr::if_else(
 deploymentID %in% mica_sessions$data$deployments$deploymentID[4],
 timestamp + delta,
 timestamp
 )
)
camOp_sessions <- get_cam_op(mica_sessions, session_col = "session")
recordTable_sessions <- get_record_table(mica_sessions)

# Create a multi-season detection history
get_detection_history(
  recordTable_sessions,
  camOp_sessions,
  species = "Anas platyrhynchos",
  output = "n_individuals",
  unmarkedMultFrameInput = TRUE
)
#> $detection_history
#>                               o1__SESS_2020 o2__SESS_2020 o3__SESS_2020
#> B_DL_val 5_beek kleine vijver             0             0             2
#> B_DL_val 3_dikke boom                     0             0             0
#>                               o4__SESS_2020 o5__SESS_2020 o6__SESS_2020
#> B_DL_val 5_beek kleine vijver             0             5             3
#> B_DL_val 3_dikke boom                     0             0             0
#>                               o7__SESS_2020 o8__SESS_2020 o9__SESS_2020
#> B_DL_val 5_beek kleine vijver             3             0             0
#> B_DL_val 3_dikke boom                     0             0             0
#>                               o10__SESS_2020 o11__SESS_2020 o12__SESS_2020
#> B_DL_val 5_beek kleine vijver              0              0             NA
#> B_DL_val 3_dikke boom                      0             NA             NA
#>                               o13__SESS_2020 o14__SESS_2020 o15__SESS_2020
#> B_DL_val 5_beek kleine vijver             NA             NA             NA
#> B_DL_val 3_dikke boom                     NA             NA             NA
#>                               o16__SESS_2020 o17__SESS_2020 o18__SESS_2020
#> B_DL_val 5_beek kleine vijver             NA             NA             NA
#> B_DL_val 3_dikke boom                     NA             NA             NA
#>                               o19__SESS_2020 o20__SESS_2020 o21__SESS_2020
#> B_DL_val 5_beek kleine vijver             NA             NA             NA
#> B_DL_val 3_dikke boom                     NA             NA             NA
#>                               o22__SESS_2020 o23__SESS_2020 o1__SESS_2021
#> B_DL_val 5_beek kleine vijver             NA             NA             0
#> B_DL_val 3_dikke boom                     NA             NA             0
#>                               o2__SESS_2021 o3__SESS_2021 o4__SESS_2021
#> B_DL_val 5_beek kleine vijver             0             0             0
#> B_DL_val 3_dikke boom                     0             0             0
#>                               o5__SESS_2021 o6__SESS_2021 o7__SESS_2021
#> B_DL_val 5_beek kleine vijver             0             0             0
#> B_DL_val 3_dikke boom                     0             0             0
#>                               o8__SESS_2021 o9__SESS_2021 o10__SESS_2021
#> B_DL_val 5_beek kleine vijver             0             0              0
#> B_DL_val 3_dikke boom                     0             0              0
#>                               o11__SESS_2021 o12__SESS_2021 o13__SESS_2021
#> B_DL_val 5_beek kleine vijver              0              0              0
#> B_DL_val 3_dikke boom                      0              0              0
#>                               o14__SESS_2021 o15__SESS_2021 o16__SESS_2021
#> B_DL_val 5_beek kleine vijver              0              0              0
#> B_DL_val 3_dikke boom                      0              0             NA
#>                               o17__SESS_2021 o18__SESS_2021 o19__SESS_2021
#> B_DL_val 5_beek kleine vijver              0              0              0
#> B_DL_val 3_dikke boom                     NA             NA             NA
#>                               o20__SESS_2021 o21__SESS_2021 o22__SESS_2021
#> B_DL_val 5_beek kleine vijver              0              0              0
#> B_DL_val 3_dikke boom                     NA             NA             NA
#>                               o23__SESS_2021
#> B_DL_val 5_beek kleine vijver              0
#> B_DL_val 3_dikke boom                     NA
#> 
#> $effort
#>                               o1__SESS_2020 o2__SESS_2020 o3__SESS_2020
#> B_DL_val 5_beek kleine vijver     0.7710532             1             1
#> B_DL_val 3_dikke boom             0.1250000             1             1
#>                               o4__SESS_2020 o5__SESS_2020 o6__SESS_2020
#> B_DL_val 5_beek kleine vijver             1             1             1
#> B_DL_val 3_dikke boom                     1             1             1
#>                               o7__SESS_2020 o8__SESS_2020 o9__SESS_2020
#> B_DL_val 5_beek kleine vijver             1             1             1
#> B_DL_val 3_dikke boom                     1             1             1
#>                               o10__SESS_2020 o11__SESS_2020 o12__SESS_2020
#> B_DL_val 5_beek kleine vijver      1.0000000      0.1810185             NA
#> B_DL_val 3_dikke boom              0.9815046             NA             NA
#>                               o13__SESS_2020 o14__SESS_2020 o15__SESS_2020
#> B_DL_val 5_beek kleine vijver             NA             NA             NA
#> B_DL_val 3_dikke boom                     NA             NA             NA
#>                               o16__SESS_2020 o17__SESS_2020 o18__SESS_2020
#> B_DL_val 5_beek kleine vijver             NA             NA             NA
#> B_DL_val 3_dikke boom                     NA             NA             NA
#>                               o19__SESS_2020 o20__SESS_2020 o21__SESS_2020
#> B_DL_val 5_beek kleine vijver             NA             NA             NA
#> B_DL_val 3_dikke boom                     NA             NA             NA
#>                               o22__SESS_2020 o23__SESS_2020 o1__SESS_2021
#> B_DL_val 5_beek kleine vijver             NA             NA    0.14006944
#> B_DL_val 3_dikke boom                     NA             NA    0.02908565
#>                               o2__SESS_2021 o3__SESS_2021 o4__SESS_2021
#> B_DL_val 5_beek kleine vijver             1             1             1
#> B_DL_val 3_dikke boom                     1             1             1
#>                               o5__SESS_2021 o6__SESS_2021 o7__SESS_2021
#> B_DL_val 5_beek kleine vijver             1             1             1
#> B_DL_val 3_dikke boom                     1             1             1
#>                               o8__SESS_2021 o9__SESS_2021 o10__SESS_2021
#> B_DL_val 5_beek kleine vijver             1             1              1
#> B_DL_val 3_dikke boom                     1             1              1
#>                               o11__SESS_2021 o12__SESS_2021 o13__SESS_2021
#> B_DL_val 5_beek kleine vijver              1              1              1
#> B_DL_val 3_dikke boom                      1              1              1
#>                               o14__SESS_2021 o15__SESS_2021 o16__SESS_2021
#> B_DL_val 5_beek kleine vijver              1      1.0000000              1
#> B_DL_val 3_dikke boom                      1      0.9168519             NA
#>                               o17__SESS_2021 o18__SESS_2021 o19__SESS_2021
#> B_DL_val 5_beek kleine vijver              1              1              1
#> B_DL_val 3_dikke boom                     NA             NA             NA
#>                               o20__SESS_2021 o21__SESS_2021 o22__SESS_2021
#> B_DL_val 5_beek kleine vijver              1              1              1
#> B_DL_val 3_dikke boom                     NA             NA             NA
#>                               o23__SESS_2021
#> B_DL_val 5_beek kleine vijver      0.8923611
#> B_DL_val 3_dikke boom                     NA
#> 
#> $dates
#>                               o1__SESS_2020 o2__SESS_2020 o3__SESS_2020
#> B_DL_val 5_beek kleine vijver "2020-07-29"  "2020-07-30"  "2020-07-31" 
#> B_DL_val 3_dikke boom         "2020-06-19"  "2020-06-20"  "2020-06-21" 
#>                               o4__SESS_2020 o5__SESS_2020 o6__SESS_2020
#> B_DL_val 5_beek kleine vijver "2020-08-01"  "2020-08-02"  "2020-08-03" 
#> B_DL_val 3_dikke boom         "2020-06-22"  "2020-06-23"  "2020-06-24" 
#>                               o7__SESS_2020 o8__SESS_2020 o9__SESS_2020
#> B_DL_val 5_beek kleine vijver "2020-08-04"  "2020-08-05"  "2020-08-06" 
#> B_DL_val 3_dikke boom         "2020-06-25"  "2020-06-26"  "2020-06-27" 
#>                               o10__SESS_2020 o11__SESS_2020 o12__SESS_2020
#> B_DL_val 5_beek kleine vijver "2020-08-07"   "2020-08-08"   NA            
#> B_DL_val 3_dikke boom         "2020-06-28"   NA             NA            
#>                               o13__SESS_2020 o14__SESS_2020 o15__SESS_2020
#> B_DL_val 5_beek kleine vijver NA             NA             NA            
#> B_DL_val 3_dikke boom         NA             NA             NA            
#>                               o16__SESS_2020 o17__SESS_2020 o18__SESS_2020
#> B_DL_val 5_beek kleine vijver NA             NA             NA            
#> B_DL_val 3_dikke boom         NA             NA             NA            
#>                               o19__SESS_2020 o20__SESS_2020 o21__SESS_2020
#> B_DL_val 5_beek kleine vijver NA             NA             NA            
#> B_DL_val 3_dikke boom         NA             NA             NA            
#>                               o22__SESS_2020 o23__SESS_2020 o1__SESS_2021
#> B_DL_val 5_beek kleine vijver NA             NA             "2021-03-27" 
#> B_DL_val 3_dikke boom         NA             NA             "2021-10-08" 
#>                               o2__SESS_2021 o3__SESS_2021 o4__SESS_2021
#> B_DL_val 5_beek kleine vijver "2021-03-28"  "2021-03-29"  "2021-03-30" 
#> B_DL_val 3_dikke boom         "2021-10-09"  "2021-10-10"  "2021-10-11" 
#>                               o5__SESS_2021 o6__SESS_2021 o7__SESS_2021
#> B_DL_val 5_beek kleine vijver "2021-03-31"  "2021-04-01"  "2021-04-02" 
#> B_DL_val 3_dikke boom         "2021-10-12"  "2021-10-13"  "2021-10-14" 
#>                               o8__SESS_2021 o9__SESS_2021 o10__SESS_2021
#> B_DL_val 5_beek kleine vijver "2021-04-03"  "2021-04-04"  "2021-04-05"  
#> B_DL_val 3_dikke boom         "2021-10-15"  "2021-10-16"  "2021-10-17"  
#>                               o11__SESS_2021 o12__SESS_2021 o13__SESS_2021
#> B_DL_val 5_beek kleine vijver "2021-04-06"   "2021-04-07"   "2021-04-08"  
#> B_DL_val 3_dikke boom         "2021-10-18"   "2021-10-19"   "2021-10-20"  
#>                               o14__SESS_2021 o15__SESS_2021 o16__SESS_2021
#> B_DL_val 5_beek kleine vijver "2021-04-09"   "2021-04-10"   "2021-04-11"  
#> B_DL_val 3_dikke boom         "2021-10-21"   "2021-10-22"   NA            
#>                               o17__SESS_2021 o18__SESS_2021 o19__SESS_2021
#> B_DL_val 5_beek kleine vijver "2021-04-12"   "2021-04-13"   "2021-04-14"  
#> B_DL_val 3_dikke boom         NA             NA             NA            
#>                               o20__SESS_2021 o21__SESS_2021 o22__SESS_2021
#> B_DL_val 5_beek kleine vijver "2021-04-15"   "2021-04-16"   "2021-04-17"  
#> B_DL_val 3_dikke boom         NA             NA             NA            
#>                               o23__SESS_2021
#> B_DL_val 5_beek kleine vijver "2021-04-18"  
#> B_DL_val 3_dikke boom         NA            
#> 
```
