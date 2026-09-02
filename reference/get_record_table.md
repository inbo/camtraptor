# Get the record table

**\[superseded\]**

This function is superseded because camtrapR now supports reading Camera
Trap Data Packages. Use
[`camtrapR::readCamtrapDP()`](https://jniedballa.github.io/camtrapR/reference/readCamtrapDP.html)
and
[`camtrapR::recordTable()`](https://jniedballa.github.io/camtrapR/reference/recordTable.html)
instead.

Creates the record table from a Camera Trap Data Package and so
tabulating species records. Only event-based observations and their
corresponding media are taken into account. The record table is a
concept developed within the camtrapR package, see [this
article](https://jniedballa.github.io/camtrapR/articles/camtrapr3.html).
See also the function documentation for
[`camtrapR::recordTable()`](https://jniedballa.github.io/camtrapR/reference/recordTable.html).

**Note**: All dates and times are expressed in UTC format.

## Usage

``` r
get_record_table(
  x,
  stationCol = "locationName",
  exclude = NULL,
  minDeltaTime = 0,
  deltaTimeComparedTo = NULL,
  removeDuplicateRecords = TRUE
)
```

## Arguments

- x:

  Camera trap data package object, as returned by
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

- stationCol:

  Character name of the column containing stations. Default:
  `"locationName"`.

- exclude:

  Character vector of scientific names to be excluded from the record
  table. Default: `NULL`.

- minDeltaTime:

  Time difference between records of the same species at the same
  station to be considered independent (in minutes). Default: 0.

- deltaTimeComparedTo:

  One of `"lastIndependentRecord"` or `"lastRecord"`. For two records to
  be considered independent, the second one must be at least
  `minDeltaTime` minutes after the last independent record of the same
  species (`deltaTimeComparedTo = "lastIndependentRecord"`), or
  `minDeltaTime` minutes after the last record
  (`deltaTimeComparedTo = "lastRecord"`). If `minDeltaTime` is 0,
  `deltaTimeComparedTo` must be `NULL` (default).

- removeDuplicateRecords:

  Logical. If there are several records of the same species, but e.g.
  different `sex` or `lifeStage`, at the same station at exactly the
  same time, show only one? Default: `TRUE`. Duplicates are removed by
  keeping only the first observation in the observation table.

## Value

A tibble data frame containing species records and additional
information about stations, date, time and further metadata, such as
filenames and directories of the images (media) linked to the species
records. Some more details about the columns returned:

- `Station`: Character, station names, as found in the deployment column
  defined in parameter `stationCol`.

- `Species`: Character, the scientific name of the observed species.

- `n`: Numeric, the number of observed individuals (renamed from
  [`count`](https://camtrap-dp.tdwg.org/data/#observations.count) in the
  observations table).

- `DateTimeOriginal`: Datetime object, as found in column `eventStart`
  of `observations`, in UTC format.

- `Date`: Date object, the date part of `DateTimeOriginal`, in UTC
  format.

- `Time`: Character, the time part of `DateTimeOriginal` in UTC format.

- `delta.time.secs`: Numeric, the duration in seconds from the previous
  independent record of a given species at a certain location.

- `delta.time.mins`: Numeric, the duration in minutes from the previous
  independent record of a given species at a certain location.

- `delta.time.hours`: Numeric, the duration in hours from the previous
  independent record of a given species at a certain location.

- `delta.time.days`: Numeric, the duration in days from the previous
  independent record of a given species at a certain location.

- `Directory`: List, file paths of the images linked to the given
  record, as defined in column `filePath` of `media`.

- `Filename`: List, file names of the images linked to the given record,
  as defined in column `fileName` of `media`.

- `Latitude`: Numeric, latitude of the station, based on `deploymentID`
  of the observations.

- `Longitude`: Numeric, longitude of the station, based on
  `deploymentID` of the observations.

- `clock`: Numeric, clock time in radians.

- `solar`: Numeric, solar time in radians. Calculated using
  [`overlap::sunTime`](https://rdrr.io/pkg/overlap/man/sunTime.html),
  which essentially uses the approach described in Nouvellet et
  al. (2012)
  [doi:10.1111/j.1469-7998.2011.00864.x](https://doi.org/10.1111/j.1469-7998.2011.00864.x)
  .

## See also

Other deprecated camtrapR-derived functions:
[`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md),
[`get_detection_history()`](https://inbo.github.io/camtraptor/reference/get_detection_history.md)

## Examples

``` r
library(lubridate)

x <- example_dataset()
get_record_table(x)
#> # A tibble: 26 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas p…     2 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  2 B_DL_val … Anas p…     2 2020-07-30 04:29:31 2020-07-30 04:2…           81763
#>  3 B_DL_val … Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…           87242
#>  4 B_DL_val … Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  5 B_DL_val … Anas p…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  6 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  7 B_HS_val … Anas p…     1 2020-05-30 02:57:37 2020-05-30 02:5…               0
#>  8 B_HS_val … Anas p…     2 2020-05-31 04:05:10 2020-05-31 04:0…           90453
#>  9 B_HS_val … Anas p…     1 2020-06-06 04:11:07 2020-06-06 04:1…          518757
#> 10 B_HS_val … Anas p…     4 2020-06-09 03:16:11 2020-06-09 03:1…          255904
#> # ℹ 16 more rows
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# Create a new Camera Trap Data Package with dependent observations only for
# demonstration.
obs <- observations(x)
obs[obs$observationID == "9e191d10",]$scientificName <- "Martes foina"
x_dep <- x
observations(x_dep) <- obs

# Set a minDeltaTime of 100 minutes from last record
get_record_table(
  x_dep,
  minDeltaTime = 100,
  deltaTimeComparedTo = "lastRecord"
)
#> Number of not independent observations to be removed: 1
#> # A tibble: 25 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas p…     2 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  2 B_DL_val … Anas p…     2 2020-07-30 04:29:31 2020-07-30 04:2…           81763
#>  3 B_DL_val … Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…           87242
#>  4 B_DL_val … Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  5 B_DL_val … Anas p…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  6 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  7 B_HS_val … Anas p…     1 2020-05-30 02:57:37 2020-05-30 02:5…               0
#>  8 B_HS_val … Anas p…     2 2020-05-31 04:05:10 2020-05-31 04:0…           90453
#>  9 B_HS_val … Anas p…     1 2020-06-06 04:11:07 2020-06-06 04:1…          518757
#> 10 B_HS_val … Anas p…     4 2020-06-09 03:16:11 2020-06-09 03:1…          255904
#> # ℹ 15 more rows
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# Differences can occur between `deltaTimeCoparedTo` = `"lastRecord"` and
# `"lastIndependentRecord"`
obs <- observations(x)
obs[obs$eventID == "02ae9f43", "eventStart"] <- 
  as_datetime("2020-08-02 05:10:20")

med <- media(x) 
rows_to_update <- which(med$eventID == "02ae9f43") 
med[rows_to_update, "timestamp"] <- as_datetime("2020-08-02 05:10:20") 

x_modified <- x
observations(x_modified) <- obs
media(x_modified) <- med

rec_last_indep <- get_record_table(
  x_modified,
  minDeltaTime = 10,
  deltaTimeComparedTo = "lastIndependentRecord"
)

rec_last <- get_record_table(
  x_modified,
  minDeltaTime = 10,
  deltaTimeComparedTo = "lastRecord"
)
#> Number of not independent observations to be removed: 1

# Exclude observations of Anas platyrhynchos.
get_record_table(x, exclude = "Anas platyrhynchos")
#> # A tibble: 14 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas s…     3 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  2 B_DL_val … Anas s…     1 2020-08-05 05:02:01 2020-08-05 05:0…          602113
#>  3 B_DM_val … Ardea       1 2021-04-05 19:08:33 2021-04-05 19:0…               0
#>  4 B_DM_val … Ardea       1 2021-04-11 19:43:09 2021-04-11 19:4…          520476
#>  5 B_HS_val … Ardea …     1 2020-06-12 04:04:29 2020-06-12 04:0…               0
#>  6 B_DL_val … Aves        1 2020-08-08 04:20:35 2020-08-08 04:2…               0
#>  7 B_DM_val … Aves        1 2021-03-27 20:38:18 2021-03-27 20:3…               0
#>  8 B_DL_val … Martes…     1 2020-06-28 22:01:12 2020-06-28 22:0…               0
#>  9 B_DL_val … Mustel…     1 2020-06-19 22:31:51 2020-06-19 22:3…               0
#> 10 B_DL_val … Mustel…     1 2020-06-23 23:33:53 2020-06-23 23:3…          349322
#> 11 B_DL_val … Mustel…     1 2020-06-28 23:33:16 2020-06-28 23:3…          431963
#> 12 B_HS_val … Rattus…     1 2020-05-31 20:06:43 2020-05-31 20:0…               0
#> 13 B_HS_val … Rattus…     1 2020-06-27 01:19:06 2020-06-27 01:1…         2265143
#> 14 B_DL_val … Vulpes…     1 2020-06-26 02:09:25 2020-06-26 02:0…               0
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# Specify column to pass station names
get_record_table(x, stationCol = "locationID")
#> # A tibble: 26 × 16
#>    Station  Species       n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>    <chr>     <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 2df5259b Anas pla…     2 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  2 2df5259b Anas pla…     2 2020-07-30 04:29:31 2020-07-30 04:2…           81763
#>  3 2df5259b Anas pla…     2 2020-07-31 04:43:33 2020-07-31 04:4…           87242
#>  4 2df5259b Anas pla…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  5 2df5259b Anas pla…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  6 2df5259b Anas pla…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  7 e254a13c Anas pla…     1 2020-05-30 02:57:37 2020-05-30 02:5…               0
#>  8 e254a13c Anas pla…     2 2020-05-31 04:05:10 2020-05-31 04:0…           90453
#>  9 e254a13c Anas pla…     1 2020-06-06 04:11:07 2020-06-06 04:1…          518757
#> 10 e254a13c Anas pla…     4 2020-06-09 03:16:11 2020-06-09 03:1…          255904
#> # ℹ 16 more rows
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# Include "duplicates", i.e. records of same species at same time, but
# different attributes, such as life stage or sex.
get_record_table(
 x,
 removeDuplicateRecords = FALSE
)
#> # A tibble: 29 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas p…     2 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  2 B_DL_val … Anas p…     2 2020-07-30 04:29:31 2020-07-30 04:2…           81763
#>  3 B_DL_val … Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…           87242
#>  4 B_DL_val … Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  5 B_DL_val … Anas p…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  6 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  7 B_HS_val … Anas p…     1 2020-05-30 02:57:37 2020-05-30 02:5…               0
#>  8 B_HS_val … Anas p…     2 2020-05-31 04:05:10 2020-05-31 04:0…           90453
#>  9 B_HS_val … Anas p…     1 2020-06-06 04:11:07 2020-06-06 04:1…          518757
#> 10 B_HS_val … Anas p…     9 2020-06-06 04:11:07 2020-06-06 04:1…               0
#> # ℹ 19 more rows
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>
```
