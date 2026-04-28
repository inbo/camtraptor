# Get record table

Calculates the record table from a camera trap data package and so
tabulating species records. The record table is a concept developed
within the camtrapR package, see [this
article](https://jniedballa.github.io/camtrapR/articles/camtrapr3.html).
See also the function documentation for
[camtrapR::recordTable()](https://jniedballa.github.io/camtrapR/reference/recordTable.html).
**Note**: All dates and times are expressed in UTC format.

## Usage

``` r
get_record_table(
  package = NULL,
  ...,
  stationCol = "locationName",
  exclude = NULL,
  minDeltaTime = 0,
  deltaTimeComparedTo = NULL,
  removeDuplicateRecords = TRUE,
  datapkg = lifecycle::deprecated()
)
```

## Arguments

- package:

  Camera trap data package object, as returned by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- ...:

  Filter predicates for filtering on deployments

- stationCol:

  Character name of the column containing stations. Default:
  `"locationName"`.

- exclude:

  Character vector of species names (scientific names or vernacular
  names) to be excluded from the record table. Default: `NULL`.

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

  Logical. If there are several records of the same species at the same
  station at exactly the same time, show only one?

- datapkg:

  Deprecated. Use `package` instead.

## Value

A tibble data frame containing species records and additional
information about stations, date, time and further metadata, such as
filenames and directories of the images (media) linked to the species
records. Some more details about the columns returned:

- `Station`: Character, station names, as found in the deployment column
  defined in parameter `stationCol`.

- `Species`: Character, the scientific name of the observed species.

- `n`: Numeric, the number of individuals of the observed species.

- `DateTimeOriginal`: Datetime object, as found in column `timestamp` of
  `observations`, in UTC format.

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
  which essentially uses the approach described in [Nouvellet et al.
  (2012)](https://doi.org/10.1111/j.1469-7998.2011.00864.x).

## See also

Other camtrapR-derived functions:
[`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md),
[`get_detection_history()`](https://inbo.github.io/camtraptor/reference/get_detection_history.md)

## Examples

``` r
get_record_table(mica)
#> # A tibble: 17 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…               0
#>  2 B_DL_val … Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  3 B_DL_val … Anas p…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  4 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  5 B_DL_val … Anas s…     4 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  6 B_DL_val … Anas s…     1 2020-07-30 04:29:31 2020-07-30 04:2…           81763
#>  7 B_DL_val … Anas s…     1 2020-08-05 05:02:01 2020-08-05 05:0…          520350
#>  8 B_DM_val … Ardea       1 2021-04-05 19:08:33 2021-04-05 19:0…               0
#>  9 B_DM_val … Ardea       1 2021-04-11 19:43:09 2021-04-11 19:4…          520476
#> 10 B_DM_val … Ardea …     1 2021-03-27 20:38:18 2021-03-27 20:3…               0
#> 11 B_DL_val … Castor…     1 2020-06-19 22:05:55 2020-06-19 22:0…               0
#> 12 Mica Viane Homo s…     2 2019-10-23 10:19:44 2019-10-23 10:1…               0
#> 13 B_DL_val … Martes…     1 2020-06-28 22:01:12 2020-06-28 22:0…               0
#> 14 B_DL_val … Mustel…     1 2020-06-19 22:31:51 2020-06-19 22:3…               0
#> 15 B_DL_val … Mustel…     1 2020-06-23 23:33:53 2020-06-23 23:3…          349322
#> 16 B_DL_val … Mustel…     1 2020-06-28 23:33:16 2020-06-28 23:3…          431963
#> 17 B_DL_val … Vulpes…     1 2020-06-26 02:09:25 2020-06-26 02:0…               0
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# Set a minDeltaTime of 20 minutes from last independent record for filtering
# out not independent observations
mica_dependent <- mica
mica_dependent$data$observations[4,"timestamp"] <- lubridate::as_datetime("2020-07-29 05:55:00")
get_record_table(
  mica_dependent,
  minDeltaTime = 20,
  deltaTimeComparedTo = "lastIndependentRecord"
)
#> Number of not independent observations to be removed: 1
#> # A tibble: 16 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…               0
#>  2 B_DL_val … Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  3 B_DL_val … Anas p…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  4 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  5 B_DL_val … Anas s…     4 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  6 B_DL_val … Anas s…     1 2020-08-05 05:02:01 2020-08-05 05:0…          602113
#>  7 B_DM_val … Ardea       1 2021-04-05 19:08:33 2021-04-05 19:0…               0
#>  8 B_DM_val … Ardea       1 2021-04-11 19:43:09 2021-04-11 19:4…          520476
#>  9 B_DM_val … Ardea …     1 2021-03-27 20:38:18 2021-03-27 20:3…               0
#> 10 B_DL_val … Castor…     1 2020-06-19 22:05:55 2020-06-19 22:0…               0
#> 11 Mica Viane Homo s…     2 2019-10-23 10:19:44 2019-10-23 10:1…               0
#> 12 B_DL_val … Martes…     1 2020-06-28 22:01:12 2020-06-28 22:0…               0
#> 13 B_DL_val … Mustel…     1 2020-06-19 22:31:51 2020-06-19 22:3…               0
#> 14 B_DL_val … Mustel…     1 2020-06-23 23:33:53 2020-06-23 23:3…          349322
#> 15 B_DL_val … Mustel…     1 2020-06-28 23:33:16 2020-06-28 23:3…          431963
#> 16 B_DL_val … Vulpes…     1 2020-06-26 02:09:25 2020-06-26 02:0…               0
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# Set a minDeltaTime of 20 minutes from last record for filtering out not
# independent observations
get_record_table(
  mica_dependent,
  minDeltaTime = 20,
  deltaTimeComparedTo = "lastRecord"
)
#> Number of not independent observations to be removed: 1
#> # A tibble: 16 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…               0
#>  2 B_DL_val … Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  3 B_DL_val … Anas p…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  4 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  5 B_DL_val … Anas s…     4 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  6 B_DL_val … Anas s…     1 2020-08-05 05:02:01 2020-08-05 05:0…          602113
#>  7 B_DM_val … Ardea       1 2021-04-05 19:08:33 2021-04-05 19:0…               0
#>  8 B_DM_val … Ardea       1 2021-04-11 19:43:09 2021-04-11 19:4…          520476
#>  9 B_DM_val … Ardea …     1 2021-03-27 20:38:18 2021-03-27 20:3…               0
#> 10 B_DL_val … Castor…     1 2020-06-19 22:05:55 2020-06-19 22:0…               0
#> 11 Mica Viane Homo s…     2 2019-10-23 10:19:44 2019-10-23 10:1…               0
#> 12 B_DL_val … Martes…     1 2020-06-28 22:01:12 2020-06-28 22:0…               0
#> 13 B_DL_val … Mustel…     1 2020-06-19 22:31:51 2020-06-19 22:3…               0
#> 14 B_DL_val … Mustel…     1 2020-06-23 23:33:53 2020-06-23 23:3…          349322
#> 15 B_DL_val … Mustel…     1 2020-06-28 23:33:16 2020-06-28 23:3…          431963
#> 16 B_DL_val … Vulpes…     1 2020-06-26 02:09:25 2020-06-26 02:0…               0
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# Exclude observations of mallard
# Exclude is case insensitive and vernacular names are allowed
get_record_table(mica, exclude = "wilde eend")
#> Scientific name of wilde eend: Anas platyrhynchos
#> # A tibble: 13 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas s…     4 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  2 B_DL_val … Anas s…     1 2020-07-30 04:29:31 2020-07-30 04:2…           81763
#>  3 B_DL_val … Anas s…     1 2020-08-05 05:02:01 2020-08-05 05:0…          520350
#>  4 B_DM_val … Ardea       1 2021-04-05 19:08:33 2021-04-05 19:0…               0
#>  5 B_DM_val … Ardea       1 2021-04-11 19:43:09 2021-04-11 19:4…          520476
#>  6 B_DM_val … Ardea …     1 2021-03-27 20:38:18 2021-03-27 20:3…               0
#>  7 B_DL_val … Castor…     1 2020-06-19 22:05:55 2020-06-19 22:0…               0
#>  8 Mica Viane Homo s…     2 2019-10-23 10:19:44 2019-10-23 10:1…               0
#>  9 B_DL_val … Martes…     1 2020-06-28 22:01:12 2020-06-28 22:0…               0
#> 10 B_DL_val … Mustel…     1 2020-06-19 22:31:51 2020-06-19 22:3…               0
#> 11 B_DL_val … Mustel…     1 2020-06-23 23:33:53 2020-06-23 23:3…          349322
#> 12 B_DL_val … Mustel…     1 2020-06-28 23:33:16 2020-06-28 23:3…          431963
#> 13 B_DL_val … Vulpes…     1 2020-06-26 02:09:25 2020-06-26 02:0…               0
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# Specify column to pass station names
get_record_table(
  mica,
  stationCol = "locationID",
  minDeltaTime = 20,
  deltaTimeComparedTo = "lastRecord"
)
#> # A tibble: 17 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 2df5259b-… Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…               0
#>  2 2df5259b-… Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  3 2df5259b-… Anas p…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  4 2df5259b-… Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  5 2df5259b-… Anas s…     4 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  6 2df5259b-… Anas s…     1 2020-07-30 04:29:31 2020-07-30 04:2…           81763
#>  7 2df5259b-… Anas s…     1 2020-08-05 05:02:01 2020-08-05 05:0…          520350
#>  8 ce943ced-… Ardea       1 2021-04-05 19:08:33 2021-04-05 19:0…               0
#>  9 ce943ced-… Ardea       1 2021-04-11 19:43:09 2021-04-11 19:4…          520476
#> 10 ce943ced-… Ardea …     1 2021-03-27 20:38:18 2021-03-27 20:3…               0
#> 11 ff1535c0-… Castor…     1 2020-06-19 22:05:55 2020-06-19 22:0…               0
#> 12 3232bcfd-… Homo s…     2 2019-10-23 10:19:44 2019-10-23 10:1…               0
#> 13 ff1535c0-… Martes…     1 2020-06-28 22:01:12 2020-06-28 22:0…               0
#> 14 ff1535c0-… Mustel…     1 2020-06-19 22:31:51 2020-06-19 22:3…               0
#> 15 ff1535c0-… Mustel…     1 2020-06-23 23:33:53 2020-06-23 23:3…          349322
#> 16 ff1535c0-… Mustel…     1 2020-06-28 23:33:16 2020-06-28 23:3…          431963
#> 17 ff1535c0-… Vulpes…     1 2020-06-26 02:09:25 2020-06-26 02:0…               0
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# How to deal with duplicates
mica_dup <- mica
# create a duplicate at 2020-07-29 05:46:48, location: B_DL_val 5_beek kleine vijver
mica_dup$data$observations[4,"sequenceID"] <- mica_dup$data$observations$sequenceID[3]
mica_dup$data$observations[4, "deploymentID"] <- mica_dup$data$observations$deploymentID[3]
mica_dup$data$observations[4, "timestamp"] <- mica_dup$data$observations$timestamp[3]

# duplicates are removed by default by get_record_table()
get_record_table(mica_dup)
#> # A tibble: 16 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…               0
#>  2 B_DL_val … Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  3 B_DL_val … Anas p…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  4 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  5 B_DL_val … Anas s…     1 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  6 B_DL_val … Anas s…     1 2020-08-05 05:02:01 2020-08-05 05:0…          602113
#>  7 B_DM_val … Ardea       1 2021-04-05 19:08:33 2021-04-05 19:0…               0
#>  8 B_DM_val … Ardea       1 2021-04-11 19:43:09 2021-04-11 19:4…          520476
#>  9 B_DM_val … Ardea …     1 2021-03-27 20:38:18 2021-03-27 20:3…               0
#> 10 B_DL_val … Castor…     1 2020-06-19 22:05:55 2020-06-19 22:0…               0
#> 11 Mica Viane Homo s…     2 2019-10-23 10:19:44 2019-10-23 10:1…               0
#> 12 B_DL_val … Martes…     1 2020-06-28 22:01:12 2020-06-28 22:0…               0
#> 13 B_DL_val … Mustel…     1 2020-06-19 22:31:51 2020-06-19 22:3…               0
#> 14 B_DL_val … Mustel…     1 2020-06-23 23:33:53 2020-06-23 23:3…          349322
#> 15 B_DL_val … Mustel…     1 2020-06-28 23:33:16 2020-06-28 23:3…          431963
#> 16 B_DL_val … Vulpes…     1 2020-06-26 02:09:25 2020-06-26 02:0…               0
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# duplicate not removed
get_record_table(mica_dup, removeDuplicateRecords = FALSE)
#> # A tibble: 17 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…               0
#>  2 B_DL_val … Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  3 B_DL_val … Anas p…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  4 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  5 B_DL_val … Anas s…     4 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  6 B_DL_val … Anas s…     1 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  7 B_DL_val … Anas s…     1 2020-08-05 05:02:01 2020-08-05 05:0…          602113
#>  8 B_DM_val … Ardea       1 2021-04-05 19:08:33 2021-04-05 19:0…               0
#>  9 B_DM_val … Ardea       1 2021-04-11 19:43:09 2021-04-11 19:4…          520476
#> 10 B_DM_val … Ardea …     1 2021-03-27 20:38:18 2021-03-27 20:3…               0
#> 11 B_DL_val … Castor…     1 2020-06-19 22:05:55 2020-06-19 22:0…               0
#> 12 Mica Viane Homo s…     2 2019-10-23 10:19:44 2019-10-23 10:1…               0
#> 13 B_DL_val … Martes…     1 2020-06-28 22:01:12 2020-06-28 22:0…               0
#> 14 B_DL_val … Mustel…     1 2020-06-19 22:31:51 2020-06-19 22:3…               0
#> 15 B_DL_val … Mustel…     1 2020-06-23 23:33:53 2020-06-23 23:3…          349322
#> 16 B_DL_val … Mustel…     1 2020-06-28 23:33:16 2020-06-28 23:3…          431963
#> 17 B_DL_val … Vulpes…     1 2020-06-26 02:09:25 2020-06-26 02:0…               0
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>

# Applying filter(s) on deployments, e.g. deployments with latitude >= 51.18
get_record_table(mica, pred_gte("latitude", 51.18))
#> df %>% dplyr::filter((latitude >= 51.18))
#> # A tibble: 13 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…               0
#>  2 B_DL_val … Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  3 B_DL_val … Anas p…     3 2020-08-03 05:09:12 2020-08-03 05:0…           86938
#>  4 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…           86097
#>  5 B_DL_val … Anas s…     4 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  6 B_DL_val … Anas s…     1 2020-07-30 04:29:31 2020-07-30 04:2…           81763
#>  7 B_DL_val … Anas s…     1 2020-08-05 05:02:01 2020-08-05 05:0…          520350
#>  8 B_DL_val … Castor…     1 2020-06-19 22:05:55 2020-06-19 22:0…               0
#>  9 B_DL_val … Martes…     1 2020-06-28 22:01:12 2020-06-28 22:0…               0
#> 10 B_DL_val … Mustel…     1 2020-06-19 22:31:51 2020-06-19 22:3…               0
#> 11 B_DL_val … Mustel…     1 2020-06-23 23:33:53 2020-06-23 23:3…          349322
#> 12 B_DL_val … Mustel…     1 2020-06-28 23:33:16 2020-06-28 23:3…          431963
#> 13 B_DL_val … Vulpes…     1 2020-06-26 02:09:25 2020-06-26 02:0…               0
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>
```
