# Record table

This functionality has been superseded because camtrapR supports reading
Camera Trap Data Packages, see the function
[`camtrapR::readCamtrapDP()`](https://jniedballa.github.io/camtrapR/reference/readCamtrapDP.html).

This vignette shows how to get a **species record table** from a Camera
Trap Data Package dataset, equivalent to the record table returned by
camtrapR’s function
[recordTable](https://jniedballa.github.io/camtrapR/reference/recordTable.html).

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
```

For this example the function
[`example_dataset()`](https://inbo.github.io/camtraptor/reference/example_dataset.md)
is used to load an example Camera Trap Data Package dataset. The dataset
is derived from a study on detecting invasive muskrat and coypu
populations using camera traps.

``` r

x <- example_dataset()
```

## Species record table

The camtrapR function `recordTable()` generates a record table from
camera trap images or videos. In a Camera Trap Data Package, `media`
(e.g. images) are already aggregated into `observations` to a certain
extent. If all observations are considered independent, the record table
can be generated simply with:

``` r

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
```

The function returns the same columns as the camtrapR’s function
`recordTable()` except for column `n`. The following mapping is applied:

| column name output | description |
|----|----|
| `Station` | the station name as provided by argument `stationCol` (default: `locationName`). It has to be a column of `deployments` |
| `Species` | the `scientificName` column in `observations` |
| `n` | the `count` column in `observations` (number of observed individuals) |
| `DateTimeOriginal` | the `eventStart` column in `observations` |
| `Date` | the date from `eventStart` |
| `Time` | the time part from `eventStart` |
| `delta.time.secs` | the elapsed time in seconds between two (independent) observations |
| `delta.time.mins` | the elapsed time in minutes between two (independent) observations |
| `delta.time.hours` | the elapsed time in hours between two (independent) observations |
| `delta.time.days` | the elapsed time in days between two (independent) observations |
| `Directory` | a list with file paths as stored in column `filePath` of `media` |
| `FileName` | a list with file names as stored in column `fileName` of `media` |
| `Latitude` | the latitude of the station |
| `Longitude` | the longitude of the station |
| `clock` | the clock time in radians |
| `solar` | the solar time in radians |

The following remarks are both valid for camtrapR’s function
`recordTable()` and the function
[`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)
of this package:

1.  observations are grouped by station and species
2.  observations of unidentified animals are removed
3.  the elapsed time of the first observation (record) of a species at a
    certain station is set to 0 by default

### Temporal independence

As described in [Chapter
3](https://jniedballa.github.io/camtrapR/articles/camtrapr3.html) of
camtrapR documentation, observations can be filtered for temporal
independence by setting a minimum time difference between subsequent
records of the same species. As for `recordTable()`, this is achieved
via argument `minDeltaTime`, defined as the minimum time difference (in
minutes) between two records of the same species at the same station
which are to be considered independent. The default is 0, causing the
function to return all records.

Again, as for `recordTable()`, an argument is provided,
`deltaTimeComparedTo`, to further control how independence between
records is assessed.

For each event there are generally multiple media files, as many camera
traps use bursts. Setting `deltaTimeComparedTo` to
`"lastIndependentRecord"` returns only records taken `minDeltaTime`
minutes after the first photo of the last independent record (i.e.,
`eventStart`). Setting `deltaTimeComparedTo` to `"lastRecord"` returns
only records taken `minDeltaTime` minutes after the last photo of the
last independent record (i.e., the last `timestamp` in the event’s
media).

Let’s first modify the temporal data of the Camera Trap Data Package
dataset to create a dependent observation, solely for demonstration
purposes.

``` r

obs <- observations(x)
obs[obs$eventID == "02ae9f43", "eventStart"] <- as_datetime(
  "2020-08-02 05:10:20"
)
 
med <- media(x) 
rows_to_update <- which(med$eventID == "02ae9f43") 
med[rows_to_update, "timestamp"] <- as_datetime("2020-08-02 05:10:20") 

x_modified <- x
observations(x_modified) <- obs
media(x_modified) <- med
```

Example with `minDeltaTime = 10` and
`deltaTimeComparedTo = "lastIndependentRecord"`:

``` r

get_record_table(
  x_modified,
  minDeltaTime = 10,
  deltaTimeComparedTo = "lastIndependentRecord"
  )
#> # A tibble: 26 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas p…     2 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  2 B_DL_val … Anas p…     2 2020-07-30 04:29:31 2020-07-30 04:2…           81763
#>  3 B_DL_val … Anas p…     2 2020-07-31 04:43:33 2020-07-31 04:4…           87242
#>  4 B_DL_val … Anas p…     5 2020-08-02 05:00:14 2020-08-02 05:0…          173801
#>  5 B_DL_val … Anas p…     3 2020-08-02 05:10:20 2020-08-02 05:1…             606
#>  6 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…          172429
#>  7 B_HS_val … Anas p…     1 2020-05-30 02:57:37 2020-05-30 02:5…               0
#>  8 B_HS_val … Anas p…     2 2020-05-31 04:05:10 2020-05-31 04:0…           90453
#>  9 B_HS_val … Anas p…     1 2020-06-06 04:11:07 2020-06-06 04:1…          518757
#> 10 B_HS_val … Anas p…     4 2020-06-09 03:16:11 2020-06-09 03:1…          255904
#> # ℹ 16 more rows
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>
```

Example with `minDeltaTime = 10` and
`deltaTimeComparedTo = "lastRecord"`:

``` r

get_record_table(
  x_modified,
  minDeltaTime = 10,
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
#>  5 B_DL_val … Anas p…     3 2020-08-04 05:04:09 2020-08-04 05:0…          173035
#>  6 B_HS_val … Anas p…     1 2020-05-30 02:57:37 2020-05-30 02:5…               0
#>  7 B_HS_val … Anas p…     2 2020-05-31 04:05:10 2020-05-31 04:0…           90453
#>  8 B_HS_val … Anas p…     1 2020-06-06 04:11:07 2020-06-06 04:1…          518757
#>  9 B_HS_val … Anas p…     4 2020-06-09 03:16:11 2020-06-09 03:1…          255904
#> 10 B_HS_val … Anas p…     1 2020-06-12 04:04:29 2020-06-12 04:0…          262098
#> # ℹ 15 more rows
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>
```

Running the code above with `deltaTimeComparedTo = "lastRecord"`
produces the message
`Number of not independent observations to be removed: 1`, which does
not appear when using `deltaTimeComparedTo = "lastIndependentRecord"`.
The two settings thus produce different record tables, compare row 5 in
particular.

This difference arises because event `45ee3031` spans a 51-second burst.
`lastRecord` counts from the last photo of that burst (05:01:05),
whereas `lastIndependentRecord` counts from the first photo (05:00:14).
The next Anas platyrhynchos event (05:10:20) falls between the two
thresholds: it is independent under lastIndependentRecord (threshold:
05:10:14) but dependent under lastRecord (threshold: 05:11:05).

### Exclude some species

Similar to `recordTable()`, the function
[`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)
allows you also to exclude species. Only scientific names are allowed:

``` r

get_record_table(x, exclude = c("Anas platyrhynchos", "Vulpes vulpes"))
#> # A tibble: 13 × 16
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
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>
```

### Station names

The column containing the station names can also be defined by the user
if the default value, `"locationName"`, is not the correct one. It has
to be a valid column of `deployments`. Below, `locationID` is used:

``` r

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
```

### Remove duplicates

Sometimes multiple observations of the same species are recorded at the
same time and location, but differ in attributes such as `lifeStage` or
`sex`, for example, when adult females and juveniles are photographed
together. The `removeDuplicateRecords` argument controls whether such
duplicates are retained: by default it is `TRUE`, keeping only one
observation per group. Set it to `FALSE` to retain all observations.

To illustrate the difference, compare the two record tables below.

Without duplicates (default):

``` r

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
```

With duplicates:

``` r

get_record_table(x, removeDuplicateRecords = FALSE)
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

Row 10 differs between the two outputs (among others). When
`removeDuplicateRecords = FALSE`, there are two records with
`DateTimeOriginal = 2020-06-06 04:11:07` one on row 9 and one on row 10.

### Other arguments needed?

Are there other arguments of camtrapR’s function `recordTable()` you
think should be relevant to add to
[`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md),
please let us know by posting an
[issue](https://github.com/inbo/camtraptor/issues)!
