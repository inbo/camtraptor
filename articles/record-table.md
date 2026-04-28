# Record table

This vignette shows how to get a *species record table* as returned by
camtrapR’s function
[recordTable](https://jniedballa.github.io/camtrapR/reference/recordTable.html)
starting from a camera trap data package.

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
`camtraptor` is made available. This data package contains camera trap
data of musk rats and coypus. We will use this variable from now on.

## Species record table

The camtrapR’s function `recordTable()` generates:

> a record table from camera trap images or videos

At a certain extent the aggregation of `media` (e.g. images) into
`observations` is already done in a camera trap data package.

If we consider that all observations are independent, then, it will be
sufficient to run the following:

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
```

The function returns the same columns as the camtrapR’s function
`recordTable()` except for column `n`. The following mapping is applied:

| column name output | description                                                                                                             |
|--------------------|-------------------------------------------------------------------------------------------------------------------------|
| `Station`          | the station name as provided by argument `stationCol` (default: `locationName`). It has to be a column of `deployments` |
| `Species`          | `scientific_name` column in `observations`                                                                              |
| `n`                | `count` column in `observations` (number of individuals)                                                                |
| `DateTimeOriginal` | the `timestamp` column in `observations`                                                                                |
| `Date`             | the date from `timestamp`                                                                                               |
| `Time`             | the time part from `timestamp`                                                                                          |
| `delta.time.secs`  | the elapsed time in seconds between two (independent) observations                                                      |
| `delta.time.mins`  | the elapsed time in minutes between two (independent) observations                                                      |
| `delta.time.hours` | the elapsed time in hours between two (independent) observations                                                        |
| `delta.time.days`  | the elapsed time in days between two (independent) observations                                                         |
| `Directory`        | a list with file paths as stored in column `file_path` of `media`                                                       |
| `FileName`         | a list with file paths as stored in column `file_path` of `media`                                                       |

The following remarks are both valid for camtrapR’s function
`recordTable()` and the function
[`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)
of this package: 1. observations are grouped by station and species 2.
observations of unidentified animals are removed 2. the elapsed time of
the first observation (record) of a species at a certain station is set
to 0 by default

### Temporal independence

As described in [Chapter
3](https://jniedballa.github.io/camtrapR/articles/camtrapr3.html) of
camtrapR documentation, we could filter observations using an adjustable
criterion for temporal independence between subsequent records of the
same species in an attempt to remove non-independent records. As for
`recordTable()`, this is achieved via argument `minDeltaTime`, defined
as the minimum time difference (in minutes) between two records of the
same species at the same station which are to be considered independent.
As shown above, the default is 0, causing the function to return all
records.

Again, as for `recordTable()`, we provide an argument,
`deltaTimeComparedTo`, to further control how independence between
records is assessed. Setting it to `“lastRecord”` returns only records
taken `minDeltaTime` minutes after the last record, i.e. `minDeltaTime`
minutes after `timestamp` of the last recorded media file. Example with
`minDeltaTime = 60` (1 hour):

``` r
mica_dependent <- mica
mica_dependent$data$observations[4,"timestamp"] <- lubridate::as_datetime("2020-07-29 05:55:00")
get_record_table(mica_dependent, 
                 minDeltaTime = 10, 
                 deltaTimeComparedTo = "lastRecord")
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
```

Setting `deltaTimeComparedTo` to `“lastIndependentRecord”` returns only
records taken `minDeltaTime` minutes after the last independent record,
i.e. `minDeltaTime` minutes after `timestamp` of the last observation.
Example with `minDeltaTime = 60` (1 hour):

``` r
get_record_table(mica_dependent, 
                 minDeltaTime = 10, 
                 deltaTimeComparedTo = "lastIndependentRecord")
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
```

### Exclude some species

Similar to `recordTable()`, the function
[`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)
allows you also to exclude some species. Both vernacular names and
scientific names are allowed (case insensitive):

``` r
get_record_table(mica, exclude = c("grey heron", "Anas platyrhynchos", "mens"))
#> Scientific name of grey heron: Ardea cinerea
#> Scientific name of mens: Homo sapiens
#> # A tibble: 11 × 16
#>    Station    Species     n DateTimeOriginal    Date       Time  delta.time.secs
#>    <chr>      <chr>   <dbl> <dttm>              <date>     <chr>           <dbl>
#>  1 B_DL_val … Anas s…     4 2020-07-29 05:46:48 2020-07-29 05:4…               0
#>  2 B_DL_val … Anas s…     1 2020-07-30 04:29:31 2020-07-30 04:2…           81763
#>  3 B_DL_val … Anas s…     1 2020-08-05 05:02:01 2020-08-05 05:0…          520350
#>  4 B_DM_val … Ardea       1 2021-04-05 19:08:33 2021-04-05 19:0…               0
#>  5 B_DM_val … Ardea       1 2021-04-11 19:43:09 2021-04-11 19:4…          520476
#>  6 B_DL_val … Castor…     1 2020-06-19 22:05:55 2020-06-19 22:0…               0
#>  7 B_DL_val … Martes…     1 2020-06-28 22:01:12 2020-06-28 22:0…               0
#>  8 B_DL_val … Mustel…     1 2020-06-19 22:31:51 2020-06-19 22:3…               0
#>  9 B_DL_val … Mustel…     1 2020-06-23 23:33:53 2020-06-23 23:3…          349322
#> 10 B_DL_val … Mustel…     1 2020-06-28 23:33:16 2020-06-28 23:3…          431963
#> 11 B_DL_val … Vulpes…     1 2020-06-26 02:09:25 2020-06-26 02:0…               0
#> # ℹ 9 more variables: delta.time.mins <dbl>, delta.time.hours <dbl>,
#> #   delta.time.days <dbl>, Directory <list>, FileName <list>, latitude <dbl>,
#> #   longitude <dbl>, clock <dbl>, solar <dbl>
```

### Station names

The column containing the station names can also be defined by the user
if the default value, `"locationName"`, is not the correct one. It has
to be a valid column of `deployments`. Here below, `locationID` is used:

``` r
get_record_table(mica, stationCol = "locationID")
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
```

### Remove duplicates

It can happen that “duplicates” occur, e.g. when two distinct
observations of the same species are made based on the same sequence of
images, e.g. same species but different `lifeStage` or `sex`. You can
decide what to do with these duplicates by using the argument
`removeDuplicateRecords`: by default it is equal to `TRUE`. The
duplicates are therefore removed. To not remove them, set
`removeDuplicateRecords` equal to `FALSE`.

Let’s create an easy example with a duplicate based on `mica`
datapackage:

``` r
mica_dup <- mica
# create a duplicate at 2020-07-29 05:46:48, location: B_DL_val 5_beek kleine vijver
mica_dup$data$observations[4,"sequenceID"] <- mica_dup$data$observations$sequenceID[3]
mica_dup$data$observations[4, "deploymentID"] <- mica_dup$data$observations$deploymentID[3]
mica_dup$data$observations[4, "timestamp"] <- mica_dup$data$observations$timestamp[3]
```

Record table without duplicates:

``` r
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
```

Record table with duplicates:

``` r
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
```

### Use filter predicates

As for visualization and all other functions, you can select a subset of
deployments by using filter predicates. E.g. to get the record table of
observations for the deployments with latitude equal or higher than
51.18:

``` r
get_record_table(mica, pred_gt("latitude", 51.18))
#> df %>% dplyr::filter((latitude > 51.18))
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

### Other arguments needed?

Are there other arguments of camtrapR’s function `recordTable()` you
think should be relevant to add to `get_camera_record()`, please let us
know by posting an [issue](https://github.com/inbo/camtraptor/issues)!
