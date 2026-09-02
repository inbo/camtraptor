# Summarize deployments information

Summarizes deployments information, more specifically the duration
effort.

## Usage

``` r
summarize_deployments(
  x,
  group_by = c("deploymentID", "latitude", "longitude"),
  group_time_by = NULL
)

summarise_deployments(
  x,
  group_by = c("deploymentID", "latitude", "longitude"),
  group_time_by = NULL
)
```

## Arguments

- x:

  Camera trap data package object, as returned by
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

- group_by:

  Character vector with the names of the columns in deployments. At the
  moment you can choose one or many columns among:
  `c("deploymentID", "latitude", "longitude", "locationID", "locationName", "deploymentStart", "deploymentEnd", "deploymentTags")`.
  Default: `c("deploymentID", "latitude", "longitude")`.

- group_time_by:

  Character, one of `"day"`, `"week"`, `"month"`, `"year"`. The effort
  is calculated at the interval rate defined in `group_time_by`.
  Default: `NULL`, no grouping, i.e. the entire duration of the
  deployment is taken into account as a whole.

## Value

A grouped tibble data frame with the following columns:

- `group_by` names, e.g. `deploymentID`, `latitude`, `longitude` and
  `locationName`.

- `group_time_by` name if provided, e.g. `month`. It contains the first
  date of the time interval, e.g. the first day of the month.

- `effort_duration`: A duration object (duration is a class from
  lubridate package). Duration is always recorded as a fixed number of
  seconds. See
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html).

## Details

`summarize_deployments()` and `summarise_deployments()` are synonyms.

## See also

Other exploration functions:
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md)

## Examples

``` r
x <- example_dataset()

# Return effort using default `group_by` and no time grouping
summarize_deployments(x)
#> # A tibble: 4 × 4
#> # Groups:   deploymentID, latitude, longitude [4]
#>   deploymentID latitude longitude effort_duration       
#>   <chr>           <dbl>     <dbl> <Duration>            
#> 1 00a2c20d         51.5      4.77 2789044s (~4.61 weeks)
#> 2 29b7d356         51.2      5.66 859859s (~1.42 weeks) 
#> 3 577b543a         51.2      5.66 786802s (~1.3 weeks)  
#> 4 62c200a9         50.7      4.01 1903602s (~3.15 weeks)

# Return effort using default `group_by` and grouping by year
summarize_deployments(x, group_time_by = "year")
#> # A tibble: 4 × 5
#> # Groups:   deploymentID, latitude, longitude, year [4]
#>   deploymentID latitude longitude year                effort_duration       
#>   <chr>           <dbl>     <dbl> <dttm>              <Duration>            
#> 1 00a2c20d         51.5      4.77 2020-01-01 00:00:00 2789044s (~4.61 weeks)
#> 2 29b7d356         51.2      5.66 2020-01-01 00:00:00 859859s (~1.42 weeks) 
#> 3 577b543a         51.2      5.66 2020-01-01 00:00:00 786802s (~1.3 weeks)  
#> 4 62c200a9         50.7      4.01 2021-01-01 00:00:00 1903602s (~3.15 weeks)

# Return effort specifying grouping columns, e.g. `deploymentID` and
# `locationName` and grouping by day
summarize_deployments(
  x,
  group_by = c("deploymentID", "locationName"),
  group_time_by = "day"
)
#> # A tibble: 77 × 4
#> # Groups:   deploymentID, locationName, day [77]
#>    deploymentID locationName           day                 effort_duration      
#>    <chr>        <chr>                  <dttm>              <Duration>           
#>  1 00a2c20d     B_HS_val 2_processiep… 2020-05-30 00:00:00 75743s (~21.04 hours)
#>  2 00a2c20d     B_HS_val 2_processiep… 2020-05-31 00:00:00 86400s (~1 days)     
#>  3 00a2c20d     B_HS_val 2_processiep… 2020-06-01 00:00:00 86400s (~1 days)     
#>  4 00a2c20d     B_HS_val 2_processiep… 2020-06-02 00:00:00 86400s (~1 days)     
#>  5 00a2c20d     B_HS_val 2_processiep… 2020-06-03 00:00:00 86400s (~1 days)     
#>  6 00a2c20d     B_HS_val 2_processiep… 2020-06-04 00:00:00 86400s (~1 days)     
#>  7 00a2c20d     B_HS_val 2_processiep… 2020-06-05 00:00:00 86400s (~1 days)     
#>  8 00a2c20d     B_HS_val 2_processiep… 2020-06-06 00:00:00 86400s (~1 days)     
#>  9 00a2c20d     B_HS_val 2_processiep… 2020-06-07 00:00:00 86400s (~1 days)     
#> 10 00a2c20d     B_HS_val 2_processiep… 2020-06-08 00:00:00 86400s (~1 days)     
#> # ℹ 67 more rows

# Afterwards, you can calculate the total effort over all deployments. You 
# can also show other information, e.g. the (number of) deployments and
# locations.
library(dplyr)
summarize_deployments(
  x,
  group_by = c("deploymentID", "locationName"),
  group_time_by = "month"
) %>%
  group_by(month) %>%
  summarise(
    deploymentIDs = list(deploymentID),
    ndep = length(unique(deploymentID)),
    nloc = length(unique(locationName)),
    effort_duration = sum(effort_duration)
  )
#> # A tibble: 6 × 5
#>   month               deploymentIDs  ndep  nloc effort_duration
#>   <dttm>              <list>        <int> <int>           <dbl>
#> 1 2020-05-01 00:00:00 <chr [1]>         1     1          162143
#> 2 2020-06-01 00:00:00 <chr [2]>         2     2         3378802
#> 3 2020-07-01 00:00:00 <chr [2]>         2     2          274320
#> 4 2020-08-01 00:00:00 <chr [1]>         1     1          620440
#> 5 2021-03-01 00:00:00 <chr [1]>         1     1          357702
#> 6 2021-04-01 00:00:00 <chr [1]>         1     1         1545900
```
