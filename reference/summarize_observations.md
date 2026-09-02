# Summarize observations information

Summarizes event-based observations by calculating:

- Number of scientific names.

- Number of events.

- Number of observations.

- Sum of individual counts.

- Relative Abundance Index (RAI) based on number of observations.

- Relative Abundance Index (RAI) based on individual counts.

## Usage

``` r
summarize_observations(
  x,
  group_by = c("deploymentID", "latitude", "longitude", "scientificName"),
  group_time_by = NULL,
  extend = FALSE
)

summarise_observations(
  x,
  group_by = c("deploymentID", "latitude", "longitude", "scientificName"),
  group_time_by = NULL,
  extend = FALSE
)
```

## Arguments

- x:

  Camera trap data package object, as returned by
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

- group_by:

  Character vector with names of columns in deployments and
  observations. At the moment you can choose one or many columns among:
  `c("deploymentID", "latitude", "longitude", "locationID", "locationName", "deploymentStart", "deploymentEnd", "deploymentTags", "scientificName", "lifeStage", "sex", "behavior")`.
  Default:
  `c("deploymentID", "latitude", "longitude", "scientificName")`.

- group_time_by:

  Character, one of `"day"`, `"week"`, `"month"`, `"year"`. The effort
  is calculated at the interval rate defined in `group_time_by`.
  Default: `NULL`, no grouping, i.e. the entire duration of the
  deployment is taken into account as a whole.

- extend:

  Logical. If `TRUE`, the summary is extended with all possible groups
  left out by `summarize_observations()`. See details section for more
  information. Default: `FALSE`.

## Value

A grouped tibble data frame with the following columns:

- `group_by` names, e.g. `deploymentID`, `latitude`, `longitude`, and
  `scientificName`.

- `group_time_by` name if provided, e.g. `month`. It is a datetime
  column containing the first date of the time interval, e.g. the first
  day of the month.

- `n_scientificName`: integer vector with the number of scientific
  names. If `scientificName` is in `group_by`, `n_scientificName` is
  equal to 1 or 0, if `scientificName = NA` (unidentified animals).

- `n_events`: integer vector with the number of events.

- `n_observations`: integer vector with the number of observations.

- `sum_count`: integer vector with the sum of individual counts.

- `rai_observations`: numeric vector with the Relative Abundance Index
  (RAI), defined as `100 * (n_observations/effort)` where
  `n_observations` is the number of observations and `effort` is the
  `effort_duration` as returned by
  [`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md)
  expressed in days.

- `rai_count`: numeric vector with the Relative Abundance Index (RAI),
  defined as `100 * (sum_count/effort)` where `sum_count` is the sum of
  individual counts and `effort` is the `effort_duration` as returned by
  [`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md)
  expressed in days.

## Details

`summarize_observations()` and `summarise_observations()` are synonyms.

By default (`extend = FALSE`), the function follows the standard
behavior of
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
returning only groups that have observations. This means deployments or
time periods with zero observations for the specified grouping are
excluded from the output.

When `extend = TRUE`, the summary is extended to include all possible
combinations of grouping variables, even when no observations exist for
a particular group. This is particularly useful for visualisations
([`map_summary()`](https://inbo.github.io/camtraptor/reference/map_summary.md))
and analysis as it identifies for example:

- Deployments where a specific species was not observed.

- Time periods when a specific species was not observed.

- Presence/absence patterns across deployments.

For extended summaries, feature values are set to `0` for groups with no
observations, except for `n_scientificName` which is set to `NA` when no
species are present as `0` is used when only unidentified individuals
are observed.

## See also

Other exploration functions:
[`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md)

## Examples

``` r
x <- example_dataset()
# Summarize observations by `deploymentID`, `latitude`, `longitude` and
# `scientificName` (default)
summarize_observations(x)
#> # A tibble: 15 × 10
#> # Groups:   deploymentID, latitude, longitude, scientificName [15]
#>    deploymentID latitude longitude scientificName     n_scientificName n_events
#>    <chr>           <dbl>     <dbl> <chr>                         <int>    <int>
#>  1 00a2c20d         51.5      4.77 Anas platyrhynchos                1        6
#>  2 00a2c20d         51.5      4.77 Ardea cinerea                     1        1
#>  3 00a2c20d         51.5      4.77 Rattus norvegicus                 1        2
#>  4 00a2c20d         51.5      4.77 NA                                0        2
#>  5 29b7d356         51.2      5.66 Anas platyrhynchos                1        6
#>  6 29b7d356         51.2      5.66 Anas strepera                     1        2
#>  7 29b7d356         51.2      5.66 Aves                              1        1
#>  8 29b7d356         51.2      5.66 NA                                0        2
#>  9 577b543a         51.2      5.66 Martes foina                      1        1
#> 10 577b543a         51.2      5.66 Mustela putorius                  1        3
#> 11 577b543a         51.2      5.66 Vulpes vulpes                     1        1
#> 12 577b543a         51.2      5.66 NA                                0        1
#> 13 62c200a9         50.7      4.01 Ardea                             1        2
#> 14 62c200a9         50.7      4.01 Aves                              1        1
#> 15 62c200a9         50.7      4.01 NA                                0        2
#> # ℹ 4 more variables: n_observations <int>, sum_count <int>,
#> #   rai_observations <dbl>, rai_count <dbl>

# Summarize observations by `locationId`, and `locationName` (summary by
# deployment columns only)
summarize_observations(x, group_by = "locationName")
#> # A tibble: 4 × 7
#> # Groups:   locationName [4]
#>   locationName                n_scientificName n_events n_observations sum_count
#>   <chr>                                  <int>    <int>          <int>     <int>
#> 1 B_DL_val 3_dikke boom                      3        6              6         5
#> 2 B_DL_val 5_beek kleine vij…                3       10             11        22
#> 3 B_DM_val 4_'t WAD                          2        5              5         3
#> 4 B_HS_val 2_processiepark                   3       10             14        26
#> # ℹ 2 more variables: rai_observations <dbl>, rai_count <dbl>

# Summarize observations by `scientificName` and `sex` (summary by
# observation columns only)
summarize_observations(x, group_by = c("scientificName", "sex"))
#> # A tibble: 12 × 8
#> # Groups:   scientificName, sex [12]
#>    scientificName     sex    n_scientificName n_events n_observations sum_count
#>    <chr>              <fct>             <int>    <int>          <int>     <int>
#>  1 Anas platyrhynchos female                2        7              7        11
#>  2 Anas platyrhynchos male                  1        3              3         6
#>  3 Anas platyrhynchos NA                    2        5              5        23
#>  4 Anas strepera      NA                    1        2              2         4
#>  5 Ardea              NA                    1        2              2         2
#>  6 Ardea cinerea      NA                    1        1              1         1
#>  7 Aves               NA                    2        2              2         2
#>  8 Martes foina       NA                    1        1              1         1
#>  9 Mustela putorius   NA                    1        3              3         3
#> 10 Rattus norvegicus  NA                    1        2              2         2
#> 11 Vulpes vulpes      NA                    1        1              1         1
#> 12 NA                 NA                    0        7              7         0
#> # ℹ 2 more variables: rai_observations <dbl>, rai_count <dbl>

# Apply temporal grouping by month
summarize_observations(x, group_time_by = "month")
#> # A tibble: 21 × 11
#> # Groups:   deploymentID, latitude, longitude, scientificName, month [21]
#>    deploymentID latitude longitude scientificName     month              
#>    <chr>           <dbl>     <dbl> <chr>              <dttm>             
#>  1 00a2c20d         51.5      4.77 Anas platyrhynchos 2020-05-01 00:00:00
#>  2 00a2c20d         51.5      4.77 Anas platyrhynchos 2020-06-01 00:00:00
#>  3 00a2c20d         51.5      4.77 Ardea cinerea      2020-06-01 00:00:00
#>  4 00a2c20d         51.5      4.77 Rattus norvegicus  2020-05-01 00:00:00
#>  5 00a2c20d         51.5      4.77 Rattus norvegicus  2020-06-01 00:00:00
#>  6 00a2c20d         51.5      4.77 NA                 2020-06-01 00:00:00
#>  7 00a2c20d         51.5      4.77 NA                 2020-07-01 00:00:00
#>  8 29b7d356         51.2      5.66 Anas platyrhynchos 2020-07-01 00:00:00
#>  9 29b7d356         51.2      5.66 Anas platyrhynchos 2020-08-01 00:00:00
#> 10 29b7d356         51.2      5.66 Anas strepera      2020-07-01 00:00:00
#> # ℹ 11 more rows
#> # ℹ 6 more variables: n_scientificName <int>, n_events <int>,
#> #   n_observations <int>, sum_count <int>, rai_observations <dbl>,
#> #   rai_count <dbl>

# Extend the summary to include all possible groups
summarize_observations(x, extend = TRUE)
#> # A tibble: 40 × 10
#> # Groups:   deploymentID, latitude, longitude, scientificName [40]
#>    deploymentID latitude longitude scientificName     n_scientificName n_events
#>    <chr>           <dbl>     <dbl> <chr>                         <int>    <int>
#>  1 00a2c20d         51.5      4.77 Anas platyrhynchos                1        6
#>  2 00a2c20d         51.5      4.77 Anas strepera                    NA        0
#>  3 00a2c20d         51.5      4.77 Ardea                            NA        0
#>  4 00a2c20d         51.5      4.77 Ardea cinerea                     1        1
#>  5 00a2c20d         51.5      4.77 Aves                             NA        0
#>  6 00a2c20d         51.5      4.77 Martes foina                     NA        0
#>  7 00a2c20d         51.5      4.77 Mustela putorius                 NA        0
#>  8 00a2c20d         51.5      4.77 Rattus norvegicus                 1        2
#>  9 00a2c20d         51.5      4.77 Vulpes vulpes                    NA        0
#> 10 00a2c20d         51.5      4.77 NA                                0        2
#> # ℹ 30 more rows
#> # ℹ 4 more variables: n_observations <int>, sum_count <int>,
#> #   rai_observations <dbl>, rai_count <dbl>
```
