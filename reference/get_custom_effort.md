# Get custom effort

Gets the effort for each deployment and a specific time interval such as
day, week, month or year. A custom time window can also be set up. This
function calls
[`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md)
internally.

## Usage

``` r
get_custom_effort(
  package = NULL,
  ...,
  start = NULL,
  end = NULL,
  group_by = NULL,
  unit = "hour",
  datapkg = lifecycle::deprecated()
)
```

## Arguments

- package:

  Camera trap data package object, as returned by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- ...:

  filter predicates

- start:

  Start date. Default: `NULL`. If `NULL` the earliest start date among
  all deployments is used. If `group_by` unit is not `NULL`, the lowest
  start value allowed is one group by unit before the start date of the
  earliest deployment. If this condition doesn't hold true, a warning is
  returned and the earliest start date among all deployments is used. If
  `group_by` unit is `NULL` the start must be later than or equal to the
  start date among all deployments.

- end:

  End date. Default: `NULL`. If `NULL` the latest end date among all
  deployments is used. If `group_by` unit is not `NULL`, the latest end
  value allowed is one group by unit after the end date of the latest
  deployment. If this condition doesn't hold true, a warning is returned
  and the latest end date among all deployments is used. If `group_by`
  unit is `NULL` the end must be earlier than or equal to the end date
  among all deployments.

- group_by:

  Character, one of `"day"`, `"week"`, `"month"`, `"year"`. The effort
  is calculated at the interval rate defined in `group_by`. Default:
  `NULL`: no grouping, i.e. the entire interval from `start` to `end` is
  taken into account as a whole. Calendar values are used, i.e. grouping
  by year will calculate the effort from Jan 1st up to Dec 31st for each
  year.

- unit:

  Character, the time unit to use while returning custom effort. One of:
  `hour` (default), `day`.

- datapkg:

  Deprecated. Use `package` instead.

## Value

A tibble data frame with following columns:

- `deploymentID`: Deployment unique identifier.

- `locationName`: Location name of the deployments.

- `begin`: Begin date of the interval the effort is calculated over.

- `effort`: The effort as number.

- `unit`: Character specifying the effort unit.

## See also

Other exploration functions:
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md),
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md),
[`get_scientific_name()`](https://inbo.github.io/camtraptor/reference/get_scientific_name.md),
[`get_species()`](https://inbo.github.io/camtraptor/reference/get_species.md)

## Examples

``` r
# Effort for each deployment over the entire duration of the project
# (datapackage) measured in hours (default)
get_custom_effort(mica)
#> # A tibble: 4 × 5
#>   deploymentID                         locationName      begin      effort unit 
#>   <chr>                                <chr>             <date>      <dbl> <chr>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek … 2019-10-09   239. hour 
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372 B_DL_val 3_dikke… 2019-10-09   219. hour 
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1 B_DM_val 4_'t WAD 2019-10-09   529. hour 
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Mica Viane        2019-10-09   335. hour 

# Effort for each deployment expressed in days
get_custom_effort(mica, unit = "day")
#> # A tibble: 4 × 5
#>   deploymentID                         locationName      begin      effort unit 
#>   <chr>                                <chr>             <date>      <dbl> <chr>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek … 2019-10-09   9.95 day  
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372 B_DL_val 3_dikke… 2019-10-09   9.11 day  
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1 B_DM_val 4_'t WAD 2019-10-09  22.0  day  
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Mica Viane        2019-10-09  13.9  day  

# Effort for each deployment from a specific start to a specific end
get_custom_effort(
  mica,
  start = as.Date("2019-12-15"), # or lubridate::as_date("2019-12-15")
  end = as.Date("2021-01-10")
)
#> # A tibble: 4 × 5
#>   deploymentID                         locationName      begin      effort unit 
#>   <chr>                                <chr>             <date>      <dbl> <chr>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek … 2019-12-15   239. hour 
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372 B_DL_val 3_dikke… 2019-12-15   219. hour 
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1 B_DM_val 4_'t WAD 2019-12-15     0  hour 
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Mica Viane        2019-12-15     0  hour 

# Effort for each deployment at daily interval
get_custom_effort(
  mica,
  group_by = "day"
)
#> # A tibble: 2,232 × 5
#>    deploymentID                         locationName     begin      effort unit 
#>    <chr>                                <chr>            <date>      <dbl> <chr>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-09      0 hour 
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-10      0 hour 
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-11      0 hour 
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-12      0 hour 
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-13      0 hour 
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-14      0 hour 
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-15      0 hour 
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-16      0 hour 
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-17      0 hour 
#> 10 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-18      0 hour 
#> # ℹ 2,222 more rows

# Effort for each deployment at weekly interval
get_custom_effort(
  mica,
  group_by = "week"
)
#> # A tibble: 324 × 5
#>    deploymentID                         locationName     begin      effort unit 
#>    <chr>                                <chr>            <date>      <dbl> <chr>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-06      0 hour 
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-13      0 hour 
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-20      0 hour 
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-27      0 hour 
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-11-03      0 hour 
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-11-10      0 hour 
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-11-17      0 hour 
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-11-24      0 hour 
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-12-01      0 hour 
#> 10 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-12-08      0 hour 
#> # ℹ 314 more rows

# Effort for each deployment at monthly interval
get_custom_effort(
  mica,
  group_by = "month"
)
#> # A tibble: 76 × 5
#>    deploymentID                         locationName     begin      effort unit 
#>    <chr>                                <chr>            <date>      <dbl> <chr>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-10-01    0   hour 
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-11-01    0   hour 
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-12-01    0   hour 
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2020-01-01    0   hour 
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2020-02-01    0   hour 
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2020-03-01    0   hour 
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2020-04-01    0   hour 
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2020-05-01    0   hour 
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2020-06-01    0   hour 
#> 10 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2020-07-01   66.5 hour 
#> # ℹ 66 more rows

# Effort for each deployment at yearly interval
get_custom_effort(
  mica,
  group_by = "year"
)
#> # A tibble: 12 × 5
#>    deploymentID                         locationName     begin      effort unit 
#>    <chr>                                <chr>            <date>      <dbl> <chr>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2019-01-01     0  hour 
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2020-01-01   239. hour 
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek… 2021-01-01     0  hour 
#>  4 577b543a-2cf1-4b23-b6d2-cda7e2eac372 B_DL_val 3_dikk… 2019-01-01     0  hour 
#>  5 577b543a-2cf1-4b23-b6d2-cda7e2eac372 B_DL_val 3_dikk… 2020-01-01   219. hour 
#>  6 577b543a-2cf1-4b23-b6d2-cda7e2eac372 B_DL_val 3_dikk… 2021-01-01     0  hour 
#>  7 62c200a9-0e03-4495-bcd8-032944f6f5a1 B_DM_val 4_'t W… 2019-01-01     0  hour 
#>  8 62c200a9-0e03-4495-bcd8-032944f6f5a1 B_DM_val 4_'t W… 2020-01-01     0  hour 
#>  9 62c200a9-0e03-4495-bcd8-032944f6f5a1 B_DM_val 4_'t W… 2021-01-01   529. hour 
#> 10 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Mica Viane       2019-01-01   335. hour 
#> 11 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Mica Viane       2020-01-01     0  hour 
#> 12 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Mica Viane       2021-01-01     0  hour 

# Applying filter(s), e.g. deployments with latitude >= 51.18, can be
# combined with other arguments
get_custom_effort(mica, pred_gte("latitude", 51.18), group_by = "month")
#> df %>% dplyr::filter((latitude >= 51.18))
#> # A tibble: 6 × 5
#>   deploymentID                         locationName      begin      effort unit 
#>   <chr>                                <chr>             <date>      <dbl> <chr>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek … 2020-06-01    0   hour 
#> 2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek … 2020-07-01   66.5 hour 
#> 3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 B_DL_val 5_beek … 2020-08-01  172.  hour 
#> 4 577b543a-2cf1-4b23-b6d2-cda7e2eac372 B_DL_val 3_dikke… 2020-06-01  219.  hour 
#> 5 577b543a-2cf1-4b23-b6d2-cda7e2eac372 B_DL_val 3_dikke… 2020-07-01    0   hour 
#> 6 577b543a-2cf1-4b23-b6d2-cda7e2eac372 B_DL_val 3_dikke… 2020-08-01    0   hour 

# You can afterwards calculate the total effort over all deployments
library(dplyr)
get_custom_effort(mica, group_by = "year", unit = "day") %>%
  dplyr::filter(effort > 0) %>%
  dplyr::group_by(begin) %>% 
  dplyr::summarise(
    deploymentIDs = list(deploymentID),
    locationNames = list(locationName),
    ndep = length(unique(deploymentID)),
    nloc = length(unique(locationName)),
    effort = sum(effort),
    unit = unique(unit)
  )
#> # A tibble: 3 × 7
#>   begin      deploymentIDs locationNames  ndep  nloc effort unit 
#>   <date>     <list>        <list>        <int> <int>  <dbl> <chr>
#> 1 2019-01-01 <chr [1]>     <chr [1]>         1     1   13.9 day  
#> 2 2020-01-01 <chr [2]>     <chr [2]>         2     2   19.1 day  
#> 3 2021-01-01 <chr [1]>     <chr [1]>         1     1   22.0 day  
```
