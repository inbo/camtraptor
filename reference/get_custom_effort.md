# Get custom effort

**\[deprecated\]**

This function is deprecated. Use
[`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md)
instead.

Gets the effort for each deployment and a specific time interval such as
day, week, month or year.

## Usage

``` r
get_custom_effort(
  x,
  ...,
  start = NULL,
  end = NULL,
  group_by = NULL,
  unit = "hour"
)
```

## Arguments

- x:

  Camera trap data package object, as returned by
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

- ...:

  **\[deprecated\]** filter predicates for filtering on deployments are
  not supported anymore and an error is returned. Anything else throws a
  deprecation warning and will be ignored. Please use
  [`filter_deployments()`](https://inbo.github.io/camtraptor/reference/filter_deployments.md)
  to filter on deployments.

- start:

  **\[deprecated\]** if not `NULL `. Not supported anymore. Use
  [`filter_deployments()`](https://inbo.github.io/camtraptor/reference/filter_deployments.md)
  to filter out deployments.

- end:

  **\[deprecated\]** if not `NULL `. Not supported anymore. Use
  [`filter_deployments()`](https://inbo.github.io/camtraptor/reference/filter_deployments.md)
  to filter out deployments.

- group_by:

  Character, one of `"day"`, `"week"`, `"month"`, `"year"`. Default:
  `NULL`. See `group_time_by` argument in `[summarize_deployments()]`.

- unit:

  **\[deprecated\]** The unit used to quantify the effort. Ignored as
  the effort is returned only as a duration object.

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

## See also

Other deprecated exploration functions:
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md),
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md)

## Examples

``` r
x <- example_dataset()

# Effort for each deployment over the entire duration of the project
get_custom_effort(x)
#> Warning: `get_custom_effort()` was deprecated in camtraptor 1.0.0.
#> ℹ Please use `summarize_deployments(x, group_by = c("deploymentID",
#>   "latitude"), "longitude"))` instead. Notice also that the effort is only
#>   returned as a lubridate duration object in column `effort_duration`. The
#>   columns `effort` and `unit` are not returned anymore.
#> ℹ The deprecated feature was likely used in the camtraptor package.
#>   Please report the issue at <https://github.com/inbo/camtraptor/issues>.
#> Warning: The `unit` argument of `get_effort()` is deprecated as of camtraptor 1.0.0.
#> ℹ The effort is now only returned as a lubridate duration object in column
#>   `effort_duration`. To suppress this warning, set `unit = NULL`.
#> ℹ The deprecated feature was likely used in the camtraptor package.
#>   Please report the issue at <https://github.com/inbo/camtraptor/issues>.
#> # A tibble: 4 × 2
#> # Groups:   deploymentID [4]
#>   deploymentID effort_duration       
#>   <chr>        <Duration>            
#> 1 00a2c20d     2789044s (~4.61 weeks)
#> 2 29b7d356     859859s (~1.42 weeks) 
#> 3 577b543a     786802s (~1.3 weeks)  
#> 4 62c200a9     1903602s (~3.15 weeks)

# Effort at monthly interval
get_custom_effort(x, group_by = "month")
#> # A tibble: 8 × 3
#> # Groups:   deploymentID, month [8]
#>   deploymentID month               effort_duration       
#>   <chr>        <dttm>              <Duration>            
#> 1 00a2c20d     2020-05-01 00:00:00 162143s (~1.88 days)  
#> 2 00a2c20d     2020-06-01 00:00:00 2592000s (~4.29 weeks)
#> 3 00a2c20d     2020-07-01 00:00:00 34901s (~9.69 hours)  
#> 4 29b7d356     2020-07-01 00:00:00 239419s (~2.77 days)  
#> 5 29b7d356     2020-08-01 00:00:00 620440s (~1.03 weeks) 
#> 6 577b543a     2020-06-01 00:00:00 786802s (~1.3 weeks)  
#> 7 62c200a9     2021-03-01 00:00:00 357702s (~4.14 days)  
#> 8 62c200a9     2021-04-01 00:00:00 1545900s (~2.56 weeks)

# You can afterwards calculate the total effort over all deployments
library(dplyr)
get_custom_effort(x, group_by = "year") %>%
  filter(effort_duration > 0) %>%
  group_by(year) %>% 
  summarise(
    deploymentIDs = list(deploymentID),
    ndep = n_distinct(deploymentID),
    effort_duration = sum(effort_duration)
  )
#> # A tibble: 2 × 4
#>   year                deploymentIDs  ndep effort_duration
#>   <dttm>              <list>        <int>           <dbl>
#> 1 2020-01-01 00:00:00 <chr [3]>         3         4435705
#> 2 2021-01-01 00:00:00 <chr [1]>         1         1903602
```
