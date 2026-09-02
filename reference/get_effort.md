# Get effort

**\[deprecated\]**

This function is deprecated. Use
[`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md)
instead.

Gets the effort (deployment duration) per deployment.

## Usage

``` r
get_effort(x, ..., unit = "hour")
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
[`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md),
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md)

## Examples

``` r
x <- example_dataset()
get_effort(x)
#> Warning: `get_effort()` was deprecated in camtraptor 1.0.0.
#> ℹ Please use `summarize_deployments(x, group_by = c("deploymentID",
#>   "latitude"), "longitude"))` instead. Notice also that the effort is only
#>   returned as a lubridate duration object in column `effort_duration`. The
#>   columns `effort` and `unit` are not returned anymore.
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
```
