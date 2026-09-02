# Get number of identified species for each deployment

**\[deprecated\]**

This function is deprecated. Use
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md)
instead.

## Usage

``` r
get_n_species(x, ...)
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

## Value

A tibble data frame with the following columns:

- `deploymentID`: Deployment unique identifier.

- `n`: Number of observed and identified species.

## See also

Other deprecated exploration functions:
[`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md),
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md)

## Examples

``` r
x <- example_dataset()
# Get number of species
get_n_species(x)
#> Warning: `get_n_species()` was deprecated in camtraptor 1.0.0.
#> ℹ Please use `summarize_observations(x, group_by = c("deploymentID",
#>   "latitude"), "longitude"), "scientificName")` instead.
#> ℹ The deprecated feature was likely used in the camtraptor package.
#>   Please report the issue at <https://github.com/inbo/camtraptor/issues>.
#> # A tibble: 4 × 2
#> # Groups:   deploymentID [4]
#>   deploymentID     n
#>   <chr>        <int>
#> 1 00a2c20d         3
#> 2 29b7d356         3
#> 3 577b543a         3
#> 4 62c200a9         2
```
