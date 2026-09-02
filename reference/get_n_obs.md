# Get number of observations for each deployment

**\[deprecated\]**

This function is deprecated. Use
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md)
instead.

Gets the number of event-based observations per deployment.

## Usage

``` r
get_n_obs(x, ..., species = "all", sex = NULL, life_stage = NULL)
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

- species:

  **\[deprecated\]** Character with scientific names. Common names are
  not supported anymore as of camtraptor 1.0.0. Please, check
  [`filter_observations()`](https://inbo.github.io/camtraptor/reference/filter_observations.md)
  to know how to filter by `scientificName`. If `"all"` (default) all
  scientific names are automatically selected. If `NULL` all
  observations of all species are taken into account.

- sex:

  **\[deprecated\]** Character defining the sex class to filter on, e.g.
  `"female"` or `c("male", "unknown")`. If `NULL` (default) all
  observations of all sex classes are taken into account. Please, check
  [`filter_observations()`](https://inbo.github.io/camtraptor/reference/filter_observations.md)
  to know how to filter by `sex`.

- life_stage:

  **\[deprecated\]** Character vector defining the life stage class to
  filter on, e.g. `"adult"` or `c("subadult", "adult")`. If `NULL`
  (default) all observations of all life stage classes are taken into
  account. Please, check
  [`filter_observations()`](https://inbo.github.io/camtraptor/reference/filter_observations.md)
  to know how to filter by `lifeStage`.

## Value

A tibble data frame with the following columns:

- `deploymentID`: Deployment unique identifier.

- `scientificName`: Scientific name of the species. This column is
  omitted if parameter `species = NULL`.

- `n`: Number of observations.

## See also

Other deprecated exploration functions:
[`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md),
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md),
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md)

## Examples

``` r
x <- example_dataset()

# Get number of observations for each species
get_n_obs(x)
#> Warning: `get_n_obs()` was deprecated in camtraptor 1.0.0.
#> ℹ Please use `summarize_observations(x, group_by = c("deploymentID",
#>   "latitude"), "longitude"), "scientificName")` instead.
#> ℹ The deprecated feature was likely used in the camtraptor package.
#>   Please report the issue at <https://github.com/inbo/camtraptor/issues>.
#> # A tibble: 15 × 3
#> # Groups:   deploymentID, scientificName [15]
#>    deploymentID scientificName         n
#>    <chr>        <chr>              <int>
#>  1 00a2c20d     Anas platyrhynchos     9
#>  2 00a2c20d     Ardea cinerea          1
#>  3 00a2c20d     Rattus norvegicus      2
#>  4 00a2c20d     NA                     2
#>  5 29b7d356     Anas platyrhynchos     6
#>  6 29b7d356     Anas strepera          2
#>  7 29b7d356     Aves                   1
#>  8 29b7d356     NA                     2
#>  9 577b543a     Martes foina           1
#> 10 577b543a     Mustela putorius       3
#> 11 577b543a     Vulpes vulpes          1
#> 12 577b543a     NA                     1
#> 13 62c200a9     Ardea                  2
#> 14 62c200a9     Aves                   1
#> 15 62c200a9     NA                     2

# Get number of obs of all species, not identified individuals as well
get_n_obs(x, species = NULL)
#> # A tibble: 4 × 2
#> # Groups:   deploymentID [4]
#>   deploymentID     n
#>   <chr>        <int>
#> 1 00a2c20d        14
#> 2 29b7d356        11
#> 3 577b543a         6
#> 4 62c200a9         5
 
# Get number of observations of Anas platyrhynchos
get_n_obs(x, species = "Anas platyrhynchos")
#> Warning: The `species` argument of `get_n_obs()` is deprecated as of camtraptor 1.0.0.
#> ℹ Argument `species` is deprecated as of camtraptor 1.0.0. Please, use
#>   `filter_observations()` to filter by `scientificName`.
#> ℹ The deprecated feature was likely used in the camtraptor package.
#>   Please report the issue at <https://github.com/inbo/camtraptor/issues>.
#> # A tibble: 2 × 3
#> # Groups:   deploymentID, scientificName [2]
#>   deploymentID scientificName         n
#>   <chr>        <chr>              <int>
#> 1 00a2c20d     Anas platyrhynchos     9
#> 2 29b7d356     Anas platyrhynchos     6

# Specify sex
get_n_obs(x, sex = "female")
#> Warning: The `sex` argument of `get_n_obs()` is deprecated as of camtraptor 1.0.0.
#> ℹ Argument `sex` is deprecated as of camtraptor 1.0.0. Please, use
#>   `filter_observations()` to filter by `sex`.
#> ℹ The deprecated feature was likely used in the camtraptor package.
#>   Please report the issue at <https://github.com/inbo/camtraptor/issues>.
#> # A tibble: 2 × 3
#> # Groups:   deploymentID, scientificName [2]
#>   deploymentID scientificName         n
#>   <chr>        <chr>              <int>
#> 1 00a2c20d     Anas platyrhynchos     4
#> 2 29b7d356     Anas platyrhynchos     3

# Specify life stage
get_n_obs(x, life_stage = c("subadult", "adult"))
#> Warning: The `life_stage` argument of `get_n_obs()` is deprecated as of camtraptor
#> 1.0.0.
#> ℹ Argument `life_stage` is deprecated as of camtraptor 1.0.0. Please, use
#>   `filter_observations()` to filter by `lifeStage`.
#> ℹ The deprecated feature was likely used in the camtraptor package.
#>   Please report the issue at <https://github.com/inbo/camtraptor/issues>.
#> # A tibble: 7 × 3
#> # Groups:   deploymentID, scientificName [7]
#>   deploymentID scientificName         n
#>   <chr>        <chr>              <int>
#> 1 00a2c20d     Anas platyrhynchos     7
#> 2 00a2c20d     Ardea cinerea          1
#> 3 29b7d356     Anas platyrhynchos     6
#> 4 29b7d356     Anas strepera          2
#> 5 577b543a     Martes foina           1
#> 6 577b543a     Mustela putorius       3
#> 7 577b543a     Vulpes vulpes          1
```
