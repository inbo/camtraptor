# Get Relative Abundance Index (RAI)

**\[deprecated\]**

## Usage

``` r
get_rai(x, ..., species = "all", sex = NULL, life_stage = NULL)
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

- `deploymentID`: character vector with the deployment unique
  identifiers.

- `scientificName`: character vector with the scientific name.

- `rai`: numeric vector with the relative abundance index.

## Details

Gets the RAI (Relative Abundance Index) per deployment. The RAI is
normalized using 100 days deployment activity. In other words:
`RAI = 100 * (n/effort)` where `n` is the number of event-based
observations and `effort` is the effort duration in days.

It is deprecated as of camtraptor 1.0.0. Please use
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md)instead.

## See also

Other deprecated exploration functions:
[`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md),
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md)

## Examples

``` r
x <- example_dataset()

# Calculate RAI for all species
get_rai(x) # species = "all" by default, so equivalent of
#> Warning: `get_rai()` was deprecated in camtraptor 1.0.0.
#> ℹ Please use `summarize_observations(x, group_by = c("deploymentID",
#>   "latitude"), "longitude"), "scientificName")` instead.
#> ℹ The deprecated feature was likely used in the camtraptor package.
#>   Please report the issue at <https://github.com/inbo/camtraptor/issues>.
#> # A tibble: 15 × 3
#> # Groups:   deploymentID, scientificName [15]
#>    deploymentID scientificName       rai
#>    <chr>        <chr>              <dbl>
#>  1 00a2c20d     Anas platyrhynchos 27.9 
#>  2 00a2c20d     Ardea cinerea       3.10
#>  3 00a2c20d     Rattus norvegicus   6.20
#>  4 00a2c20d     NA                  6.20
#>  5 29b7d356     Anas platyrhynchos 60.3 
#>  6 29b7d356     Anas strepera      20.1 
#>  7 29b7d356     Aves               10.0 
#>  8 29b7d356     NA                 20.1 
#>  9 577b543a     Martes foina       11.0 
#> 10 577b543a     Mustela putorius   32.9 
#> 11 577b543a     Vulpes vulpes      11.0 
#> 12 577b543a     NA                 11.0 
#> 13 62c200a9     Ardea               9.08
#> 14 62c200a9     Aves                4.54
#> 15 62c200a9     NA                  9.08
get_rai(x, species = "all")
#> # A tibble: 15 × 3
#> # Groups:   deploymentID, scientificName [15]
#>    deploymentID scientificName       rai
#>    <chr>        <chr>              <dbl>
#>  1 00a2c20d     Anas platyrhynchos 27.9 
#>  2 00a2c20d     Ardea cinerea       3.10
#>  3 00a2c20d     Rattus norvegicus   6.20
#>  4 00a2c20d     NA                  6.20
#>  5 29b7d356     Anas platyrhynchos 60.3 
#>  6 29b7d356     Anas strepera      20.1 
#>  7 29b7d356     Aves               10.0 
#>  8 29b7d356     NA                 20.1 
#>  9 577b543a     Martes foina       11.0 
#> 10 577b543a     Mustela putorius   32.9 
#> 11 577b543a     Vulpes vulpes      11.0 
#> 12 577b543a     NA                 11.0 
#> 13 62c200a9     Ardea               9.08
#> 14 62c200a9     Aves                4.54
#> 15 62c200a9     NA                  9.08

# Selected species
get_rai(x, species = c("Anas platyrhynchos", "Martes foina"))
#> Warning: The `species` argument of `get_rai()` is deprecated as of camtraptor 1.0.0.
#> ℹ Argument `species` is deprecated as of camtraptor 1.0.0. Please, use
#>   `filter_observations()` to filter by `scientificName`.
#> ℹ The deprecated feature was likely used in the camtraptor package.
#>   Please report the issue at <https://github.com/inbo/camtraptor/issues>.
#> # A tibble: 3 × 3
#> # Groups:   deploymentID, scientificName [3]
#>   deploymentID scientificName       rai
#>   <chr>        <chr>              <dbl>
#> 1 00a2c20d     Anas platyrhynchos  27.9
#> 2 29b7d356     Anas platyrhynchos  60.3
#> 3 577b543a     Martes foina        11.0
```
