# Get number of identified species for each deployment

Gets the number of identified species per deployment.

## Usage

``` r
get_n_species(package = NULL, ..., datapkg = lifecycle::deprecated())
```

## Arguments

- package:

  Camera trap data package object, as returned by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- ...:

  Filter predicates for filtering on deployments.

- datapkg:

  Deprecated. Use `package` instead.

## Value

A tibble data frame with the following columns:

- `deploymentID`: Deployment unique identifier.

- `n`: Number of observed and identified species.

## See also

Other exploration functions:
[`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md),
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md),
[`get_scientific_name()`](https://inbo.github.io/camtraptor/reference/get_scientific_name.md),
[`get_species()`](https://inbo.github.io/camtraptor/reference/get_species.md)

## Examples

``` r
# Get number of species
get_n_species(mica)
#> # A tibble: 4 × 2
#>   deploymentID                             n
#>   <chr>                                <int>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8     2
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372     4
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1     2
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7     1

# Get number of species for deployments with latitude >= 51.18
get_n_species(mica, pred_gte("latitude", 51.18))
#> df %>% dplyr::filter((latitude >= 51.18))
#> # A tibble: 2 × 2
#>   deploymentID                             n
#>   <chr>                                <int>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8     2
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372     4
```
