# Get number of individuals for each deployment

Gets the number of individuals (of a subset of species) per deployment.
The number of observed individuals is stored in field `count` of
`observations`.

## Usage

``` r
get_n_individuals(
  package = NULL,
  ...,
  species = "all",
  sex = NULL,
  life_stage = NULL,
  datapkg = lifecycle::deprecated()
)
```

## Arguments

- package:

  Camera trap data package object, as returned by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- ...:

  filter predicates for filtering on deployments

- species:

  Character with scientific names or common names (case insensitive). If
  `"all"` (default) all scientific names are automatically selected. If
  `NULL` all observations of all species are taken into account.

- sex:

  Character defining the sex class to filter on, e.g. `"female"` or
  `c("male", "unknown")`. If `NULL` (default) all observations of all
  sex classes are taken into account.

- life_stage:

  Character vector defining the life stage class to filter on, e.g.
  `"adult"` or `c("subadult", "adult")`. If `NULL` (default) all
  observations of all life stage classes are taken into account.

- datapkg:

  Deprecated. Use `package` instead.

## Value

A tibble data frame with the following columns:

- `deploymentID`: Deployment unique identifier.

- `scientificName`: Scientific name of the species. This column is
  omitted if parameter `species = NULL`.

- `n`: Number of individuals.

## See also

Other exploration functions:
[`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md),
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md),
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md),
[`get_scientific_name()`](https://inbo.github.io/camtraptor/reference/get_scientific_name.md),
[`get_species()`](https://inbo.github.io/camtraptor/reference/get_species.md)

## Examples

``` r
# Get number of observations for each species
get_n_individuals(mica)
#> # A tibble: 36 × 3
#>    deploymentID                         scientificName         n
#>    <chr>                                <chr>              <int>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera          6
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos    13
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber           0
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius       0
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes          0
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina           0
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea          0
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                  0
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens           0
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas strepera          0
#> # ℹ 26 more rows

# Get number of obs of all species, not identified individuals as well
get_n_individuals(mica, species = NULL)
#> # A tibble: 4 × 2
#>   deploymentID                             n
#>   <chr>                                <int>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8    19
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372     6
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1     3
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7     2

# Get number of observations of Anas platyrhynchos
get_n_individuals(mica, species = "Anas platyrhynchos")
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 4 × 3
#>   deploymentID                         scientificName         n
#>   <chr>                                <chr>              <int>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos    13
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos     0
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1 Anas platyrhynchos     0
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Anas platyrhynchos     0

# Get number of observations of eurasian beaver (vernacular name)
get_n_individuals(mica, species = "eurasian beaver")
#> Scientific name of eurasian beaver: Castor fiber
#> There are 3 deployments without observations: 29b7d356-4bb4-4ec4-b792-2af5cc32efa8, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 4 × 3
#>   deploymentID                         scientificName     n
#>   <chr>                                <chr>          <int>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber       0
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Castor fiber       1
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1 Castor fiber       0
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Castor fiber       0

# Mix scientific and vernacular names
get_n_individuals(mica, species = c("Anas platyrhynchos", "eurasian beaver"))
#> Scientific name of eurasian beaver: Castor fiber
#> There are 2 deployments without observations: 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 8 × 3
#>   deploymentID                         scientificName         n
#>   <chr>                                <chr>              <int>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos    13
#> 2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber           0
#> 3 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos     0
#> 4 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Castor fiber           1
#> 5 62c200a9-0e03-4495-bcd8-032944f6f5a1 Anas platyrhynchos     0
#> 6 62c200a9-0e03-4495-bcd8-032944f6f5a1 Castor fiber           0
#> 7 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Anas platyrhynchos     0
#> 8 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Castor fiber           0

# Case insensitive
get_n_individuals(mica, species = "AnAS PLatyrhyncHOS")
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 4 × 3
#>   deploymentID                         scientificName         n
#>   <chr>                                <chr>              <int>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos    13
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos     0
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1 Anas platyrhynchos     0
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Anas platyrhynchos     0
get_n_individuals(mica, species = "eurasian BEAVER")
#> Scientific name of eurasian BEAVER: Castor fiber
#> There are 3 deployments without observations: 29b7d356-4bb4-4ec4-b792-2af5cc32efa8, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 4 × 3
#>   deploymentID                         scientificName     n
#>   <chr>                                <chr>          <int>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber       0
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Castor fiber       1
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1 Castor fiber       0
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Castor fiber       0

# Specify life stage
get_n_individuals(mica, life_stage = "adult")
#> There are 1 deployments without observations: 62c200a9-0e03-4495-bcd8-032944f6f5a1
#> # A tibble: 36 × 3
#>    deploymentID                         scientificName         n
#>    <chr>                                <chr>              <int>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera          1
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber           0
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius       0
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes          0
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina           0
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens           0
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos     0
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                  0
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea          0
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas strepera          0
#> # ℹ 26 more rows

# Specify sex
get_n_individuals(mica, sex = "female")
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 36 × 3
#>    deploymentID                         scientificName         n
#>    <chr>                                <chr>              <int>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera          1
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos     0
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                  0
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea          0
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber           0
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens           0
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina           0
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius       0
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes          0
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas strepera          0
#> # ℹ 26 more rows

# Specify both sex and life stage
get_n_individuals(mica, sex = "unknown", life_stage = "adult")
#> There are 3 deployments without observations: 29b7d356-4bb4-4ec4-b792-2af5cc32efa8, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 36 × 3
#>    deploymentID                         scientificName         n
#>    <chr>                                <chr>              <int>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber           0
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius       0
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes          0
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina           0
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos     0
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera          0
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                  0
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea          0
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens           0
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Castor fiber           1
#> # ℹ 26 more rows

# Apply filter(s), e.g. deployments with latitude >= 51.18
get_n_individuals(mica, pred_gte("latitude", 51.18))
#> df %>% dplyr::filter((latitude >= 51.18))
#> # A tibble: 18 × 3
#>    deploymentID                         scientificName         n
#>    <chr>                                <chr>              <int>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera          6
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos    13
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber           0
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius       0
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes          0
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina           0
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea          0
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                  0
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens           0
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas strepera          0
#> 11 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos     0
#> 12 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Castor fiber           1
#> 13 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Mustela putorius       3
#> 14 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Vulpes vulpes          1
#> 15 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Martes foina           1
#> 16 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Ardea cinerea          0
#> 17 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Ardea                  0
#> 18 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Homo sapiens           0
```
