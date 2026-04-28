# Get Relative Abundance Index (RAI)

Gets the RAI (Relative Abundance Index) per deployment. The RAI is
normalized using 100 days deployment activity. In other words:
`RAI = 100 * (n/effort)` where `n` is the number of observations as
calculated via
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md)
and `effort` is the effort in days as calculated via
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md).

## Usage

``` r
get_rai(
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

  Filter predicates for filtering on deployments.

- species:

  Character with scientific names or common names (case insensitive). If
  `"all"` (default) all scientific names are automatically selected.

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

- `scientificName`: Scientific name.

- `rai`: Relative abundance index.

## See also

Other exploration functions:
[`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md),
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md),
[`get_scientific_name()`](https://inbo.github.io/camtraptor/reference/get_scientific_name.md),
[`get_species()`](https://inbo.github.io/camtraptor/reference/get_species.md)

## Examples

``` r
# Calculate RAI for all species
get_rai(mica) # species = "all" by default, so equivalent of
#> # A tibble: 36 × 3
#>    deploymentID                         scientificName       rai
#>    <chr>                                <chr>              <dbl>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos  40.2
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera       30.1
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                0  
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea        0  
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber         0  
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens         0  
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina         0  
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius     0  
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes        0  
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> # ℹ 26 more rows
get_rai(mica, species = "all")
#> # A tibble: 36 × 3
#>    deploymentID                         scientificName       rai
#>    <chr>                                <chr>              <dbl>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos  40.2
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera       30.1
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                0  
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea        0  
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber         0  
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens         0  
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina         0  
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius     0  
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes        0  
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> # ℹ 26 more rows

# Selected species
get_rai(mica, species = c("Anas platyrhynchos", "Martes foina"))
#> There are 2 deployments without observations: 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 8 × 3
#>   deploymentID                         scientificName       rai
#>   <chr>                                <chr>              <dbl>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos  40.2
#> 2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina         0  
#> 3 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> 4 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Martes foina        11.0
#> 5 62c200a9-0e03-4495-bcd8-032944f6f5a1 Anas platyrhynchos   0  
#> 6 62c200a9-0e03-4495-bcd8-032944f6f5a1 Martes foina         0  
#> 7 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Anas platyrhynchos   0  
#> 8 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Martes foina         0  

# With vernacular names, even mixing languages
get_rai(mica, species = c("mallard", "steenmarter"))
#> Scientific name of mallard: Anas platyrhynchos
#> Scientific name of steenmarter: Martes foina
#> There are 2 deployments without observations: 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 8 × 3
#>   deploymentID                         scientificName       rai
#>   <chr>                                <chr>              <dbl>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos  40.2
#> 2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina         0  
#> 3 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> 4 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Martes foina        11.0
#> 5 62c200a9-0e03-4495-bcd8-032944f6f5a1 Anas platyrhynchos   0  
#> 6 62c200a9-0e03-4495-bcd8-032944f6f5a1 Martes foina         0  
#> 7 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Anas platyrhynchos   0  
#> 8 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Martes foina         0  

# Mixed scientific and vernacular names
get_rai(mica, species = c("Anas platyrhynchos", "steenmarter"))
#> Scientific name of steenmarter: Martes foina
#> There are 2 deployments without observations: 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 8 × 3
#>   deploymentID                         scientificName       rai
#>   <chr>                                <chr>              <dbl>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos  40.2
#> 2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina         0  
#> 3 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> 4 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Martes foina        11.0
#> 5 62c200a9-0e03-4495-bcd8-032944f6f5a1 Anas platyrhynchos   0  
#> 6 62c200a9-0e03-4495-bcd8-032944f6f5a1 Martes foina         0  
#> 7 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Anas platyrhynchos   0  
#> 8 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Martes foina         0  

# Species parameter is case insensitive
get_rai(mica, species = c("ANAS plAtyRhynChOS"))
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 4 × 3
#>   deploymentID                         scientificName       rai
#>   <chr>                                <chr>              <dbl>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos  40.2
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1 Anas platyrhynchos   0  
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7 Anas platyrhynchos   0  

# Specify sex
get_rai(mica, sex = "female")
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 36 × 3
#>    deploymentID                         scientificName       rai
#>    <chr>                                <chr>              <dbl>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos   0  
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera       10.0
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                0  
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea        0  
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber         0  
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens         0  
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina         0  
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius     0  
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes        0  
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> # ℹ 26 more rows
get_rai(mica, sex = c("female", "unknown"))
#> There are 1 deployments without observations: 7ca633fa-64f8-4cfc-a628-6b0c419056d7
#> # A tibble: 36 × 3
#>    deploymentID                         scientificName       rai
#>    <chr>                                <chr>              <dbl>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos  40.2
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera       30.1
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                0  
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea        0  
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber         0  
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens         0  
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina         0  
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius     0  
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes        0  
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> # ℹ 26 more rows

# Specify life stage
get_rai(mica, life_stage = "adult")
#> There are 1 deployments without observations: 62c200a9-0e03-4495-bcd8-032944f6f5a1
#> # A tibble: 36 × 3
#>    deploymentID                         scientificName       rai
#>    <chr>                                <chr>              <dbl>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos   0  
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera       10.0
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                0  
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea        0  
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber         0  
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens         0  
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina         0  
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius     0  
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes        0  
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> # ℹ 26 more rows
get_rai(mica, life_stage = c("adult", "subadult"))
#> There are 1 deployments without observations: 62c200a9-0e03-4495-bcd8-032944f6f5a1
#> # A tibble: 36 × 3
#>    deploymentID                         scientificName       rai
#>    <chr>                                <chr>              <dbl>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos  30.1
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera       30.1
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                0  
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea        0  
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber         0  
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens         0  
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina         0  
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius     0  
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes        0  
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> # ℹ 26 more rows

# Apply filter(s): deployments with latitude >= 51.18
get_rai(mica, pred_gte("latitude", 51.18))
#> df %>% dplyr::filter((latitude >= 51.18))
#> df %>% dplyr::filter((latitude >= 51.18))
#> # A tibble: 18 × 3
#>    deploymentID                         scientificName       rai
#>    <chr>                                <chr>              <dbl>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos  40.2
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera       30.1
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                0  
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea        0  
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber         0  
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens         0  
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina         0  
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius     0  
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes        0  
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos   0  
#> 11 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas strepera        0  
#> 12 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Ardea                0  
#> 13 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Ardea cinerea        0  
#> 14 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Castor fiber        11.0
#> 15 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Homo sapiens         0  
#> 16 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Martes foina        11.0
#> 17 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Mustela putorius    32.9
#> 18 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Vulpes vulpes       11.0
```
