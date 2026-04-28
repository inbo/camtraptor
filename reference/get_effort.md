# Get effort

Gets the effort (deployment duration) per deployment.

## Usage

``` r
get_effort(
  package = NULL,
  ...,
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

- unit:

  Time unit to use while returning deployment effort (duration). One of:

  - `second`

  - `minute`

  - `hour`

  - `day`

  - `month`

  - `year`

- datapkg:

  Deprecated. Use `package` instead.

## Value

A tibble data frame with following columns:

- `deploymentID`: Deployment unique identifier.

- `effort`: Effort expressed in the unit passed by parameter `unit`.

- `unit`: The unit used to express the effort. One of the values
  available for parameter `unit`.

- `effort_duration`: A duration object (duration is a class from
  lubridate package).

## See also

Other exploration functions:
[`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md),
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md),
[`get_scientific_name()`](https://inbo.github.io/camtraptor/reference/get_scientific_name.md),
[`get_species()`](https://inbo.github.io/camtraptor/reference/get_species.md)

## Examples

``` r
# Efforts expressed in hours
get_effort(mica)
#> # A tibble: 4 × 4
#>   deploymentID                         effort unit  effort_duration       
#>   <chr>                                 <dbl> <chr> <Duration>            
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8   239. hour  859859s (~1.42 weeks) 
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372   219. hour  786802s (~1.3 weeks)  
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1   529. hour  1903602s (~3.15 weeks)
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7   335. hour  1204929s (~1.99 weeks)

# Effort expressed as days
get_effort(mica, unit = "day")
#> # A tibble: 4 × 4
#>   deploymentID                         effort unit  effort_duration       
#>   <chr>                                 <dbl> <chr> <Duration>            
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8   9.95 day   859859s (~1.42 weeks) 
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372   9.11 day   786802s (~1.3 weeks)  
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1  22.0  day   1903602s (~3.15 weeks)
#> 4 7ca633fa-64f8-4cfc-a628-6b0c419056d7  13.9  day   1204929s (~1.99 weeks)
```
