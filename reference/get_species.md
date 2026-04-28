# Get species

Gets all identified species.

## Usage

``` r
get_species(package = NULL, datapkg = lifecycle::deprecated())
```

## Arguments

- package:

  Camera trap data package object, as returned by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- datapkg:

  Deprecated. Use `package` instead.

## Value

A tibble data frame with all scientific names and vernacular names of
the identified species.

## See also

Other exploration functions:
[`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md),
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md),
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md),
[`get_scientific_name()`](https://inbo.github.io/camtraptor/reference/get_scientific_name.md)

## Examples

``` r
get_species(mica)
#> # A tibble: 9 × 5
#>   taxonID taxonIDReference  scientificName vernacularNames.en vernacularNames.nl
#>   <chr>   <chr>             <chr>          <chr>              <chr>             
#> 1 DGP6    https://www.cata… Anas platyrhy… mallard            wilde eend        
#> 2 DGPL    https://www.cata… Anas strepera  gadwall            krakeend          
#> 3 32FH    https://www.cata… Ardea          great herons       reigers           
#> 4 GCHS    https://www.cata… Ardea cinerea  grey heron         blauwe reiger     
#> 5 RQPW    https://www.cata… Castor fiber   Eurasian beaver    bever             
#> 6 6MB3T   https://www.cata… Homo sapiens   human              mens              
#> 7 3Y9VW   https://www.cata… Martes foina   beech marten       steenmarter       
#> 8 44QYC   https://www.cata… Mustela putor… European polecat   bunzing           
#> 9 5BSG3   https://www.cata… Vulpes vulpes  red fox            vos               
```
