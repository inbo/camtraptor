# Get species

**\[deprecated\]**

It is deprecated as of camtraptor 1.0.0. Please use
[`taxa()`](https://inbo.github.io/camtraptor/reference/taxa.md) instead.

## Usage

``` r
get_species(x)
```

## Arguments

- x:

  Camera trap data package object, as returned by
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

## Value

A tibble data frame with taxonomic information.

## Examples

``` r
x <- example_dataset()
get_species(x)
#> Warning: `get_species()` was deprecated in camtraptor 1.0.0.
#> ℹ Please use `taxa()` instead.
#> # A tibble: 10 × 5
#>    scientificName     taxonID  taxonRank vernacularNames.eng vernacularNames.nld
#>    <chr>              <chr>    <chr>     <chr>               <chr>              
#>  1 Anas platyrhynchos https:/… species   mallard             wilde eend         
#>  2 Anas strepera      https:/… species   gadwall             krakeend           
#>  3 Ardea              https:/… genus     great herons        reigers            
#>  4 Ardea cinerea      https:/… species   grey heron          blauwe reiger      
#>  5 Aves               https:/… class     bird sp.            vogel              
#>  6 Homo sapiens       https:/… species   human               mens               
#>  7 Martes foina       https:/… species   beech marten        steenmarter        
#>  8 Mustela putorius   https:/… species   European polecat    bunzing            
#>  9 Rattus norvegicus  https:/… species   brown rat           bruine rat         
#> 10 Vulpes vulpes      https:/… species   red fox             vos                
```
