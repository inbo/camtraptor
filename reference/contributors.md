# Get or set contributors

Re-export of
[`camtrapdp::contributors()`](https://inbo.github.io/camtrapdp/reference/contributors.html).

## Usage

``` r
contributors(x)

contributors(x) <- value
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

- value:

  A data frame to assign as contributors.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with the contributors, containing the following columns
(columns absent in `x$contributors` will be created):

- `title`

- `firstName`: if absent, this will be set to the first word in `title`,
  except if it is a single word or the `role` is `rightsHolder` or
  `publisher`.

- `lastName`: if absent, this will be set to the remaining words in
  `title`, with the same exceptions as `firstName`.

- `email`

- `path`

- `role`

- `organization`

## See also

Other accessor functions:
[`deployments()`](https://inbo.github.io/camtraptor/reference/deployments.md),
[`events()`](https://inbo.github.io/camtraptor/reference/events.md),
[`individuals()`](https://inbo.github.io/camtraptor/reference/individuals.md),
[`locations()`](https://inbo.github.io/camtraptor/reference/locations.md),
[`media()`](https://inbo.github.io/camtraptor/reference/media.md),
[`observations()`](https://inbo.github.io/camtraptor/reference/observations.md),
[`taxa()`](https://inbo.github.io/camtraptor/reference/taxa.md)

## Examples

``` r
x <- example_dataset()
# Get contributors
contributors(x)
#> # A tibble: 6 × 7
#>   title                        firstName lastName email path  role  organization
#>   <chr>                        <chr>     <chr>    <chr> <chr> <chr> <chr>       
#> 1 Axel Neukermans              Axel      Neukerm… axel… http… cont… Research In…
#> 2 Danny Van der beeck          Danny     Van der… dani… NA    NA    NA          
#> 3 Emma Cartuyvels              Emma      Cartuyv… emma… NA    prin… Research In…
#> 4 Peter Desmet                 Peter     Desmet   pete… http… cont… Research In…
#> 5 Research Institute for Natu… NA        NA       NA    http… righ… NA          
#> 6 Research Institute for Natu… NA        NA       NA    http… publ… NA          

# Set contributors
contributors(x) <- head(contributors(x), 1)
```
