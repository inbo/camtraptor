# Check scientific or vernacular name(s)

Checks if a given scientific or vernacular name(s) can be found in the
metadata (`package$taxonomic`) and returns error if not.

## Usage

``` r
check_species(
  package = NULL,
  species,
  arg_name = "species",
  datapkg = lifecycle::deprecated()
)
```

## Arguments

- package:

  Camera trap data package object, as returned by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- species:

  Character vector with scientific or vernacular names.

- arg_name:

  Character with argument name to return in error message Default:
  "species".

- datapkg:

  Deprecated. Use `package` instead.

## Value

A character vector with the correspondent scientific names.

## Examples

``` r
# Species is a scientific name
check_species(mica, "Martes foina")
#> [1] "Martes foina"

# Species is a vector of vernacular names
check_species(mica, c("beech marten", "european polecat"))
#> Scientific name of beech marten: Martes foina
#> Scientific name of european polecat: Mustela putorius
#> [1] "Martes foina"     "Mustela putorius"

# Vernacular names can be specified in any language available
check_species(mica, c("vos", "blauwe reiger"))
#> Scientific name of vos: Vulpes vulpes
#> Scientific name of blauwe reiger: Ardea cinerea
#> [1] "Vulpes vulpes" "Ardea cinerea"

# Vernacular names and scientific names can be mixed up
check_species(mica, c("beech marten", "blauwe reiger", "Anas strepera"))
#> Scientific name of beech marten: Martes foina
#> Scientific name of blauwe reiger: Ardea cinerea
#> [1] "Martes foina"  "Ardea cinerea" "Anas strepera"

# Case insensitive
check_species(mica, "AnaS StrePeRa")
#> [1] "Anas strepera"
check_species(mica, "bEEch mARteN")
#> Scientific name of bEEch mARteN: Martes foina
#> [1] "Martes foina"

if (FALSE) { # \dontrun{
check_species(mica, "bad name")
} # }
```
