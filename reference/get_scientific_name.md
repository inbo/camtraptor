# Get scientific name for vernacular name

Gets the scientific name for one or more vernacular names based on the
taxonomic information found in the metadata (`package$taxonomic`). The
match is performed case insensitively. If a vernacular name is not
valid, an error is returned

## Usage

``` r
get_scientific_name(
  package = NULL,
  vernacular_name,
  datapkg = lifecycle::deprecated()
)
```

## Arguments

- package:

  Camera trap data package object, as returned by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- vernacular_name:

  Character vector with input vernacular name(s).

- datapkg:

  Deprecated. Use `package` instead.

## Value

Character vector of scientific name(s).

## See also

Other exploration functions:
[`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md),
[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md),
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md),
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md),
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md),
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md),
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md),
[`get_species()`](https://inbo.github.io/camtraptor/reference/get_species.md)

## Examples

``` r
# One or more vernacular names
get_scientific_name(mica, "beech marten")
#> [1] "Martes foina"
get_scientific_name(mica, c("beech marten", "mallard"))
#> [1] "Martes foina"       "Anas platyrhynchos"

# Vernacular names can be passed in different languages
get_scientific_name(mica, c("beech marten", "wilde eend"))
#> [1] "Martes foina"       "Anas platyrhynchos"

# Search is performed case insensitively
get_scientific_name(mica, c("MaLLarD"))
#> [1] "Anas platyrhynchos"

if (FALSE) { # \dontrun{
# An error is returned if at least one invalid vernacular name is passed
get_scientfic_name(mica, "this is a bad vernacular name")

# A scientific name is an invalid vernacular name of course
get_scientific_name(mica, c("Castor fiber", "wilde eend"))
} # }
```
