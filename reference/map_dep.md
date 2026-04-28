# Visualize deployments features

This function visualizes deployments features such as number of detected
species, number of observations and RAI on a dynamic map. The circle
size and colour are proportional to the mapped feature. Deployments
without observations are shown as gray circles and a message is
returned.

## Usage

``` r
map_dep(
  package = NULL,
  feature,
  ...,
  species = NULL,
  sex = NULL,
  life_stage = NULL,
  effort_unit = NULL,
  cluster = TRUE,
  hover_columns = c("n", "species", "deploymentID", "locationID", "locationName",
    "latitude", "longitude", "start", "end"),
  palette = "inferno",
  zero_values_show = TRUE,
  zero_values_icon_url = "https://img.icons8.com/ios-glyphs/30/000000/multiply.png",
  zero_values_icon_size = 10,
  na_values_show = TRUE,
  na_values_icon_url = "https://img.icons8.com/ios-glyphs/30/FA5252/multiply.png",
  na_values_icon_size = 10,
  relative_scale = TRUE,
  max_scale = NULL,
  radius_range = c(10, 50),
  datapkg = lifecycle::deprecated()
)
```

## Arguments

- package:

  Camera trap data package object, as returned by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- feature:

  Deployment feature to visualize. One of:

  - `n_species`: Number of identified species.

  - `n_obs`: Number of observations.

  - `n_individuals`: Number of individuals.

  - `rai`: Relative Abundance Index.

  - `rai_individuals`: Relative Abundance Index based on number of
    individuals.

  - `effort`: Effort (duration) of the deployment.

- ...:

  Filter predicates for subsetting deployments.

- species:

  Character with a scientific name. Required for `rai`, optional for
  `n_obs`. Default: `NULL`.

- sex:

  Character defining the sex class to filter on, e.g. `"female"`. If
  `NULL` (default) all observations of all sex classes are taken into
  account. Optional parameter for `n_obs` and `n_individuals`.

- life_stage:

  Character vector defining the life stage class to filter on, e.g.
  `"adult"` or `c("subadult", "adult")`. If `NULL` (default) all
  observations of all life stage classes are taken into account.
  Optional parameter for `n_obs` and `n_individuals`.

- effort_unit:

  Time unit to use while visualizing deployment effort (duration). One
  of:

  - `second`

  - `minute`

  - `hour`

  - `day`

  - `month`

  - `year` If `NULL` (default), the effort is returned in hours.

- cluster:

  Logical value indicating whether using the cluster option while
  visualizing maps. Default: `TRUE`.

- hover_columns:

  Character vector with the name of the columns to use for showing
  location deployment information on mouse hover. One or more from
  deployment columns. Use `NULL` to disable hovering. Default
  information:

  - `n`: Number of species, number of observations, RAI or effort
    (column created internally by a `get_*()` function).

  - `species`: Species name(s).

  - `start`: Start deployment.

  - `end`: End deployment.

  - `deploymentID`: Deployment unique identifier.

  - `locationID`: Location unique identifier.

  - `locationName`: Location name.

  - `latitude`

  - `longitude`

  See the [Deployment](https://camtrap-dp.tdwg.org/data/#deployments)
  section of Camtrap DP for the full list of columns you can use.

- palette:

  The palette name or the colour function that values will be mapped to.
  Typically one of the following:

  - A character vector of RGB or named colours. Examples:
    `c("#000000", "#0000FF", "#FFFFFF"))`,`topo.colors(10))`.

  - The full name of a RColorBrewer palette, e.g. "BuPu" or "Greens", or
    viridis palette: `"viridis"`, `"magma"`, `"inferno"` or `"plasma"`.
    For more options, see parameter `palette` of
    [`leaflet::colorNumeric()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html).

- zero_values_show:

  Logical indicating whether to show deployments with zero values.
  Default: `TRUE`.

- zero_values_icon_url:

  Character with URL to icon for showing deployments with zero values.
  Default: a cross (multiply symbol)
  `"https://img.icons8.com/ios-glyphs/30/000000/multiply.png"`.

- zero_values_icon_size:

  A number to set the size of the icon to show deployments with zero
  values. Default: 10.

- na_values_show:

  Logical indicating whether to show deployments with zero values.
  Notice that only feature `"n_species"` generates NA values. Default:
  `TRUE`.

- na_values_icon_url:

  Character with URL to icon for showing deployments with `NA` values.
  Notice that only feature `"n_species"` generates NA values. Default: a
  red cross (multiply symbol)
  `"https://img.icons8.com/ios-glyphs/30/FA5252/multiply.png"`.

- na_values_icon_size:

  A number to set the size of the icon to show deployments with `NA`
  values. Notice that only feature `"n_species"` generates NA values.
  Default: 10.

- relative_scale:

  Logical indicating whether to use a relative colour and radius scale
  (`TRUE`) or an absolute scale (`FALSE`). If absolute scale is used,
  specify a valid `max_scale`.

- max_scale:

  Number indicating the max value used to map colour and radius.

- radius_range:

  Vector of length 2 containing the lower and upper limit of the circle
  radius. The lower value is used for deployments with zero feature
  value, i.e. no observations, no identified species, zero RAI or zero
  effort. The upper value is used for the deployment(s) with the highest
  feature value (`relative_scale` = `TRUE`) or `max_scale`
  (`relative_scale` = `FALSE`). Default: `c(10, 50)`.

- datapkg:

  Deprecated. Use `package` instead.

## Value

Leaflet map.

## See also

Check documentation about filter predicates:
[`pred()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md),
[`pred_in()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md),
[`pred_and()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md),
...

## Examples

``` r
if (FALSE) { # \dontrun{
# Show number of species
map_dep(
  mica,
  "n_species"
)

# Show number of observations (observations of unidentified species included
# if any)
map_dep(
  mica,
  "n_obs"
)

# Show number of observations of Anas platyrhynchos
map_dep(
  mica,
  "n_obs",
  species = "Anas platyrhynchos"
)

# Show number of observations of subadult individuals of Anas strepera
map_dep(
  mica,
  "n_obs",
  species = "Anas strepera",
  life_stage = "subadult"
)

# Show number of observations of female or unknown individuals of gadwall
map_dep(
  mica,
  "n_obs",
  species = "gadwall",
  sex = c("female", "unknown")
)

# Show number of individuals (individuals of unidentified species included if
# any)
map_dep(
  mica,
  "n_individuals"
)

# Same filters by life stage and sex as for number of observations apply
map_dep(
  mica,
  "n_individuals",
  species = "Anas strepera",
  sex = "female",
  life_stage = "adult"
)

# Show RAI
map_dep(
  mica,
  "rai",
  species = "Anas strepera"
)

# Same filters by life_stage and sex as for number of observations apply
map_dep(
  mica,
  "rai",
  species = "Anas strepera",
  sex = "female",
  life_stage = "adult"
)

# Show RAI calculated by using number of detected individuals
map_dep(
  mica,
  "rai_individuals",
  species = "Anas strepera"
)

# Same filters by life stage and sex as for basic RAI apply
map_dep(
  mica,
  "rai_individuals",
  species = "Anas strepera",
  sex = "female",
  life_stage = "adult"
)

# Show effort (hours)
map_dep(
  mica,
  "effort"
)
# Show effort (days)
map_dep(
  mica,
  "effort",
  effort_unit = "day"
)

# Use viridis palette (viridis palettes)
map_dep(
  mica,
  "n_obs",
  palette = "viridis"
)

# Use "BuPu" colour palette (RColorBrewer palettes)
map_dep(
  mica,
  "n_obs",
  palette = "BuPu"
)

# Use a palette defined by colour names
map_dep(
  mica,
  "n_obs",
  palette = c("black", "blue", "white")
)

# Use a palette defined by hex colours
map_dep(
  mica,
  "n_obs",
  palette = c("#000000", "#0000FF", "#FFFFFF")
)

# Do not show deployments with zero values
map_dep(
  mica,
  "n_obs",
  life_stage = "subadult",
  zero_values_show = FALSE
)

# Use same icon but but a non default colour for zero values deployments,
# e.g. red (hex: E74C3C)
map_dep(
  mica,
  "n_obs",
  life_stage = "subadult",
  zero_values_icon_url = "https://img.icons8.com/ios-glyphs/30/E74C3C/multiply.png"
)

# ... or yellow (F1C40F)
map_dep(
  mica,
  "n_obs",
  life_stage = "subadult",
  zero_values_icon_url = "https://img.icons8.com/ios-glyphs/30/F1C40F/multiply.png"
)

# Use another icon via a different URL, e.g. the character Fry from Futurama
# in green (2ECC71)
map_dep(
  mica,
  "n_obs",
  life_stage = "subadult",
  zero_values_icon_url = "https://img.icons8.com/ios-glyphs/30/2ECC71/futurama-fry.png"
)

# Same behavior for the icon visualizing NA values (`"n_species"` feature)
unknown_species_vs_no_obs <- mica
unknown_species_vs_no_obs$data$observations <- 
  unknown_species_vs_no_obs$data$observations %>% 
  # a deployment has detected only unknown species
  filter(is.na(.data$scientificName) | 
           .data$scientificName != "Homo sapiens") %>%
  # a deployment has no observations
  filter(deploymentID != "62c200a9-0e03-4495-bcd8-032944f6f5a1")
# create new map
map_dep(
  unknown_species_vs_no_obs,
  feature = "n_species",
  zero_values_icon_url = "https://img.icons8.com/ios-glyphs/30/2ECC71/futurama-fry.png",
  zero_values_icon_size = 60,
  na_values_icon_url = "https://img.icons8.com/ios-glyphs/30/E74C3C/futurama-fry.png",
  na_values_icon_size = 60
)

# Set size of the icon for zero values deployments
map_dep(
  mica,
  "n_obs",
  life_stage = "subadult",
  zero_values_icon_size = 30
)

# Disable cluster
map_dep(
  mica,
  "n_species",
  cluster = FALSE
)

# Show only number of observations and location name while hovering
map_dep(
  mica,
  "n_obs",
  hover_columns = c("locationName", "n")
)

# Use absolute scale for colours and radius
map_dep(mica,
  "n_species",
  relative_scale = FALSE,
  max_scale = 4
)

# Change max and min size circles
map_dep(
  mica,
  "n_obs",
  radius_range = c(40, 150)
)
} # }
```
