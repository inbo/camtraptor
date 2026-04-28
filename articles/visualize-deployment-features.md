# Visualize deployment features

This vignette shows how to use function
[`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md) to
visualize important deployment features on a leaflet map:

- number of identified species
- number of observations
- RAI (Relative Abundance Index)
- effort (duration of a deployment)

## Setup

Load packages:

``` r
library(camtraptor)
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

This load automatically a camera trap data package, `mica`, containing
camera trap data about musk rat and coypu. We will use this variable
from now on.

## Get taxonomic information

As some features must or could be used in combination with a species
name, it’s sometimes useful to have an idea first about which species
have been detected and the correspondent vernacular names if present:

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

## Create maps

### Basic usage

#### Number of species

You can visualize the number of species detected by each deployment by
using the function
[`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md)
with `feature` argument set to `n_species`:

``` r
map_dep(mica,
        feature = "n_species")
```

#### Number of observations

To visualize the number of observations, set `feature` = `n_obs`:

``` r
map_dep(mica,
        feature = "n_obs")
```

You can also specify which species you want to calculate the number of
observations:

``` r
map_dep(mica,
        feature = "n_obs",
        species = "Anas platyrhynchos")
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

Notice how zero values are also visualized by a specific icon for ease
detection. By default a black multiplication symbol `×`.

You can filter by sex:

``` r
map_dep(
  mica,
  "n_obs",
  species = "Anas platyrhynchos",
  sex = c("female", "unknown")
)
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

and life stage:

``` r
map_dep(
  mica,
  "n_obs",
  life_stage = c("unknown", "subadult")
)
#> There are 2 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

#### Number of individuals

To visualize the number of observed individuals, set `feature` =
`n_individuals`:

``` r
map_dep(mica,
        feature = "n_individuals")
```

As for observations, you can specify a species:

``` r
map_dep(mica,
        feature = "n_individuals",
        species = "Anas platyrhynchos")
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

and filter by sex and/or life stage:

``` r
map_dep(
  mica,
  "n_individuals",
  species = "Anas platyrhynchos",
  sex = c("female", "unknown")
)
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

#### RAI

To visualize the Relative Abundance Index (RAI) for a species, set
`feature` = `rai` and specify a species using its scientific name:

``` r
map_dep(mica,
        feature = "rai",
        species = "Anas platyrhynchos")
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

**Notice that in this package the RAI is normalized over a deployment
activity period of 100 days.**

As for number of observations and number of individuals, you can filter
by sex and/or life stage:

``` r
map_dep(
  mica,
  "rai",
  species = "Anas platyrhynchos",
  sex = c("female", "unknown")
)
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

Common names are allowed as values of `species` as well:

``` r
map_dep(mica,
        feature = "rai",
        species = "great herons")
#> Scientific name of great herons: Ardea
#> There are 3 deployments without observations: 29b7d356-4bb4-4ec4-b792-2af5cc32efa8, 577b543a-2cf1-4b23-b6d2-cda7e2eac372 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

Values of `species` are also interpreted case insensitive:

``` r
map_dep(mica,
        feature = "rai",
        species = "CastoR FIBer")
#> There are 3 deployments without observations: 29b7d356-4bb4-4ec4-b792-2af5cc32efa8, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

If `species` is not specified or is wrong, an informative error message
listing all valid values is returned:

``` r
map_dep(mica,
          feature = "rai",
          species = "This is not a species name")
#> Error:
#> ! Invalid value for species parameter: this is not a species name.
#> Valid inputs are: anas platyrhynchos, anas strepera, ardea, ardea cinerea, castor fiber, homo sapiens, martes foina, mustela putorius, vulpes vulpes, mallard, gadwall, great herons, grey heron, eurasian beaver, human, beech marten, european polecat, red fox, wilde eend, krakeend and others...
```

#### RAI (individuals)

You can also visualize the RAI based on number of detected individuals
instead of the standard RAI which is based on the number of
observations. Set `feature = "rai_individuals"`:

``` r
map_dep(mica,
        feature = "rai_individuals",
        species = "Anas platyrhynchos")
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

Everything described in previous section about visualizing RAI holds
true for RAI based on individuals as well.

#### Effort

Visualize duration of the deployments, also called *effort*, as number
of active hours:

``` r
map_dep(mica, 
        feature = "effort",
        effort_unit = "hour")
```

The same using days as time unit

``` r
map_dep(mica, 
        feature = "effort",
        effort_unit = "day")
```

or months

``` r
map_dep(mica, 
        feature = "effort",
        effort_unit = "month")
```

### Clustering and hovering

You can specify which information you want to show while hovering with
the mouse over the deployment. You can choose among all columns from
deployments (see allowed fields in [camera trap data package standard
documentation](https://tdwg.github.io/camtrap-dp/data/#deployments)) and
`n` (number of species, number of observations or RAI).

Here below the lat/lon, the camera height and the tags are shown while
hovering:

``` r
map_dep(mica,
        hover_columns = c("latitude",
                          "longitude",
                          "cameraHeight",
                          "tags"),
        feature = "n_obs"
)
```

Deactivating both cluster mode and hovering is also possible:

``` r
map_dep(mica,
        feature = "n_species", 
        cluster = FALSE,
        hover_columns = NULL)
```

### Visualize deployments without detected animals

It can happen that some deployments didn’t detected any recognizable
animal (`scientificName` = `NA`) or didn’t observe anything at all
(deployments with no observations). While visualizing the number of
species, these two situations are shown by default as red and black
multiplication symbols `×` respectively. A message about deployment with
zero observations is also returned to the R console:

``` r
# create data package with one deployment with 0 obs and one delpoyment with
# observations of unknown species
unknown_species_vs_no_obs <- mica
unknown_species_vs_no_obs$data$observations <- 
  unknown_species_vs_no_obs$data$observations %>% 
  # a deployment has detected only unknown species
  filter(is.na(.data$scientificName) | 
           .data$scientificName != "Homo sapiens") %>%
  # a deployment has no observations
  filter(deploymentID != "62c200a9-0e03-4495-bcd8-032944f6f5a1")
# create new map
map_dep(unknown_species_vs_no_obs,
        feature = "n_species")
#> There are 1 deployments without observations: 62c200a9-0e03-4495-bcd8-032944f6f5a1
```

### Use a color palette

The default color palette is a [viridis color
palette](https://cran.microsoft.com/snapshot/2017-08-01/web/packages/viridis/vignettes/intro-to-viridis.html)
called `"inferno"`. You can specify another viridis color palette,
e.g. `"viridis"` or `"magma"`, or a
[RColorBrewer](https://renenyffenegger.ch/notes/development/languages/R/packages/RColorBrewer/index)
palette, e.g. `"BuPu"` or `"Oranges"`. Below we use the `viridis` color
palette:

``` r
map_dep(
  mica,
  "n_obs",
  palette = "viridis"
)
```

We can use a palette from `RColorBrewer`, e.g. the `"BuPu"` palette:

``` r
map_dep(
  mica,
  "n_obs",
  palette = "BuPu"
)
```

Another easy way to specify a palette is to create it by passing a
vector of colors as names or hex colors,
e.g. `c("black", "blue", "#A3675F")`:

``` r
map_dep(
  mica,
  "n_obs",
  palette = c("black", "blue", "#A3675F")
)
```

### Use a specific icon and color for zero values

You can pass to `zero_value_icon_url` argument the URL to an icon and to
`zero_values_icon_size` the size in pixels of such icon. There are
several icon libraries you can choose from. A library with many free
icons is [icons8](https://icons8.com/). Here, an example with the icon
of Fry from Futurama animation series and icon size 50:

``` r
map_dep(
  mica,
  "n_obs",
  life_stage = "subadult",
  zero_values_icon_url = "https://img.icons8.com/color/48/000000/futurama-fry.png",
  zero_values_icon_size = 50
)
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

Typically the colour is part of the URL. Here below two examples where
we change the color of the default icon to green (2ECC71):

``` r
map_dep(
  mica,
  "n_obs",
  life_stage = "subadult",
  zero_values_icon_url = 
    "https://img.icons8.com/ios-glyphs/30/2ECC71/multiply.png"
)
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

or the INBO fuchsia (#C04384):

``` r
map_dep(
  mica,
  "n_obs",
  life_stage = "subadult",
  zero_values_icon_url = 
    "https://img.icons8.com/ios-glyphs/30/C04384/multiply.png"
)
#> There are 3 deployments without observations: 577b543a-2cf1-4b23-b6d2-cda7e2eac372, 62c200a9-0e03-4495-bcd8-032944f6f5a1 and 7ca633fa-64f8-4cfc-a628-6b0c419056d7
```

Modifying the default value (`"black"`) can be useful as the color of
deployments with zero values can be sometimes too similar to one of the
colors used in the palette.

### Modify circle size

You can also modify the upper and lower limit of the circle sizes by
specifying `radius_range` (default: `c(10,50`):

``` r
map_dep(mica,
        feature = "n_obs",
        radius_range = c(20, 150))
```

### Use absolute scale

By default the upper limit of color palette and radius are defined based
on the actual feature values. However, sometimes can be useful to set up
an absolute upper limit. This can be done by setting argument
`relative_scale` to `FALSE` and specifying the upper limit in
`max_scale`.

Upper limit lower than number of observations:

``` r
map_dep(mica,
        feature = "n_obs",
        relative_scale = FALSE,
        max_scale = 2)
```

Upper limit higher than number of observations:

``` r
map_dep(mica,
        feature = "n_obs",
        relative_scale = FALSE,
        max_scale = 50)
```

### Use filter predicates

You maybe would like to visualize deployment information for a subset of
deployments. To do this, you can use filter predicates. E.g. visualize
number of observations for the deployments with longitude equal or
higher than 5.6:

``` r
map_dep(mica,
        pred_gt("longitude", 5.6),
        feature = "n_obs")
#> df %>% dplyr::filter((longitude > 5.6))
```

More about filter predicates in [filter
predicates](https://inbo.github.io/camtraptor/articles/filter-predicates.html)
article.
