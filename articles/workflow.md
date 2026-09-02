# Workflow

Camtraptor is an R package to **explore** and **visualize** [Camera Trap
Data Package](https://camtrap-dp.tdwg.org/) (Camtrap DP) datasets. This
vignette walks you through the workflow of the package.

Coming from `camtraptor` version 0.28 or earlier? Check out
[News](https://inbo.github.io/camtraptor/news/index.html#camtraptor-100)
to see what changed. Deprecated functions will throw a warning pointing
you towards their replacements.

## Setup

Load the package:

``` r

library(camtraptor)
#> 
#> Attaching package: 'camtraptor'
#> The following object is masked from 'package:base':
#> 
#>     contributors
```

## Workflow

### Reading data

To start your data analysis you first need data.

Here, the function
[`example_dataset()`](https://inbo.github.io/camtraptor/reference/example_dataset.md)
is used to load an example Camera Trap Data Package dataset that is
included in the camtraptor package. This dataset is derived from a study
on detecting invasive muskrat and coypu populations using camera traps.

``` r

x <- example_dataset()
```

To read your own locally stored dataset, use
[`read_camtrapdp()`](https://inbo.github.io/camtraptor/reference/read_camtrapdp.md):

``` r

x <- read_camtrapdp("path/to/datapackage.json")
```

In this vignette the example dataset will be used.

### Exploring data

Now that you read in your data, you can start exploring it. A Camtrap DP
dataset consists of three tables: `deployments`, `media` and
`observations`. For more details on the data structure, see the [Camtrap
DP website](https://camtrap-dp.tdwg.org/data/). You can access each
table directly by using
[`deployments()`](https://inbo.github.io/camtraptor/reference/deployments.md),
[`media()`](https://inbo.github.io/camtraptor/reference/media.md) and
[`observations()`](https://inbo.github.io/camtraptor/reference/observations.md).
Let’s take a look at the deployments table:

``` r

deployments(x)
#> # A tibble: 4 × 24
#>   deploymentID locationID locationName  latitude longitude coordinateUncertainty
#>   <chr>        <chr>      <chr>            <dbl>     <dbl>                 <dbl>
#> 1 00a2c20d     e254a13c   B_HS_val 2_p…     51.5      4.77                   187
#> 2 29b7d356     2df5259b   B_DL_val 5_b…     51.2      5.66                   187
#> 3 577b543a     ff1535c0   B_DL_val 3_d…     51.2      5.66                   187
#> 4 62c200a9     ce943ced   B_DM_val 4_'…     50.7      4.01                   187
#> # ℹ 18 more variables: deploymentStart <dttm>, deploymentEnd <dttm>,
#> #   setupBy <chr>, cameraID <chr>, cameraModel <chr>, cameraDelay <dbl>,
#> #   cameraHeight <dbl>, cameraDepth <dbl>, cameraTilt <dbl>,
#> #   cameraHeading <dbl>, detectionDistance <dbl>, timestampIssues <lgl>,
#> #   baitUse <lgl>, featureType <fct>, habitat <chr>, deploymentGroups <chr>,
#> #   deploymentTags <chr>, deploymentComments <chr>
```

To get a quick overview of which species were recorded, use
[`taxa()`](https://inbo.github.io/camtraptor/reference/taxa.md):

``` r

taxa(x)
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

### Filtering data

Now that you have explored the dataset, you can start filtering. Use
[`filter_deployments()`](https://inbo.github.io/camtraptor/reference/filter_deployments.md)
to select specific deployment locations and
[`filter_observations()`](https://inbo.github.io/camtraptor/reference/filter_observations.md)
to select specific species. For example, filtering for two specific
locations and mallard (*Anas platyrhynchos*):

``` r

x_filtered <- x %>%
  filter_deployments(
    locationName == c(
      "B_HS_val 2_processiepark",
      "B_DM_val 4_'t WAD"
      )
    ) %>%
  filter_observations(scientificName == "Anas platyrhynchos")
```

[`filter_deployments()`](https://inbo.github.io/camtraptor/reference/filter_deployments.md)
filters the deployments table to the two selected locations, and
automatically removes any associated observations and media that do not
belong to these deployments.
[`filter_observations()`](https://inbo.github.io/camtraptor/reference/filter_observations.md)
then filters the remaining observations to only keep mallard (*Anas
platyrhynchos*) observations.

Want to filter out timelapse observations? Use
[`filter_out_timelapse()`](https://inbo.github.io/camtraptor/reference/filter_out_timelapse.md)
as a shortcut for
`filter_observations(x, captureMethod != "timelapse")`.

### Summarizing data

Before visualizing, the data first needs to be summarized.

Use
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md)
to get an overview of the observations per deployment:

``` r

summ_obs_filtered <- summarize_observations(x_filtered)
summ_obs_filtered
#> # A tibble: 1 × 10
#> # Groups:   deploymentID, latitude, longitude, scientificName [1]
#>   deploymentID latitude longitude scientificName     n_scientificName n_events
#>   <chr>           <dbl>     <dbl> <chr>                         <int>    <int>
#> 1 00a2c20d         51.5      4.77 Anas platyrhynchos                1        6
#> # ℹ 4 more variables: n_observations <int>, sum_count <int>,
#> #   rai_observations <dbl>, rai_count <dbl>
```

Use
[`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md)
to get an overview of the deployments:

``` r

summ_depl_filtered <- summarize_deployments(x_filtered)
summ_depl_filtered
#> # A tibble: 2 × 4
#> # Groups:   deploymentID, latitude, longitude [2]
#>   deploymentID latitude longitude effort_duration       
#>   <chr>           <dbl>     <dbl> <Duration>            
#> 1 00a2c20d         51.5      4.77 2789044s (~4.61 weeks)
#> 2 62c200a9         50.7      4.01 1903602s (~3.15 weeks)
```

### Visualizing data

Use
[`map_summary()`](https://inbo.github.io/camtraptor/reference/map_summary.md)
to visualize a summary on a map.

Visualize the number of mallard observations per deployment:

``` r

map_summary(summ_obs_filtered, feature = "n_observations")
```

As you can see, in only one of the two deployments was mallard observed.

You can also visualize the daily effort per deployment:

``` r

map_summary(summ_depl_filtered, feature = "effort_duration")
```

### Putting it all together

In practice you’d chain everything into a single pipeline, e.g.:

``` r

x %>%
  filter_deployments(
    locationName == c(
      "B_HS_val 2_processiepark",
      "B_DM_val 4_'t WAD"
      )
    ) %>%
  filter_observations(scientificName == "Anas platyrhynchos") %>%
  summarize_observations() %>%
  map_summary(feature = "n_observations")
```

For more information about visualization, check out the vignette
[`vignette("visualize-deployment-features")`](https://inbo.github.io/camtraptor/articles/visualize-deployment-features.md).
