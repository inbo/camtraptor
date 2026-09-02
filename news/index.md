# Changelog

## camtraptor 1.0.0

This major release updates the internal data model of camtraptor to
Camtrap DP 1.0, drops support for Camtrap DP 0.1.6 and facilitates a
step-by-step exploration workflow with new functions.

### New workflow

camtraptor now offers a step-by-step workflow to explore and visualize
data:

1.  **Read** Camtrap DP files with
    [`read_camtrapdp()`](https://inbo.github.io/camtraptor/reference/read_camtrapdp.md)
    (reexported from
    [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html)).
    This function supports Camtrap DP 1.0 or higher.
2.  **Filter** the data with
    [`filter_deployments()`](https://inbo.github.io/camtraptor/reference/filter_deployments.md),
    [`filter_media()`](https://inbo.github.io/camtraptor/reference/filter_media.md)
    and
    [`filter_observations()`](https://inbo.github.io/camtraptor/reference/filter_observations.md)
    (also reexported from
    [camtrapdp](https://github.com/inbo/camtrapdp)). These functions
    replace the predicate functions (which only worked on deployments)
    and filter arguments in `get_` functions.
3.  **Summarize** deployments and observations with
    [`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md)
    and
    [`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md).
    These calculate features (e.g. `"effort_duration"` or `"n_events"`)
    grouped by fields and temporal levels of your choice.
4.  **Visualize** those summary tables using
    [`map_summary()`](https://inbo.github.io/camtraptor/reference/map_summary.md),
    which creates a Leaflet map for the desired feature. This function
    replaces
    [`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md).

Here’s an example where you read files, filter on coordinates and adult
animals, calculate observation-level summaries, and create a map showing
the number of individuals:

``` r

library(camtraptor)
file <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
x <- read_camtrapdp(file)
x %>%
  filter_deployments(latitude > 51.0, longitude > 5.0) %>%
  filter_observations(lifeStage == "adult") %>%
  summarize_observations() %>%
  map_summary(feature = "sum_count")
```

Note how you can stop and explore (all) the summary results returned by
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md)
before selecting one (`"sum_count"`) to visualize with
[`map_summary()`](https://inbo.github.io/camtraptor/reference/map_summary.md):

``` r

x %>%
  filter_deployments(latitude > 51.0, longitude > 5.0) %>%
  filter_observations(lifeStage == "adult") %>%
  summarize_observations()
#> # A tibble: 4 × 10
#> # Groups:   deploymentID, latitude, longitude, scientificName [4]
#>   deploymentID latitude longitude scientificName     n_scientificName n_events
#>   <chr>           <dbl>     <dbl> <chr>                         <int>    <int>
#> 1 29b7d356         51.2      5.66 Anas platyrhynchos                1        3
#> 2 577b543a         51.2      5.66 Martes foina                      1        1
#> 3 577b543a         51.2      5.66 Mustela putorius                  1        3
#> 4 577b543a         51.2      5.66 Vulpes vulpes                     1        1
#> # ℹ 4 more variables: n_observations <int>, sum_count <int>,
#> #   rai_observations <dbl>, rai_count <dbl>
```

Before, you had to use a specific `get_` function to see a specific
summary result. And you had to repeat the filters in
[`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md):

``` r

# Before
x <- read_camtrap_dp(file)
get_n_individuals(
  x,
  life_stage = "adult",
  pred_and(pred_gt("latitude", 51.0), pred_gt("longitude", 5.0))
)
map_dep(
  x,
  feature = "n_individuals",
  life_stage = "adult",
  pred_and(pred_gt("latitude", 51.0), pred_gt("longitude", 5.0))
)
```

More details about the new workflow can be found in the vignette
[`vignette("workflow")`](https://inbo.github.io/camtraptor/articles/workflow.md).

### New functions

- New
  [`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md)
  calculates the duration effort. Users can define the grouping field(s)
  (e.g. `"locationID"`) and temporal level (e.g. `"month"`)
  ([\#366](https://github.com/inbo/camtraptor/issues/366)).
- New
  [`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md)
  calculates several observation-related features, such as the number of
  taxa, the number of events, the number of observations, the sum of
  individual counts and the Relative Abundance Index (RAI). Users can
  define the grouping field(s) and temporal level
  ([\#367](https://github.com/inbo/camtraptor/issues/367)).
- New
  [`map_summary()`](https://inbo.github.io/camtraptor/reference/map_summary.md)
  creates a Leaflet map showing a selected feature from a summary table
  generated by
  [`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md)
  or
  [`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md).
- New
  [`filter_out_timelapse()`](https://inbo.github.io/camtraptor/reference/filter_out_timelapse.md)
  allows to filter out timelapse observations
  ([\#306](https://github.com/inbo/camtraptor/issues/306)).
- New
  [`add_coordinates()`](https://inbo.github.io/camtraptor/reference/add_coordinates.md)
  adds the deployment coordinates to observations
  ([\#350](https://github.com/inbo/camtraptor/issues/350)).

The following functions are reexported from
[camtrapdp](https://github.com/inbo/camtrapdp):

- Filter functions:
  [`filter_deployments()`](https://inbo.github.io/camtraptor/reference/filter_deployments.md),
  [`filter_media()`](https://inbo.github.io/camtraptor/reference/filter_media.md)
  and
  [`filter_observations()`](https://inbo.github.io/camtraptor/reference/filter_observations.md).
  They return a data package with filtered data
  ([\#315](https://github.com/inbo/camtraptor/issues/315)).
- Get data functions:
  [`deployments()`](https://inbo.github.io/camtraptor/reference/deployments.md),
  [`media()`](https://inbo.github.io/camtraptor/reference/media.md),
  [`observations()`](https://inbo.github.io/camtraptor/reference/observations.md),
  [`locations()`](https://inbo.github.io/camtraptor/reference/locations.md),
  [`events()`](https://inbo.github.io/camtraptor/reference/events.md),
  [`taxa()`](https://inbo.github.io/camtraptor/reference/taxa.md),
  [`individuals()`](https://inbo.github.io/camtraptor/reference/individuals.md),
  and
  [`contributors()`](https://inbo.github.io/camtraptor/reference/contributors.md).
  They return a data frame
  ([\#317](https://github.com/inbo/camtraptor/issues/317)).
- Assign data functions: `deployments<-`, `media<-`, `observations<-`,
  `contributors<-`. They assign a data frame back to the data package
  and should be used with care, as it can invalidate a data package
  ([\#328](https://github.com/inbo/camtraptor/issues/328)).
- Example data:
  [`example_dataset()`](https://inbo.github.io/camtraptor/reference/example_dataset.md).
  It returns a minimal example data package (from the Camtrap DP
  standard) and replaces the `mica` dataset.

### Superseded functionality

The following functions still work, but are superseded in favor of
correspondent [camtrapR](https://github.com/jniedballa/camtrapR)
functions:

| camtraptor | camtrapR |
|----|----|
| [`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md) | [`camtrapR::cameraOperation()`](https://jniedballa.github.io/camtrapR/reference/cameraOperation.html) ([\#419](https://github.com/inbo/camtraptor/issues/419)) |
| [`get_detection_history()`](https://inbo.github.io/camtraptor/reference/get_detection_history.md) | [`camtrapR::detectionHistory()`](https://jniedballa.github.io/camtrapR/reference/detectionHistory.html) ([\#419](https://github.com/inbo/camtraptor/issues/419)) |
| [`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md) | [`camtrapR::recordTable()`](https://jniedballa.github.io/camtrapR/reference/recordTable.html) ([\#419](https://github.com/inbo/camtraptor/issues/419)) |

### Deprecated functionality

The following functions still work for now, but **will be removed in a
future version**:

| Before | Now |
|----|----|
| `animal_pos` | `animal_positions`, with the column `"sequenceID"` renamed to `"eventID"` ([\#402](https://github.com/inbo/camtraptor/issues/402), [\#242](https://github.com/inbo/camtraptor/issues/242)) |
| [`calc_animal_pos()`](https://inbo.github.io/camtraptor/reference/calc_animal_pos.md) | [`calculate_individual_radius_angle()`](https://inbo.github.io/camtraptor/reference/calculate_individual_radius_angle.md) ([\#242](https://github.com/inbo/camtraptor/issues/242)) |
| `calib_models` | `calibration_models` ([\#402](https://github.com/inbo/camtraptor/issues/402), [\#242](https://github.com/inbo/camtraptor/issues/242)) |
| [`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md) | [`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md), column `"effort_duration"` ([\#366](https://github.com/inbo/camtraptor/issues/366)) |

[`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md)
\|
[`summarize_deployments()`](https://inbo.github.io/camtraptor/reference/summarize_deployments.md),
column `"effort_duration"`
([\#366](https://github.com/inbo/camtraptor/issues/366))
[`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md)
\|
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md),
column `"sum_count"`
([\#367](https://github.com/inbo/camtraptor/issues/367))
[`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md)
\|
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md),
column `"n_observations"`
([\#367](https://github.com/inbo/camtraptor/issues/367))
[`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md)
\|
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md),
column `"n_scientificName"`
([\#243](https://github.com/inbo/camtraptor/issues/243))
[`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md) \|
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md),
column `"rai_observations"`
([\#243](https://github.com/inbo/camtraptor/issues/243))
[`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md)
\|
[`summarize_observations()`](https://inbo.github.io/camtraptor/reference/summarize_observations.md),
column `"rai_count"`
([\#243](https://github.com/inbo/camtraptor/issues/243))
[`get_species()`](https://inbo.github.io/camtraptor/reference/get_species.md)
\| [`taxa()`](https://inbo.github.io/camtraptor/reference/taxa.md)
([\#343](https://github.com/inbo/camtraptor/issues/343))
[`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md) \|
[`map_summary()`](https://inbo.github.io/camtraptor/reference/map_summary.md)
([\#231](https://github.com/inbo/camtraptor/issues/231),
[\#91](https://github.com/inbo/camtraptor/issues/91)) `mica` \|
[`example_dataset()`](https://inbo.github.io/camtraptor/reference/example_dataset.md)
([\#402](https://github.com/inbo/camtraptor/issues/402))
[`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md)
\|
[`read_camtrapdp()`](https://inbo.github.io/camtraptor/reference/read_camtrapdp.md)
(without underscore)
([\#298](https://github.com/inbo/camtraptor/issues/298))

The following functions and objects **no longer work**:

| Before | Now |
|----|----|
| [`apply_filter_predicate()`](https://inbo.github.io/camtraptor/reference/defunct.md) | This helper function is no longer needed ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`check_species()`](https://inbo.github.io/camtraptor/reference/defunct.md) | [`taxa()`](https://inbo.github.io/camtraptor/reference/taxa.md) to return all taxa ([\#235](https://github.com/inbo/camtraptor/issues/235)) |
| [`get_scientific_name()`](https://inbo.github.io/camtraptor/reference/defunct.md) | [`taxa()`](https://inbo.github.io/camtraptor/reference/taxa.md) to return all taxa ([\#235](https://github.com/inbo/camtraptor/issues/235)) |
| [`pred()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(locationID == "e254a13c")` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_not()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(locationID != "e254a13c")` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_gt()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(x, latitude > 51)` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_gte()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(x, latitude >= 51)` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_lt()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(x, latitude < 51)` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_lte()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(x, latitude <= 51)` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_in()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(x, locationID %in% c("e254a13c", "2df5259b"))` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_notin()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(x, !locationID %in% c("e254a13c", "2df5259b"))` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_na()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(x, is.na(setupBy))` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_notna()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(x, !is.na(setupBy))` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_and()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(x, cameraHeight >= 1.0 & cameraHeading >= 100)` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`pred_or()`](https://inbo.github.io/camtraptor/reference/defunct.md) | `filter_deployments(x, cameraHeight >= 1.0 | cameraHeading >= 100)` ([\#316](https://github.com/inbo/camtraptor/issues/316)) |
| [`read_wi()`](https://inbo.github.io/camtraptor/reference/defunct.md) | Removed as it is out of scope. Wildlife Insights will provide Camtrap DP export functionality ([\#351](https://github.com/inbo/camtraptor/issues/351)). |
| [`round_coordinates()`](https://inbo.github.io/camtraptor/reference/defunct.md) | Moved to [`camtrapdp::round_coordinates()`](https://inbo.github.io/camtrapdp/reference/round_coordinates.html) ([\#327](https://github.com/inbo/camtraptor/issues/327)) |
| [`write_dwc()`](https://inbo.github.io/camtraptor/reference/defunct.md) | Moved to [`camtrapdp::write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.html) |
| [`write_eml()`](https://inbo.github.io/camtraptor/reference/defunct.md) | Moved to [`camtrapdp::write_eml()`](https://inbo.github.io/camtrapdp/reference/write_eml.html) |

The following arguments **have or will be removed**:

| Before | Now |
|----|----|
| `sex` in [`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md) and `get_` functions | Use `filter_observations(sex = "female")` first |
| `life_stage` in [`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md) and `get_` functions | Use `filter_observations(lifeStage = "adult")` first |
| `datapkg` in `read_camptrap_dp()` | `read_camtrapdp(x)` |
| `package` as the first argument in many functions | Renamed to `x` ([\#324](https://github.com/inbo/camtraptor/issues/324)) |

### Other changes

- `CITATION.cff` file has been added
  ([\#345](https://github.com/inbo/camtraptor/issues/345)).
- Sanne Govaert and Wolf Missotten have been added as authors.

## camtraptor 0.28.0

- [`get_detection_history()`](https://inbo.github.io/camtraptor/reference/get_detection_history.md)
  calculates the detection history based on a record table and a camera
  operation matrix. Some analogies with
  [`camtrapR::detectionHistory`](https://jniedballa.github.io/camtrapR/reference/detectionHistory.html)
  ([\#360](https://github.com/inbo/camtraptor/issues/360)).

## camtraptor 0.27.0

- [`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)
  returns now 4 new columns: `longitude`, `latitude` (deployment
  coordinates), `clock` (clock time of the observation in radians) and
  `solar` (sun time of the observation in radians)
  ([\#341](https://github.com/inbo/camtraptor/issues/341)).

## camtraptor 0.26.0

- [`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md)
  returns now the effort for each deployment separately
  ([\#333](https://github.com/inbo/camtraptor/issues/333)). The returned
  data frame has two new columns: `deploymentID` and `locationName`.

## camtraptor 0.25.0

- [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md)
  detects Camtrap DP version from `package$profile` using regex
  ([\#295](https://github.com/inbo/camtraptor/issues/295)). This
  supports reading Camtrap DPs created by the GBIF IPT.

## camtraptor 0.24.0

- Replicate old Camtrap DP 0.1.6 behaviour and populate `angle` and
  `radius` for event-based observations. Values are taken from the first
  media-based observation (fields `individualPositionRadius` and
  `individualPositionAngle`) for each `eventID/individualID` combination
  ([\#291](https://github.com/inbo/camtraptor/issues/291)).

## camtraptor 0.23.0

- Fix bug in
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md)
  when reading a Camtrap DP 1.0
  ([\#292](https://github.com/inbo/camtraptor/issues/292)).

## camtraptor 0.22.0

- Fix bug in
  [`write_eml()`](https://inbo.github.io/camtraptor/reference/defunct.md)
  for Camtrap DP 1.0 datasets
  ([\#290](https://github.com/inbo/camtraptor/issues/290)).
- [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md)
  will now always populate `taxonID` from the `package.taxonomy`
  ([\#290](https://github.com/inbo/camtraptor/issues/290)).

## camtraptor 0.21.0

- [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md)
  supports Camtrap DP 1.0 (upcoming Agouti export format) in favour of
  Camtrap DP 1.0-rc.1
  ([\#284](https://github.com/inbo/camtraptor/issues/284)). To avoid
  breaking changes to users, it will down-convert Camtrap DP 1.0 to
  0.1.6 which is currently used as internal data model for camtraptor.
- [`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md)
  now calculates per calendar month/week
  ([\#219](https://github.com/inbo/camtraptor/issues/219)).
- [`write_dwc()`](https://inbo.github.io/camtraptor/reference/defunct.md)
  has an updated mapping for dwc_audubon.csv
  ([\#274](https://github.com/inbo/camtraptor/issues/274)).
- [`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)
  returns the number of observed individuals
  ([\#279](https://github.com/inbo/camtraptor/issues/279)).
- [`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md)
  allows to add session and camera IDs to the station names output
  ([\#288](https://github.com/inbo/camtraptor/issues/288)).
