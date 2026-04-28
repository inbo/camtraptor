# Package index

## Read data

- [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md)
  : Read a Camtrap DP
- [`read_wi()`](https://inbo.github.io/camtraptor/reference/read_wi.md)
  : Read a Wildlife Insights export

## Filter data

- [`apply_filter_predicate()`](https://inbo.github.io/camtraptor/reference/apply_filter_predicate.md)
  : Intermediate function to apply filter predicates on a data frame
- [`pred()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_not()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_gt()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_gte()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_lt()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_lte()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_in()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_notin()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_na()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_notna()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_and()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  [`pred_or()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
  : Filter predicate

## Explore data

- [`get_custom_effort()`](https://inbo.github.io/camtraptor/reference/get_custom_effort.md)
  : Get custom effort
- [`get_effort()`](https://inbo.github.io/camtraptor/reference/get_effort.md)
  : Get effort
- [`get_n_individuals()`](https://inbo.github.io/camtraptor/reference/get_n_individuals.md)
  : Get number of individuals for each deployment
- [`get_n_obs()`](https://inbo.github.io/camtraptor/reference/get_n_obs.md)
  : Get number of observations for each deployment
- [`get_n_species()`](https://inbo.github.io/camtraptor/reference/get_n_species.md)
  : Get number of identified species for each deployment
- [`get_rai()`](https://inbo.github.io/camtraptor/reference/get_rai.md)
  : Get Relative Abundance Index (RAI)
- [`get_rai_individuals()`](https://inbo.github.io/camtraptor/reference/get_rai_individuals.md)
  : Get Relative Abundance Index (RAI) based on number of individuals
- [`get_scientific_name()`](https://inbo.github.io/camtraptor/reference/get_scientific_name.md)
  : Get scientific name for vernacular name
- [`get_species()`](https://inbo.github.io/camtraptor/reference/get_species.md)
  : Get species

## Visualize data

- [`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md)
  : Visualize deployments features

## Validate data

- [`check_species()`](https://inbo.github.io/camtraptor/reference/check_species.md)
  : Check scientific or vernacular name(s)

## Publish data

- [`round_coordinates()`](https://inbo.github.io/camtraptor/reference/round_coordinates.md)
  : Round coordinates to generalize camera trap locations
- [`write_dwc()`](https://inbo.github.io/camtraptor/reference/write_dwc.md)
  : Transform Camtrap DP data to Darwin Core
- [`write_eml()`](https://inbo.github.io/camtraptor/reference/write_eml.md)
  : Transform Camtrap DP metadata to EML

## camtrapR-derived data

- [`get_cam_op()`](https://inbo.github.io/camtraptor/reference/get_cam_op.md)
  : Get camera operation matrix
- [`get_detection_history()`](https://inbo.github.io/camtraptor/reference/get_detection_history.md)
  : Get the detection history of a species
- [`get_record_table()`](https://inbo.github.io/camtraptor/reference/get_record_table.md)
  : Get record table

## Estimate density

- [`calc_animal_pos()`](https://inbo.github.io/camtraptor/reference/calc_animal_pos.md)
  : Calculate animal position

## Sample data

- [`animal_positions`](https://inbo.github.io/camtraptor/reference/animal_positions.md)
  : Sample of animal position digitization data
- [`dep_calib_models`](https://inbo.github.io/camtraptor/reference/dep_calib_models.md)
  : Sample of deployment calibration models
- [`mica`](https://inbo.github.io/camtraptor/reference/mica.md) : Sample
  of Camtrap DP formatted data
