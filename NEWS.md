# camtraptor 1.0

This is a major release that updates the internal data model to Camtrap DP 1.0 and drops support for Camtrap DP 0.1.6.

- `read_camtrapdp()` reexports the read functionality from [camtrapdp](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html) (#298). 
It replaces the now deprecated `read_camtrap_dp()` (with underscore) (#298).
- `read_camtrap_dp()` is deprecated. Use `read_camtrapdp()` instead.
- The deprecated argument `datapkg` in `read_camtrap_dp()` is removed.
- `write_dwc()` has moved to [camtrapdp](https://inbo.github.io/camtrapdp/reference/write_dwc.html). `write_eml()` is not needed for GBIF processing of Camtrap DPs and has been removed ([camtrapdp#61](https://github.com/inbo/camtrapdp/issues/61)).
- `example_dataset()` allows to create an example dataset in Camtrap DP 1.0 format. It reexports the functionality from [camtrapdp](https://inbo.github.io/inbo/camtrapdp/reference/example_dataset.html). The internal dataset `mica` is not present anymore.
- Functions `deployments()`, `observations()` and `media()` allow to extract the respective tables (#317). They reexports the functionality from camtrapdp, e.g. see [`camtrapdp::deployments()`](https://inbo.github.io/camtrapdp/reference/deployments.html).
- The first argument of many functions accepts a Camtrap DP object: not `package` anymore, but `x` (#324).

# camtraptor 0.25.0

- `read_camtrap_dp()` detects Camtrap DP version from `package$profile` using 
regex (#295).
This supports reading Camtrap DPs created by the GBIF IPT.

# camtraptor 0.24.0

- Replicate old Camtrap DP 0.1.6 behaviour and populate `angle` and `radius` for 
event-based observations.
Values are taken from the first media-based observation (fields 
`individualPositionRadius` and `individualPositionAngle`) for each 
`eventID/individualID` combination (#291).

# camtraptor 0.23.0

- Fix bug in `read_camtrap_dp()` when reading a Camtrap DP 1.0 (#292).

# camtraptor 0.22.0

- Fix bug in `write_eml()` for Camtrap DP 1.0 datasets (#290).
- `read_camtrap_dp()` will now always populate `taxonID` from the 
  `package.taxonomy` (#290).

# camtraptor 0.21.0

- `read_camtrap_dp()` supports Camtrap DP 1.0 (upcoming Agouti export format) in 
favour of Camtrap DP 1.0-rc.1 (#284).
To avoid breaking changes to users, it will down-convert Camtrap DP 1.0 to 0.1.6
which is currently used as internal data model for camtraptor.
- `get_custom_effort()` now calculates per calendar month/week (#219).
- `write_dwc()` has an updated mapping for dwc_audubon.csv (#274).
- `get_record_table()` returns the number of observed individuals (#279).
- `get_cam_op()` allows to add session and camera IDs to the station names output (#288).
