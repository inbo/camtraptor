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