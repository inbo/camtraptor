# camtraptor 0.21.0

- `read_camtrap_dp()` supports Camtrap DP 1.0 (upcoming Agouti export format) 
  in favour of Camtrap DP 1.0-rc.1 (#284).
  To avoid breaking changes to users, it will down-convert Camtrap DP 1.0 to
  0.1.6 which is currently used as internal data model for camtraptor.
- `get_custom_effort()` now calculates per calendar month/week (#219).
- `write_dwc()` has an updated mapping for dwc_audubon.csv (#274).
