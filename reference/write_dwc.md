# Transform Camtrap DP data to Darwin Core

Transforms data from a [Camera Trap Data
Package](https://camtrap-dp.tdwg.org) to [Darwin
Core](https://dwc.tdwg.org/). The resulting CSV files can be uploaded to
an [IPT](https://www.gbif.org/ipt) for publication to GBIF. A `meta.xml`
file is included as well. See
[`write_eml()`](https://inbo.github.io/camtraptor/reference/write_eml.md)
to create an `eml.xml` file.

## Usage

``` r
write_dwc(package, directory = ".")
```

## Arguments

- package:

  A Camtrap DP, as read by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- directory:

  Path to local directory to write file(s) to. If `NULL`, then a list of
  data frames is returned instead, which can be useful for
  extending/adapting the Darwin Core mapping before writing with
  [`readr::write_csv()`](https://readr.tidyverse.org/reference/write_delim.html).

## Value

CSV and `meta.xml` files written to disk or a list of data frames when
`directory = NULL`.

## Transformation details

Data are transformed into an [Occurrence
core](https://rs.gbif.org/core/dwc_occurrence_2022-02-02.xml) and
[Audubon Media Description
extension](https://rs.gbif.org/extension/ac/audubon_2020_10_06.xml).
This **follows recommendations** discussed and created by Peter Desmet,
John Wieczorek, Lien Reyserhove, Ben Norton and others.

The following terms are set from the `package` metadata:

- **datasetName**: Title as provided in `package$title`.

- **datasetID**: Identifier as provided in `package$id`. Can be a DOI.

- **rightsHolder**: Rights holder as provided in `package$rightsHolder`.

- **collectionCode**: Platform name as provided in
  `package$platform$title`.

- **license**: License with scope `data` as provided in
  `package$licenses`.

- **rights** for media files: License with scope `media` as provided in
  `package$licenses`.

- **dwc:dataGeneralizations**: "coordinates rounded to
  `package$coordinatePrecision` degree".

- **coordinatePrecision**: `package$coordinatePrecision` (e.g. `0.001`).

Key features of the Darwin Core transformation:

- Deployments (of camera traps) are parent events, with observations
  (machine observations) as child events. No information about the
  parent event is provided other than its ID, meaning that data can be
  expressed in an Occurrence Core with one row per observation and
  `parentEventID` shared by all occurrences in a deployment.

- Sequence-based observations share an `eventID` per sequence,
  image-based observations share an `eventID` per image.

- The image(s) an observation is based on are provided in the [Audubon
  Media Description
  extension](https://rs.gbif.org/extension/ac/audubon_2020_10_06.xml),
  with a foreign key to the observation.

- Excluded are records that document blank or unclassified media,
  vehicles and observations of humans.

## See also

Other publication functions:
[`round_coordinates()`](https://inbo.github.io/camtraptor/reference/round_coordinates.md),
[`write_eml()`](https://inbo.github.io/camtraptor/reference/write_eml.md)
