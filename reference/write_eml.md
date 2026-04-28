# Transform Camtrap DP metadata to EML

Transforms the metadata of a [Camera Trap Data
Package](https://camtrap-dp.tdwg.org) to an
[EML](https://eml.ecoinformatics.org/) file that can be uploaded to a
[GBIF IPT](https://www.gbif.org/ipt) for publication.

## Usage

``` r
write_eml(
  package,
  directory = ".",
  title = package$title,
  description = package$description,
  creators = NULL,
  keywords = c("camera traps")
)
```

## Arguments

- package:

  A Camtrap DP, as read by
  [`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).

- directory:

  Path to local directory to write file to. If `NULL`, then the EML
  object is returned instead, which can be useful for extended/adapting
  the EML before writing with
  [`EML::write_eml()`](https://docs.ropensci.org/EML/reference/write_eml.html).

- title:

  Dataset title.

- description:

  Dataset description. Will be added after an automatically generated
  paragraph. Multiple paragraphs can be provided as a character vector.

- creators:

  Dataset creators

  - If `NULL` then all `package$contributors` will be added as creators,
    in the order as listed.

  - If e.g. `c("Emma Cartuyvels", "Jim Casaer", "...", "Peter Desmet")`,
    then Emma Cartuyvels, Jim Casaer and Peter Desmet will be set as
    first, second and last creators respectively, on the condition that
    their name (`title`) is present in `package$contributors`. All other
    contributors will be inserted at `"..."`, sorted on their last name.

- keywords:

  Dataset keywords.

## Value

`eml.xml` file written to disk or `EML` object when `directory = NULL`.

## Transformation details

Metadata is derived from what is provided in `package` and in the
function parameters. The following properties are set:

- **title**: Title as provided in `title` or `package$title`.

- **description**: Description as provided in `description` or
  `package$description`. The description is preceded by an automatically
  generated paragraph describing from which project and platform the
  dataset is derived, and to which extend coordinates are rounded
  (`package$coordinatePrecision`).

- **license**: License with scope `data` as provided in
  `package$licenses`.

- **creators**: Contributors (all roles) as provided in
  `package$contributors`, filtered/reordered based on `creators`.

- **contact**: First creator.

- **metadata provider**: First creator.

- **keywords**: Keywords as provided in `keywords`.

- **associated parties**: Organizations as provided in
  `package$organizations`.

- **geographic coverage**: Bounding box as provided in
  `package$spatial`.

- **taxonomic coverage**: Species (no other ranks) as provided in
  `package$taxonomic`.

- **temporal coverage**: Date range as provided in `package$temporal`.

- **project data**: Title, acronym as identifier, description, and
  sampling design as provided in `package$project`. The first creator is
  set as project personnel.

- **alternative identifier**: Identifier as provided in `package$id`. If
  this is a DOI, no new DOI will be created when publishing to GBIF.

- **external link**: URL of the project as provided in
  `package$project$path`.

To be set manually in the GBIF IPT: **type**, **subtype**, **update
frequency** and **publishing organization**.

Not set: **sampling methods** and **citations**.

Not applicable: **collection data**.

## See also

Other publication functions:
[`round_coordinates()`](https://inbo.github.io/camtraptor/reference/round_coordinates.md),
[`write_dwc()`](https://inbo.github.io/camtraptor/reference/write_dwc.md)
