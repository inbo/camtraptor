# Read a Camtrap DP

Reads files from a [Camera Trap Data
Package](https://camtrap-dp.tdwg.org) into memory. All datetime
information is automatically transformed to Coordinated Universal Time
(UTC). Vernacular names found in the metadata (`package$taxonomic`) are
added to the `observations` data frame.

## Usage

``` r
read_camtrap_dp(file = NULL, media = TRUE, path = lifecycle::deprecated())
```

## Arguments

- file:

  Path or URL to a `datapackage.json` file.

- media:

  If `TRUE` (default), read media records into memory. If `FALSE`,
  ignore media file to speed up reading larger Camtrap DP packages.

- path:

  Path to the directory containing the datapackage. Use `file` with path
  or URL to a `datapackage.json` file instead.

## Value

List describing a Data Package (as returned by
[`frictionless::read_package()`](https://docs.ropensci.org/frictionless/reference/read_package.html))
containing the original metadata, as well as a property `data`
containing the data as three data frames:

1.  `deployments`

2.  `media`

3.  `observations`

## See also

Other read functions:
[`read_wi()`](https://inbo.github.io/camtraptor/reference/read_wi.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read Camtrap DP package
camtrap_dp_file <- system.file(
  "extdata", "mica", "datapackage.json", 
  package = "camtraptor"
)
muskrat_coypu <- read_camtrap_dp(camtrap_dp_file)

# Read Camtrap DP package and ignore media file
muskrat_coypu <- read_camtrap_dp(camtrap_dp_file, media = FALSE)

# If parsing issues while reading deployments, observations or media arise,
# use readr::problems()
camtrap_dp_file_with_issues <- system.file(
  "extdata",
  "mica_parsing_issues",
  "datapackage_for_parsing_issues.json",
  package = "camtraptor"
)
muskrat_coypu_with_issues <- read_camtrap_dp(camtrap_dp_file_with_issues, media = TRUE)
readr::problems(muskrat_coypu_with_issues$data$deployments)
readr::problems(muskrat_coypu_with_issues$data$observations)
readr::problems(muskrat_coypu_with_issues$data$media)
} # }
```
