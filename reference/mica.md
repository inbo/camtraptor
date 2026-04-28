# Sample of Camtrap DP formatted data

A sample [Camera Trap Data Package](https://camtrap-dp.tdwg.org) as read
by
[`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md).
The source data are derived from the [Camtrap DP example
dataset](https://github.com/tdwg/camtrap-dp/tree/ad0278ef86ef518dacfb306c598dce97667cfb81/example)
and are saved in `inst/extdata/mica`.

## Usage

``` r
mica
```

## Format

An object of class `datapackage` (inherits from `list`) of length 16.

## Source

<https://github.com/tdwg/camtrap-dp/tree/ad0278ef86ef518dacfb306c598dce97667cfb81/example>

## Details

A larger dataset is available in `inst/extdata/mica_zenodo_5590881`. It
is derived from a dataset on
[Zenodo](https://zenodo.org/record/5590881), but excludes `media.csv`.

## See also

Other sample data:
[`animal_positions`](https://inbo.github.io/camtraptor/reference/animal_positions.md),
[`dep_calib_models`](https://inbo.github.io/camtraptor/reference/dep_calib_models.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# mica.rda was created with the code below.
mica <- read_camtrap_dp(
  system.file(
    "extdata/mica",
    "datapackage.json",
    package = "camtraptor"
  )
)
save(mica, file = "data/mica.rda")
} # }
```
