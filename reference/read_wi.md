# Read a Wildlife Insights export

Reads files from an unzipped [Wildlife
Insights](https://www.wildlifeinsights.org/) export into memory. Data
can be exported from Wildlife Insights as a
[public](https://www.wildlifeinsights.org/get-started/data-download/public)
or
[private](https://www.wildlifeinsights.org/get-started/download/private)
download. The function transforms data and metadata to a [Camera Trap
Data Package](https://camtrap-dp.tdwg.org) which can be written to file
with
[`frictionless::write_package()`](https://docs.ropensci.org/frictionless/reference/write_package.html).

## Usage

``` r
read_wi(directory = ".")
```

## Arguments

- directory:

  Path to local directory to read files from. The function expects
  `projects.csv`, `deployments.csv`, `cameras.csv`, and `images.csv`.

## Value

CSV (data) files written to disk.

## Details

**The function has only been tested on image-based projects.**

## See also

Other read functions:
[`read_camtrap_dp()`](https://inbo.github.io/camtraptor/reference/read_camtrap_dp.md)
