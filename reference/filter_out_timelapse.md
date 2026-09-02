# Filter out timelapse observations

Subsets observations in a Camera Trap Data Package object, removing
timelapse observations, i.e. observations where `captureMethod` =
`timeLapse`. This function is a shortcut for
`filter_observations(x, captureMethod != "timelapse")`.

## Usage

``` r
filter_out_timelapse(x)
```

## Arguments

- x:

  Camera trap data package object, as returned by
  [`camtrapdp::read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.html).

## Value

`x` filtered.

## See also

Other filter functions:
[`filter_deployments()`](https://inbo.github.io/camtraptor/reference/filter_deployments.md),
[`filter_media()`](https://inbo.github.io/camtraptor/reference/filter_media.md),
[`filter_observations()`](https://inbo.github.io/camtraptor/reference/filter_observations.md)

## Examples

``` r
x <- example_dataset()

# `x` doesn't contain timelapse observations, returned as is
filter_out_timelapse(x)
#> A Camera Trap Data Package "camtrap-dp-example-dataset" with 3 tables:
#> • deployments: 4 rows
#> • media: 423 rows
#> • observations: 549 rows
#> 
#> And 1 additional resource:
#> • individuals
#> Use `unclass()` to print the Data Package as a list.

# Create a data package with timelapse observations
obs <- observations(x)
obs$captureMethod <- c(rep("timelapse", nrow(obs) - 1), "activityDetection")
observations(x) <- obs
# Filter out timelapse observations
filter_out_timelapse(x)
#> A Camera Trap Data Package "camtrap-dp-example-dataset" with 3 tables:
#> • deployments: 4 rows
#> • media: 1 rows
#> • observations: 1 rows
#> 
#> And 1 additional resource:
#> • individuals
#> Use `unclass()` to print the Data Package as a list.
```
