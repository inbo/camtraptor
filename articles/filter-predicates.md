# Filter predicates

This vignette shows what filter predicates are and how to use them in
`get_*()` functions or in
[`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md).

## Setup

Load packages:

``` r
library(camtraptor)
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

By loading package `camtraptor`, a camera trap data package called
`mica` is made available. This data package contains camera trap data of
musk rats and coypus. We will use this variable from now on.

## Filter predicates

All filter predicates are functions starting with `pred` prefix. They
can be distinguished in four categories based on the type of inputs they
accept:

1.  one argument, one value
2.  one argument, no value
3.  one argument, multiple values (vector)
4.  multiple predicates

They are called filter predicates because they build (dplyr) filter
statement. Filter predicates return objects of class `filter_predicate`,
which are a particular kind of list with the following slots:

1.  `arg`, the argument
2.  `value`, the value
3.  `type`, the type of filter predicate
4.  `expr`, the filter dplyr expression

### One argument - one value predicates

This filter predicates accept one argument and one value.

#### `pred()`, `pred_not()`

The
[`pred()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
is the most basic predicates and refers to equality statements. Example,
if you want to select rows where column `a` is equal to 5:

``` r
pred(arg = "a", value = 5)
#> $arg
#> [1] "a"
#> 
#> $value
#> [1] 5
#> 
#> $type
#> [1] "equals"
#> 
#> $expr
#> (a == 5)
#> 
#> attr(,"class")
#> [1] "filter_predicate"
```

The opposite operator of
[`pred()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
(equals) is `pred_not` (notEquals):

``` r
pred_not(arg = "a", value = 5)
#> $arg
#> [1] "a"
#> 
#> $value
#> [1] 5
#> 
#> $type
#> [1] "notEquals"
#> 
#> $expr
#> (a != 5)
#> 
#> attr(,"class")
#> [1] "filter_predicate"
```

#### `pred_gt()`, `pred_gte()`, `pred_lt()`, `pred_lte()`

These predicates express `>` (greaterThan), `>=`
(greaterThanOrEqual),`<` (lessThan) and `<=` (lessThanOrEqual)
respectively. Example: if you want to select rows where column `a` is
greater than 5:

``` r
pred_gt(arg = "a", value = 5)
#> $arg
#> [1] "a"
#> 
#> $value
#> [1] 5
#> 
#> $type
#> [1] "greaterThan"
#> 
#> $expr
#> (a > 5)
#> 
#> attr(,"class")
#> [1] "filter_predicate"
```

### One argument - no value predicates

The predicate
[`pred_na()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
compares the argument against NA. To select rows where column `a` is NA:

``` r
pred_na(arg = "a")
#> $arg
#> [1] "a"
#> 
#> $value
#> [1] NA
#> 
#> $type
#> [1] "na"
#> 
#> $expr
#> (is.na(a))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
```

To select all the rows where `a` is not NA, you can use the opposite
predicate
[`pred_notna()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md):

``` r
pred_notna(arg = "a")
#> $arg
#> [1] "a"
#> 
#> $value
#> [1] NA
#> 
#> $type
#> [1] "notNa"
#> 
#> $expr
#> (!is.na(a))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
```

### One argument - multiple value predicates

These predicates accept a vector with multiple values as argument. To
get rows where column `a` is one of the values `c(1,3,5)`:

``` r
pred_in(arg = "a", value = c(1,3,5))
#> $arg
#> [1] "a"
#> 
#> $value
#> [1] 1 3 5
#> 
#> $type
#> [1] "in"
#> 
#> $expr
#> (a %in% c(1,3,5))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
```

The opposite of
[`pred_in()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
is
[`pred_notin()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md):

``` r
pred_notin(arg = "a", value = c(1,3,5))
#> $arg
#> [1] "a"
#> 
#> $value
#> [1] 1 3 5
#> 
#> $type
#> [1] "notIn"
#> 
#> $expr
#> (!(a %in% c(1,3,5)))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
```

### multiple predicates: `pred_and()` and `pred_or()`

You can combine the predicates described above to make more complex
filter statements by using
[`pred_and()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
(AND operator) and
[`pred_or()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md)
(OR operator).

Some examples. Select rows where column `a` is equal to 5 and column `b`
is not NA:

``` r
pred_and(pred("a", 5), pred_notna("b"))
#> $arg
#> $arg[[1]]
#> [1] "a"
#> 
#> $arg[[2]]
#> [1] "b"
#> 
#> 
#> $value
#> $value[[1]]
#> [1] 5
#> 
#> $value[[2]]
#> [1] NA
#> 
#> 
#> $type
#> $type[[1]]
#> [1] "equals"
#> 
#> $type[[2]]
#> [1] "notNa"
#> 
#> 
#> $expr
#> ((a == 5) & (!is.na(b)))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
```

Select rows where column `a` is equal to 5 or column `b` is not NA:

``` r
pred_or(pred("a", 5), pred_notna("b"))
#> $arg
#> $arg[[1]]
#> [1] "a"
#> 
#> $arg[[2]]
#> [1] "b"
#> 
#> 
#> $value
#> $value[[1]]
#> [1] 5
#> 
#> $value[[2]]
#> [1] NA
#> 
#> 
#> $type
#> $type[[1]]
#> [1] "equals"
#> 
#> $type[[2]]
#> [1] "notNa"
#> 
#> 
#> $expr
#> ((a == 5) | (!is.na(b)))
#> 
#> attr(,"class")
#> [1] "filter_predicate"
```

Notice how these two predicates return `filter_predicate` objects with
the same structure as any other predicate, but with slots `arg`, `value`
and `type` as long as the number of input predicates they combine.

## How to use filter predicates

The filter predicates are useful to select a subset of **deployments**
for the `get_*()` functions and the visualization function
[`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md).

### One predicate

Apply get\_\* functions only to the deployments with location name
*“B_DL_val 5_beek kleine vijver”* or *“B_DL_val 3_dikke boom”*:

``` r
get_n_obs(mica,
          pred_in("locationName",
                  c("B_DL_val 5_beek kleine vijver", "B_DL_val 3_dikke boom")))
#> df %>% dplyr::filter((locationName %in% c("B_DL_val 5_beek kleine vijver","B_DL_val 3_dikke boom")))
#> # A tibble: 18 × 3
#>    deploymentID                         scientificName         n
#>    <chr>                                <chr>              <int>
#>  1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas strepera          3
#>  2 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Anas platyrhynchos     4
#>  3 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Castor fiber           0
#>  4 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Mustela putorius       0
#>  5 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Vulpes vulpes          0
#>  6 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Martes foina           0
#>  7 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea cinerea          0
#>  8 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Ardea                  0
#>  9 29b7d356-4bb4-4ec4-b792-2af5cc32efa8 Homo sapiens           0
#> 10 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas strepera          0
#> 11 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Anas platyrhynchos     0
#> 12 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Castor fiber           1
#> 13 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Mustela putorius       3
#> 14 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Vulpes vulpes          1
#> 15 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Martes foina           1
#> 16 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Ardea cinerea          0
#> 17 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Ardea                  0
#> 18 577b543a-2cf1-4b23-b6d2-cda7e2eac372 Homo sapiens           0
```

``` r
get_effort(mica,
           pred_in("locationName",
                   c("B_DL_val 5_beek kleine vijver", "B_DL_val 3_dikke boom")))
#> df %>% dplyr::filter((locationName %in% c("B_DL_val 5_beek kleine vijver","B_DL_val 3_dikke boom")))
#> # A tibble: 2 × 4
#>   deploymentID                         effort unit  effort_duration      
#>   <chr>                                 <dbl> <chr> <Duration>           
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8   239. hour  859859s (~1.42 weeks)
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372   219. hour  786802s (~1.3 weeks)
```

``` r
get_n_species(mica,
              pred_in("locationName",
                      c("B_DL_val 5_beek kleine vijver", "B_DL_val 3_dikke boom")))
#> df %>% dplyr::filter((locationName %in% c("B_DL_val 5_beek kleine vijver","B_DL_val 3_dikke boom")))
#> # A tibble: 2 × 2
#>   deploymentID                             n
#>   <chr>                                <int>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8     2
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372     4
```

### Multiple predicates

As shown above, you can combine several predicates for more complex
filtering. E.g. calculate the number of species detected by the
deployments with one of the location names *B_ML_val 06_Oostpolderkreek*
and *B_ML_val 07_Sint-Anna*, or deployments further south than 50.7
degrees:

``` r
get_n_species(mica,
              pred_or(
                pred_in("locationName",
                        c("B_DL_val 5_beek kleine vijver", "B_DL_val 3_dikke boom")),
                pred_lt("latitude", 50.7)))
#> df %>% dplyr::filter(((locationName %in% c("B_DL_val 5_beek kleine vijver","B_DL_val 3_dikke boom")) | (latitude < 50.7)))
#> # A tibble: 3 × 2
#>   deploymentID                             n
#>   <chr>                                <int>
#> 1 29b7d356-4bb4-4ec4-b792-2af5cc32efa8     2
#> 2 577b543a-2cf1-4b23-b6d2-cda7e2eac372     4
#> 3 62c200a9-0e03-4495-bcd8-032944f6f5a1     2
```

Same syntax is valid for visualizing such information via
[`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md)
function:

``` r
map_dep(mica,
        feature = "n_species", 
        pred_or(
          pred_in("locationName",
                  c("B_DL_val 5_beek kleine vijver", "B_DL_val 3_dikke boom")),
          pred_lt("latitude", 50.7)))
#> df %>% dplyr::filter(((locationName %in% c("B_DL_val 5_beek kleine vijver","B_DL_val 3_dikke boom")) | (latitude < 50.7)))
```

Notice also that you can pass as much predicates as you want to
`get_*()` functions or
[`map_dep()`](https://inbo.github.io/camtraptor/reference/map_dep.md) by
separating them with comma: they will be combined internally using the
AND operator. E.g. to get effort of deployments southern than 51 degrees
AND eastern than 4 degrees, you can simplify this:

``` r
get_n_species(mica,
              pred_and(pred_lt("latitude", 51),
                       pred_gt("longitude", 4)))
#> df %>% dplyr::filter(((latitude < 51) & (longitude > 4)))
#> # A tibble: 1 × 2
#>   deploymentID                             n
#>   <chr>                                <int>
#> 1 62c200a9-0e03-4495-bcd8-032944f6f5a1     2
```

by omitting the
[`pred_and()`](https://inbo.github.io/camtraptor/reference/filter_predicate.md):

``` r
get_n_species(mica,
              pred_lt("latitude", 51),
              pred_gt("longitude", 4))
#> df %>% dplyr::filter((latitude < 51) & (longitude > 4))
#> # A tibble: 1 × 2
#>   deploymentID                             n
#>   <chr>                                <int>
#> 1 62c200a9-0e03-4495-bcd8-032944f6f5a1     2
```

This is similar to the behaviour of dplyr’s
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html)
function, where this:

``` r
mica$data$deployments %>%
  dplyr::filter(latitude < 51 & longitude > 4)
#> # A tibble: 1 × 24
#>   deploymentID  locationID locationName longitude latitude coordinateUncertainty
#>   <chr>         <chr>      <chr>            <dbl>    <dbl>                 <dbl>
#> 1 62c200a9-0e0… ce943ced-… B_DM_val 4_…      4.01     50.7                    NA
#> # ℹ 18 more variables: start <dttm>, end <dttm>, setupBy <chr>, cameraID <chr>,
#> #   cameraModel <chr>, cameraInterval <dbl>, cameraHeight <dbl>,
#> #   cameraTilt <dbl>, cameraHeading <dbl>, timestampIssues <lgl>,
#> #   baitUse <fct>, session <chr>, array <chr>, featureType <fct>,
#> #   habitat <chr>, tags <chr>, comments <chr>, `_id` <chr>
```

is exactly the same as this:

``` r
mica$data$deployments %>%
  dplyr::filter(latitude < 51, longitude > 4)
#> # A tibble: 1 × 24
#>   deploymentID  locationID locationName longitude latitude coordinateUncertainty
#>   <chr>         <chr>      <chr>            <dbl>    <dbl>                 <dbl>
#> 1 62c200a9-0e0… ce943ced-… B_DM_val 4_…      4.01     50.7                    NA
#> # ℹ 18 more variables: start <dttm>, end <dttm>, setupBy <chr>, cameraID <chr>,
#> #   cameraModel <chr>, cameraInterval <dbl>, cameraHeight <dbl>,
#> #   cameraTilt <dbl>, cameraHeading <dbl>, timestampIssues <lgl>,
#> #   baitUse <fct>, session <chr>, array <chr>, featureType <fct>,
#> #   habitat <chr>, tags <chr>, comments <chr>, `_id` <chr>
```

Happy filtering!
