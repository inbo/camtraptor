# Defunct functions

**\[defunct\]**

These functions are not supported anymore. If there's a known
replacement, calling the function will tell you about it.

`write_dwc()` has been moved to camtrapdp. Check
[`camtrapdp::write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.html)
for more information.

`write_eml()` has been moved to camtrapdp. Check
[`camtrapdp::write_eml()`](https://inbo.github.io/camtrapdp/reference/write_eml.html)
for more information.

`round_coordinates()` has been moved to camtrapdp. Check
[`camtrapdp::round_coordinates()`](https://inbo.github.io/camtrapdp/reference/round_coordinates.html)
for more information.

## Usage

``` r
# Deprecated in 1.0.0 -------------------------------------

pred(arg, value)

pred_not(arg, value)

pred_gt(arg, value)

pred_gte(arg, value)

pred_lt(arg, value)

pred_lte(arg, value)

pred_in(arg, value)

pred_notin(arg, value)

pred_na(arg, value)

pred_notna(arg, value)

pred_and(arg, value)

pred_or(arg, value)

apply_filter_predicate(arg, value)

check_species(x, species, arg_name)

get_scientific_name(x)

read_wi(x)

write_dwc(x)

write_eml(x)

round_coordinates(x)
```
