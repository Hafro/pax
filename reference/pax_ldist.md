# Length distribution functions

Functions to compute, scale and plot length-frequency distributions from
survey data.

## Usage

``` r
pax_ldist_alk(
  tbl,
  lgroups = seq(0, 200, 5),
  regions = list(all = 101:115),
  gear_group = list(Other = "VAR", BMT = c("BMT", "NPT", "SHT", "PGT"), LLN = "LLN", DSE
    = c("PSE", "DSE")),
  tgroup = NULL,
  ygroup = NULL,
  aldist_tbl = dplyr::summarize(dplyr::group_by(dplyr::tbl(dbplyr::remote_con(tbl),
    "aldist"), sample_id, species, length, age), count = sum(count, na.rm = TRUE), weight
    = sum(weight * count, na.rm = TRUE)/sum(count, na.rm = TRUE))
)

pax_ldist_scale_round(tbl)

pax_ldist_add_weight(tbl, lw_coeffs_tbl = "lw_coeffs")

pax_ldist_scale_tow_area(
  tbl,
  towdims_tbl = data.frame(sampling_type = c(30, 35, 31, 37, 19, 34), min_towlength =
    c(2, 2, 0.5, 0.5, 0.5, 0.5), max_towlength = c(8, 8, 4, 4, 4, 0.5), std_towlength =
    c(4, 4, 1, 1, 2, 0.5), std_width = c(17/1852, 17/1852, 17/1852, 27.595/1.852^2/1000,
    4/1852, 50)),
  vfadj_tbl = data.frame(gear_id = 78, vf_adj = 1.25)
)

pax_ldist_by_year(
  tbl,
  ldist_tbl = pax_ldist_scale_round(dplyr::tbl(dbplyr::remote_con(tbl), "ldist"))
)

pax_ldist_scale_abund(
  tbl,
  measurement_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "measurement")
)

pax_ldist_plot(tbl, scale = 1, expand = FALSE)

pax_ldist_joy_plot(ldist, max_height = 50, split_by_sex = FALSE)
```

## Arguments

- tbl:

  A dplyr query, typically from the station table

- lgroups:

  Numeric vector of length group lower bounds

- regions:

  Named list mapping region names to vectors of division codes

- gear_group:

  Named list mapping gear group names to vectors of `mfdb_gear_code`
  values

- tgroup:

  Named list mapping temporal group names to vectors of month integers,
  or `NULL` to use a single annual group

- ygroup:

  Named list mapping year group names to vectors of year integers, or
  `NULL` for one group per year

- aldist_tbl:

  A dplyr query from the aldist table, pre-aggregated by `sample_id`,
  `species`, `length`, and `age`

- lw_coeffs_tbl:

  A dplyr query or table name for length-weight coefficients, with
  columns `a`, `b`, and optionally `species` and `sex`

- towdims_tbl:

  A data.frame of per-sampling-type tow dimension standards with columns
  `sampling_type`, `min_towlength`, `max_towlength`, `std_towlength`,
  and `std_width`

- vfadj_tbl:

  A data.frame of vertical fishing adjustments with columns `gear_id`
  and `vf_adj`

- ldist_tbl:

  A dplyr query from the ldist table, pre-processed with
  `pax_ldist_scale_round()`. The ldist table from
  [`pax_mar_ldist()`](https://hafro.github.io/pax/reference/pax_mar.md)
  is already raised to the counted fish (`mar::skala_med_taldir()`), so
  don't apply `pax_ldist_scale_abund()` to it again

- measurement_tbl:

  A dplyr query from the measurement table, used to compute the ratio of
  counted (CNT/WEI) to length-measured (LEN/LENM/LENC) fish for
  abundance scaling

- scale:

  Numeric; `1` to plot proportions (default), any other value to plot
  raw counts

- expand:

  Boolean, whether to expand the data to fill all length/year
  combinations with zeroes

- ldist:

  A data.frame or dplyr query of length distributions, with columns
  `year`, `mfdb_gear_code`, `length`, and `n`

- max_height:

  Maximum ridge height in plot units

- split_by_sex:

  Boolean, whether to produce separate facets for each sex

## Value

### pax_ldist_alk

A dplyr query with columns for grouping variables, `age`, and `agep`
(proportion at age within each length group)

### pax_ldist_scale_round

A dplyr query with the `length` column rounded to the nearest integer

### pax_ldist_add_weight

A dplyr query with a `weight` column added, calculated as `a * length^b`

### pax_ldist_scale_tow_area

A dplyr query with `count` rescaled to fish per square nautical mile

### pax_ldist_by_year

A dplyr query of length distributions aggregated by species, year, sex,
length, and gear

### pax_ldist_scale_abund

A dplyr query with `count` scaled up to represent total abundance based
on subsample ratios. Only for unraised length counts: the ldist table
from
[`pax_mar_ldist()`](https://hafro.github.io/pax/reference/pax_mar.md) is
already raised

### pax_ldist_plot

A ggplot2 faceted plot of length distributions by year, with mean length
and sample size annotations

### pax_ldist_joy_plot

A ggplot2 ridgeline plot of length distributions faceted by gear and
optionally by sex
