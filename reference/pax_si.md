# Survey index computation functions

Functions to compute and scale survey indices of abundance and biomass.
The typical workflow is:

1.  `pax_si_by_length()` – join station and length data, scale by tow
    area

2.  `pax_si_scale_by_strata()` – multiply by stratum area

3.  `pax_si_strata_summary()` – aggregate within strata

4.  `pax_si_year_summary()` – aggregate across strata to annual totals

## Usage

``` r
pax_si_scale_by_alk(
  tbl,
  lgroups = seq(0, 200, 5),
  regions = list(all = 101:115),
  tgroup = NULL,
  ygroup = NULL,
  gear_group = NULL,
  alk_tbl
)

pax_si_scale_by_landings(
  tbl,
  species,
  landings_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "landings"),
  logbook_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "logbook"),
  regions = list(all = 101:115),
  gear_group = list(Other = "VAR", BMT = c("BMT", "NPT", "SHT", "PGT"), LLN = "LLN", DSE
    = c("PSE", "DSE")),
  tgroup = list(t1 = 1:6, t2 = 7:12),
  month_na = NULL,
  gear_na = NULL
)

pax_si_by_length(
  tbl,
  ldist = pax_ldist_add_weight(dplyr::tbl(dbplyr::remote_con(tbl), "ldist"))
)

pax_si_scale_winsorize(tbl, q = 0.95)

pax_si_scale_by_strata(
  tbl,
  strata_tbl,
  area_col = "rall_area",
  strata_stations = NULL
)

pax_si_strata_summary(tbl, length_range = c(5, 500), std.cv = 0.2)

pax_si_year_summary(tbl)
```

## Arguments

- tbl:

  A dplyr query, typically from the station table

- lgroups:

  Numeric vector of length group lower bounds

- regions:

  Named list mapping region names to vectors of division codes

- tgroup:

  Named list mapping temporal group names to vectors of month integers,
  or `NULL` for a single annual group

- ygroup:

  Named list mapping year group names to vectors of year integers, or
  `NULL` for one group per year

- gear_group:

  Named list mapping gear group names to vectors of `mfdb_gear_code`
  values

- alk_tbl:

  A dplyr query with `agep` column, as returned by
  [`pax_ldist_alk()`](https://hafro.github.io/pax/reference/pax_ldist.md)

- species:

  Integer species code

- landings_tbl:

  A dplyr query from the landings table

- logbook_tbl:

  A dplyr query from the logbook table, used to disaggregate landings by
  area when more than one region is defined

- month_na, gear_na:

  Month and `mfdb_gear_code` to give landings with an unknown month or
  gear (e.g. `6` and `"BMT"`, as tidypax did). Default `NULL`: landings
  with an unknown month get no `tgroup` (unless a `tgroup` contains
  `NA`) and are left out of the scaling, with a message giving their
  total

- ldist:

  A dplyr query from the ldist table, pre-processed with
  [`pax_ldist_add_weight()`](https://hafro.github.io/pax/reference/pax_ldist.md)
  to provide a `weight` column

- q:

  Quantile threshold above which station biomass values are winsorized
  (default 0.95)

- strata_tbl:

  A dplyr query or table name for strata polygons, with columns
  `stratum`, `h3_cells`, `rall_area`, and `geom`

- area_col:

  Name of the column in `strata_tbl` containing the surveyable area in
  km² (default `"rall_area"`)

- strata_stations:

  Optional fixed station list (e.g. the `strata_stations` table filtered
  to one `stratification`), a data.frame or dplyr query with columns
  `station` and `stratum`, and optionally `sampling_type`. If given,
  stations get their stratum from it, as tidypax did, instead of from
  the h3 cell of the tow position; stations not in the list get no
  stratum (as stations outside the strata). Default `NULL`, strata from
  tow positions

- length_range:

  Numeric vector of length 2, only fish within this length range
  (exclusive) contribute to the summary

- std.cv:

  Coefficient of variation used as a floor for the standard deviation
  when all station values within a stratum are identical

## Value

### pax_si_scale_by_alk

A dplyr query with `si_abund` and `si_biomass` scaled by the age-length
key proportions, filtered to positive values

### pax_si_scale_by_landings

A dplyr query with `si_abund` and `si_biomass` rescaled so that total
biomass matches commercial landings

### pax_si_by_length

A dplyr query joining station and length data, with `si_abund`
(thousands of fish per stratum) and `si_biomass` (tonnes per stratum)
columns added

### pax_si_scale_winsorize

A dplyr query with extreme `si_abund` and `si_biomass` values scaled
down to the `q` quantile within each year and species. The station
biomass (the sum of `si_biomass` over the rows of a `sample_id`,
stations with biomass above zero only) is compared with the `q` quantile
of the station biomass of its year and species; every row of a station
above it has `si_biomass` and `si_abund` multiplied by quantile /
station biomass, so the station's biomass becomes the quantile

### pax_si_scale_by_strata

A dplyr query with each station assigned to a stratum, and `si_abund`
and `si_biomass` multiplied by the stratum area (in square nautical
miles) and divided by the number of stations in that stratum

### pax_si_strata_summary

A dplyr query aggregated by species, year, stratum, sampling type, and
area, with columns `si_N`, `si_abund`, `si_abund_sd`, `si_biomass`, and
`si_biomass_sd`

### pax_si_year_summary

A dplyr query aggregated by species, sampling type, and year, with
columns `si_N`, `si_abund`, `si_abund_cv`, `si_biomass`, and
`si_biomass_cv`
