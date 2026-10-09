# Survey indices by length range from a fixed station list

The survey indices of the category 3 and SPiCT stocks, computed as the
old tidypax
`si_by_length() |> si_add_strata() |> si_by_strata() |> si_by_year()`:
stations get their stratum from a fixed station list (the
`strata_stations` table of
[`pax_from_mar()`](https://hafro.github.io/pax/reference/pax_from_mar.md)),
each survey has its own stratum areas, and the index of a length range
counts the fish strictly within it (exclusive ends, as
[`pax_si_strata_summary()`](https://hafro.github.io/pax/reference/pax_si.md)).

## Usage

``` r
pax_si_strata_stations(
  pcon,
  stratification = "new_strata",
  area_tables = c(`30` = "new_strata_spring", `35` = "new_strata_autumn")
)

pax_si_scale_by_strata_stations(tbl, strata_stations)

pax_si_by_strata(
  station_tbl,
  strata_stations,
  sampling_type,
  tow_number = NULL,
  gear_id = NULL,
  skip_years = NULL,
  fixed_only = FALSE
)

pax_si_length_index(
  station_tbl,
  strata_stations,
  length_ranges,
  sampling_type,
  tow_number = NULL,
  gear_id = NULL,
  skip_years = NULL,
  fixed_only = FALSE,
  complete_years = FALSE
)
```

## Arguments

- pcon:

  A pax database connection.

- stratification:

  The `stratification` of the `strata_stations` table. Default
  `"new_strata"`.

- area_tables:

  Named character vector: for each sampling type (the names), the strata
  table with its stratum areas (`rall_area`, km²). Default the new
  strata of the spring (30) and autumn (35) surveys.

- tbl:

  A dplyr query of station values, from
  [`pax_si_by_length()`](https://hafro.github.io/pax/reference/pax_si.md).

- strata_stations:

  A data frame with columns `station`, `stratum` and `area` (square
  nautical miles), as `pax_si_strata_stations()`.
  `pax_si_scale_by_strata_stations()` uses all its rows, so give it the
  rows of one survey; the other functions keep the rows of
  `sampling_type` if it has that column.

- station_tbl:

  A pax database connection (its `station` table is used), or a dplyr
  query of the station table, e.g. with columns added.

- sampling_type:

  The survey (one sampling type), e.g. 30 (spring survey) or 35 (autumn
  survey).

- tow_number:

  Tow numbers to keep; stations without a tow number count as tow 0, as
  tidypax. `NULL` keeps all tows.

- gear_id:

  Gears to keep, or `NULL` for all.

- skip_years:

  Years to leave out, e.g. 2011 for the autumn survey (only part of the
  area was covered).

- fixed_only:

  If `TRUE`, only the fixed stations (`fixed == 1`).

- length_ranges:

  Named list of length ranges, each `c(lower, upper)`; an index counts
  the fish strictly within the range (`c(30, 500)`: 31 cm and over in 1
  cm classes).

- complete_years:

  If `TRUE`, a year with survey stations but no fish in a length range
  gets an index of 0 (CVs `NA`); otherwise it has no row (the default,
  as tidypax). The rfb rule takes the last five rows of the index, so a
  missing year shifts index A and B.

## Value

### pax_si_strata_stations

A tibble with columns `sampling_type`, `station`, `stratum` and `area`
(the stratum area of that survey in square nautical miles, 0 if the
strata table has no area for the stratum)

### pax_si_scale_by_strata_stations

A dplyr query as
[`pax_si_scale_by_strata()`](https://hafro.github.io/pax/reference/pax_si.md):
`si_abund` and `si_biomass` multiplied by the stratum area and divided
by the number of stations of the stratum in the year. Stations not on
the list get no stratum and area 0

### pax_si_by_strata

A dplyr query of station values by length scaled to strata, as
`pax_si_scale_by_strata_stations()`

### pax_si_length_index

A tibble with columns `length_range` (the names of `length_ranges`),
`year`, `srN` (strata with fish), `n` (abundance, thousands), `n_cv`,
`b` (biomass, tonnes) and `b_cv`, ordered by length range and year

## Details

- `pax_si_strata_stations()` reads the station list of one
  stratification, with the stratum areas of each survey;

- `pax_si_scale_by_strata_stations()` scales station values to strata
  with that list (the areas come with the list);

- `pax_si_by_strata()` selects the survey stations (sampling type, tows,
  gears, years, fixed stations) and scales them to strata;

- `pax_si_length_index()` gives the biomass and abundance index with CVs
  by year for each length range.

## Examples

``` r
if (FALSE) { # \dontrun{
pcon <- pax_connect("pax.duckdb")
strata <- pax_si_strata_stations(pcon)
pax_si_length_index(
  pcon,
  strata,
  length_ranges = list(total = c(1, 500), harv = c(30, 500)),
  sampling_type = 35,
  tow_number = 0:75,
  gear_id = 77:78,
  skip_years = 2011
)
} # }
```
