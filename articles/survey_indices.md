# Survey indices

A stratified survey index in pax takes four steps, each a function that
takes a lazy table (a dplyr query on the pax database) and returns
another:

1.  [`pax_si_by_length()`](https://hafro.github.io/pax/reference/pax_si.md):
    join the stations to the length distributions, and turn the counts
    into fish per square nautical mile (tow length × width) and biomass
    (length–weight relationship);
2.  [`pax_si_scale_by_strata()`](https://hafro.github.io/pax/reference/pax_si.md)
    or
    [`pax_si_scale_by_strata_stations()`](https://hafro.github.io/pax/reference/pax_si_index.md):
    give each station a stratum and multiply by the stratum area over
    the number of stations in the stratum;
3.  [`pax_si_strata_summary()`](https://hafro.github.io/pax/reference/pax_si.md):
    sum within strata, for one length range, with standard deviations;
4.  [`pax_si_year_summary()`](https://hafro.github.io/pax/reference/pax_si.md):
    sum over strata to the index and its CV by year.

[`pax_si_length_index()`](https://hafro.github.io/pax/reference/pax_si_index.md)
runs all four for a fixed station list and several length ranges, as the
category 3 stocks use. In the stock repos the at-age indices for SAM
come from `hafroreports::hr_input_data_si_index()`, which runs the same
chain with an age–length key (see the vignette on length distributions
and age–length keys).

This vignette builds a tiny database of simulated survey data and runs
both. Nothing here comes from real surveys; only the strata polygons are
the package’s own.

## A toy survey

The stations sit at the centres of 12 statistical subrectangles
(gridcells), three in each of four strata of the spring survey
(`new_strata_spring`).

``` r

library(pax)
library(dplyr, warn.conflicts = FALSE)

pcon <- pax_connect(tempfile(fileext = ".duckdb"))
```

``` r

# Strata polygons (with stratum areas) shipped with pax
pax_import(pcon, pax_def_strata("new_strata_spring"))
```

``` r

grid <- data.frame(
  gridcell = c(2741, 3213, 3223, 3202, 3703, 3714,
               3161, 3631, 3643, 6113, 6623, 7144),
  lat = c(62.875, 63.125, 63.125, 63.375, 63.625, 63.625,
          63.375, 63.875, 63.625, 66.125, 66.625, 67.125),
  lon = c(-24.75, -21.75, -22.75, -20.25, -20.75, -21.25,
          -16.75, -13.75, -14.75, -11.75, -12.75, -14.25)
)

set.seed(42)
station <- merge(grid, data.frame(year = 2021:2024))
station <- station[order(station$year, station$gridcell), ]
station$sample_id <- seq_len(nrow(station))
station$sampling_type <- 30          # spring survey
station$month <- 3
station$tow_number <- rep(1:12, 4)
station$gear_id <- 73
station$mfdb_gear_code <- "BMT"
# Station id as in mar: rectangle, tow number, gear
station$station <- (station$gridcell %/% 10) * 1e4 +
  station$tow_number * 100 + station$gear_id
station$tow_length <- round(runif(nrow(station), 3.5, 4.2), 2)
station$tow_depth <- round(runif(nrow(station), 100, 400))
station$fixed <- 1
station$begin_lat <- station$lat
station$begin_lon <- station$lon
station$end_lat <- station$lat + 0.05
station$end_lon <- station$lon
station$lat <- station$lon <- NULL
pax_import(pcon, station)

# Lengths (cm) of the fish of each tow: more and larger fish in later years
ldist <- do.call(rbind, lapply(seq_len(nrow(station)), function(i) {
  n <- rpois(1, 20 + 5 * (station$year[i] - 2021))
  if (n == 0) return(NULL)
  len <- round(rnorm(n, 35 + station$year[i] - 2021, 10))
  len <- table(len[len > 0])
  data.frame(
    sample_id = station$sample_id[i],
    species = 1,
    length = as.numeric(names(len)),
    sex = NA_real_,
    count = as.numeric(len)
  )
}))
pax_import(pcon, ldist)

# Length-weight coefficients (weight in g = a * length^b)
pax_import(pcon, data.frame(species = 1, a = 0.01, b = 3), name = "lw_coeffs")
```

In a database built by
[`pax_from_mar()`](https://hafro.github.io/pax/reference/pax_from_mar.md)
these tables (`station`, `ldist`, `lw_coeffs`, the strata and
`strata_stations`) are already there.

## Strata from tow positions

By default each tow goes in the stratum of the h3 cell of its first
position (the strata tables hold the h3 cells of each polygon):

``` r

by_length <- tbl(pcon, "station") |>
  filter(sampling_type == 30, coalesce(tow_number, 0) %in% 0:35) |>
  pax_si_by_length()

index_pos <- by_length |>
  pax_si_scale_by_strata("new_strata_spring") |>
  pax_si_strata_summary(length_range = c(0, 500)) |>
  pax_si_year_summary() |>
  collect() |>
  arrange(year)
index_pos |> select(year, si_N, si_biomass, si_biomass_cv)
#> # A tibble: 4 × 4
#>    year  si_N si_biomass si_biomass_cv
#>   <int> <dbl>      <dbl>         <dbl>
#> 1  2021     4      1708.        0.0739
#> 2  2022     4      2181.        0.0709
#> 3  2023     4      2808.        0.0640
#> 4  2024     4      4689.        0.120
```

`si_abund` is in thousands of fish and `si_biomass` in tonnes; `si_N` is
the number of strata with fish.
[`pax_si_strata_summary()`](https://hafro.github.io/pax/reference/pax_si.md)
is where the length range is applied, and
[`pax_si_year_summary()`](https://hafro.github.io/pax/reference/pax_si.md)
drops strata without fish.

## Strata from a fixed station list

The tidypax indices, and so the published ones, put each station in the
stratum of the fixed station list (`biota.strata_stations`), by station
id.
[`pax_from_mar()`](https://hafro.github.io/pax/reference/pax_from_mar.md)
imports the list as the table `strata_stations` (with the gillnet
survey’s `smn_strata` list too). Here is a toy list that agrees with the
positions except for one station near a stratum boundary, which the list
puts in stratum 1:

``` r

strata_stations <- by_length |>
  pax_si_scale_by_strata("new_strata_spring") |>
  ungroup() |>
  distinct(sampling_type, station, stratum) |>
  collect() |>
  mutate(stratification = "new_strata")
moved <- station$station[station$gridcell == 3202][1]
strata_stations$stratum[strata_stations$station == moved] <- 1
pax_import(pcon, strata_stations)
```

[`pax_si_strata_stations()`](https://hafro.github.io/pax/reference/pax_si_index.md)
reads the list of one stratification and adds the stratum area of each
survey (converted from km² to square nautical miles);
[`pax_si_scale_by_strata_stations()`](https://hafro.github.io/pax/reference/pax_si_index.md)
then uses it in place of
[`pax_si_scale_by_strata()`](https://hafro.github.io/pax/reference/pax_si.md):

``` r

ss <- pax_si_strata_stations(
  pcon,
  stratification = "new_strata",
  area_tables = c(`30` = "new_strata_spring")
)
head(ss, 3)
#> # A tibble: 3 × 4
#>   sampling_type station stratum  area
#>           <dbl>   <dbl>   <dbl> <dbl>
#> 1            30 3160273       5  727.
#> 2            30 3210473       1 2412.
#> 3            30 3710973       3 2226.

index_list <- by_length |>
  pax_si_scale_by_strata_stations(ss |> filter(sampling_type == 30)) |>
  pax_si_strata_summary(length_range = c(0, 500)) |>
  pax_si_year_summary() |>
  collect()

full_join(
  index_pos |> select(year, by_position = si_biomass),
  index_list |> select(year, by_list = si_biomass),
  by = "year"
) |>
  arrange(year)
#> # A tibble: 4 × 3
#>    year by_position by_list
#>   <int>       <dbl>   <dbl>
#> 1  2021       1708.   1654.
#> 2  2022       2181.   2114.
#> 3  2023       2808.   2831.
#> 4  2024       4689.   4526.
```

One station moved, and every year changes: the stratum it left and the
one it joined now have different numbers of stations. In the saithe
spring survey about 50 stations a year differ, which moved the index by
−47% to +64% in single years (03-sai). Use the station list when the
published index did.

`pax_si_scale_by_strata(strata_tbl, strata_stations = ...)` also takes a
station list, but takes the areas from the one strata table; with
[`pax_si_scale_by_strata_stations()`](https://hafro.github.io/pax/reference/pax_si_index.md)
each survey keeps its own areas (spring and autumn strata differ). The
hafroreports equivalent is `hr_si_scale_by_strata_stations()`, and
`hr_input_data_si_index(strata_name =, strata_stations =)` uses it, as
in 03-sai and 01-cod:

``` r

hafroreports::hr_input_data_si_index(
  pax_db,
  sampling_type = 30,
  tow_number = 0:35,
  strata_name = "old_strata",
  strata_stations = strata_stations |>
    dplyr::filter(sampling_type == 30, stratification == "old_strata")
)
```

## Indices by length range

The category 3 stocks (e.g. 04-whg, 12-rjr, 13-cas) use biomass indices
of length ranges with the fixed station list.
[`pax_si_length_index()`](https://hafro.github.io/pax/reference/pax_si_index.md)
selects the stations (survey, tows, gears, years, fixed stations) and
returns one index per length range:

``` r

pax_si_length_index(
  pcon,
  ss,
  length_ranges = list(total = c(0, 500), large = c(40, 500)),
  sampling_type = 30,
  tow_number = 0:35
) |>
  select(length_range, year, srN, b, b_cv)
#> # A tibble: 8 × 5
#>   length_range  year   srN     b   b_cv
#>   <chr>        <int> <dbl> <dbl>  <dbl>
#> 1 large         2021     4  937. 0.0558
#> 2 large         2022     4 1193. 0.134 
#> 3 large         2023     4 1871. 0.110 
#> 4 large         2024     4 3203. 0.143 
#> 5 total         2021     4 1654. 0.0488
#> 6 total         2022     4 2114. 0.103 
#> 7 total         2023     4 2831. 0.0548
#> 8 total         2024     4 4526. 0.105
```

Length ranges are exclusive at both ends: `c(40, 500)` counts fish of 41
cm and over (in whole cm), as tidypax did. A range written as “40 cm and
over” is `c(39, 500)`.

The other arguments mirror the stock repos:

- `tow_number = 0:35` (spring survey) or `0:75` (autumn survey);
  stations without a tow number count as tow 0;
- `gear_id = 77:78` for the autumn survey;
- `skip_years = 2011` for the autumn survey, which covered only part of
  the area that year;
- `fixed_only = TRUE` keeps the fixed stations (`fixed == 1`), as 04-whg
  does for the spring survey.

A year with stations but no fish in a length range has no row by default
(as tidypax). With `complete_years = TRUE` it gets an index of 0. This
matters for the rfb rule, which takes the last five rows of the index: a
missing year shifts the index ratio.

## Pitfalls

- **Tow filter.** `tow_number` is the tow of a survey station, but the
  haul number of a commercial sample. Never apply a survey tow filter to
  commercial samples (03-sai lost 8–14% of its samples a year that way).
- **Strata from positions vs the station list.** See above. Check which
  one the published index used.
- **Length ranges are exclusive.** `c(30, 500)` starts at 31 cm.
- **Gillnet survey (SMN, sampling type 34).** Stations are per net, 0.5
  nm per net; use the `smn_strata` station list (06-lin, 08-usk).
- **Large hauls.** Old scripts scaled named hauls down (e.g. to 5%); use
  `hr_input_data_si_index(haul_scalar = ...)` for that, not
  [`pax_si_scale_winsorize()`](https://hafro.github.io/pax/reference/pax_si.md),
  which is a different, generic rule: every station whose biomass is
  above the `q` quantile of its year is scaled down to that quantile.
  (Until October 2026 it took the quantile of a column that doesn’t
  exist, and changed nothing.)
- **The ldist table is already raised** to the counted fish at import,
  so don’t apply
  [`pax_ldist_scale_abund()`](https://hafro.github.io/pax/reference/pax_ldist.md)
  to it (see the vignette on length distributions).

## AI use

This vignette was drafted with Claude (Anthropic) in October 2026 and
has not yet been checked by a person. (MFRI policy on AI use.)
