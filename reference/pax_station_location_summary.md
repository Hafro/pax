# Summarise station locations and catch

Joins station data to length distributions to produce per-station
biomass estimates, used to visualise survey coverage and zero-catch
stations.

## Usage

``` r
pax_station_location_summary(
  tbl,
  ldist = pax_ldist_add_weight(dplyr::tbl(dbplyr::remote_con(tbl), "ldist"))
)
```

## Arguments

- tbl:

  A dplyr query from the station table

- ldist:

  A dplyr query from the ldist table, pre-processed with
  [`pax_ldist_add_weight()`](https://hafro.github.io/pax/reference/pax_ldist.md).
  The ldist table from
  [`pax_mar_ldist()`](https://hafro.github.io/pax/reference/pax_mar.md)
  is already raised to the counted fish, so it is not scaled again

## Value

A dplyr query with columns `sample_id`, `begin_lat`, `begin_lon`,
`year`, `sampling_type`, `species`, `bio` (biomass index per station),
and `zero_station` (`"Zero catch"` or `"Non zero"`)
