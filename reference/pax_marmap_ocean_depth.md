# Fetch and aggregate ocean depth data

Retrieves bathymetric data via the marmap package (or uses the
pre-cached package dataset when no bounds are given) and aggregates it
to H3 cells at the current database resolution.

## Usage

``` r
pax_marmap_ocean_depth(lat1 = NULL, lon1 = NULL, lat2 = NULL, lon2 = NULL)
```

## Arguments

- lat1, lon1:

  Minimum latitude and longitude of the bounding box. When all four
  bounds are `NULL` the bundled default-bounds dataset is used instead
  of fetching from NOAA.

- lat2, lon2:

  Maximum latitude and longitude of the bounding box

## Value

A data.frame with columns `lon`, `lat`, `ocean_depth`, and `h3_cell`,
decorated with `pax_name = "ocean_depth"` for use with
[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md)
