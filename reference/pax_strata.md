# Strata definitions

Functions to retrieve the coordinate reference system and strata
shapefiles bundled with the pax package.

## Usage

``` r
pax_def_crs()

pax_def_strata_list()

pax_def_strata(strata_name)
```

## Arguments

- strata_name:

  A strata name, as returned by `pax_def_strata_list()`

## Value

### pax_def_crs

The WGS84 coordinate reference system (EPSG:4326) as an sf CRS object

### pax_def_strata_list

A character vector of strata names available in the package, suitable
for use as the `strata` argument to
[`pax_from_mar()`](https://hafro.github.io/pax/reference/pax_from_mar.md)

### pax_def_strata

An sf data.frame of strata polygons read from the bundled shapefile,
decorated with `pax_name` for use with
[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md)
