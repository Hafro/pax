# NOAA bathymetry grid

Ocean depth from NOAA on a regular grid of 4 x 4 minutes, 80 W - 50 E
and 30 N - 80 N (as the NOAA grids of
[`marmap::getNOAA.bathy()`](https://rdrr.io/pkg/marmap/man/getNOAA.bathy.html)),
with the statistical rectangle and subrectangle (gridcell) of each
point. A lookup grid, used e.g. to fill missing depths with the mean
depth of the gridcell and for depth contours on maps.

## Format

A data.frame with columns `x` (longitude), `y` (latitude), `z` (depth,
m, negative below sea level), `reitur` (rectangle) and `smareitur`
(gridcell, 10 x rectangle + subrectangle)

## Source

`ops$bthe."noaa_bathymetry"` in mar, extracted 8 October 2026 by
`pax:::data_update_noaa_bathymetry()`
