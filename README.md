# pax

## Main features / differences to tidypax

### Local DuckDB database

The main difference between tidypax and pax is pax assumes a local [duckdb](https://duckdb.org/) database, which is populated with raw data.
Generally this data comes from the [mar](https://gitlab.hafogvatn.is:dev/mar) package, but any data.frame source is possible.

```r
pcon <- pax::pax_connect()
mar <- mar::connect_mar()

pax::pax_import(pax::pax_mar_station(mar, species = 2))

# We can then operate on the ingested station table
dplyr::tbl(pcon, "station") |>
  dplyr::filter(sampling_type %in% 30, coalesce(tow_number, 0) %in% 0:35)
```

As well as collating tables, pax will collect citations for data sources.

### Re-organised function naming

Besides the obvious ``pax_`` prefix, the following naming scheme has been retrofitted:

* ``pax_add_*``: Add a derived column to the data, generally a grouping column
* ``pax_*_scale_by_*``: Scale columns (e.g. ``si``) by joining to something else (e.g. ``alk``)
* ``pax_mar_*``: Import data from the [mar](https://gitlab.hafogvatn.is:dev/mar) package ready to be imported with ``pax_import``
* ``pax_def_*``: Hard-coded / data included in package
* ``pax_describe_*``: Add description columns for e.g. gear code
* ``pax_tar_*``: pax-related targets helpers
* ``pax_*_summary``: A table intended for presentation, rather than further processing
* ``pax_*_plot``: A ggplot2 object intended for presentation
* ``data_update_*``: Internal functions to regenerate stored data (e.g. strata, bathymetry table, etc)

In addition:

* All functions pipe a table through, and will never accept a ``pcon`` object directly. This allows you to store multiple imports and/or pre-filter/combine
* Column names have been standardarised to English

### Stratification / spatial operations

We use the [DuckDB Spatial extension](https://duckdb.org/docs/stable/core_extensions/spatial/overview) and [H3 extensions](https://duckdb.org/community_extensions/extensions/h3).
[H3 cells](https://h3geo.org/) divide the earth into numbered hexagons, and all spatial data has these pre-resolved by the same resolution, by default 8 (~0.7km^2).

This is used in ``pax_si_scale_by_strata()``, ``pax_add_ocean_depth_class()``,


Instead of the ``strata_stations`` table, we have [shapefiles](https://en.wikipedia.org/wiki/Shapefile) defining strata, these get broken up into h3 cells and ``pax_si_scale_by_strata()``, ``pax_add_ocean_depth_class()`` then join against these.

Previously, ``new_strata`` contained multiple overapping stratification schemes, based on sampling_type.
It has now been broken into 2 stratification schemes, spring & autumn:

```r
mapview::mapview(pax::pax_def_strata("new_strata_spring"), zcol = "stratum", burst = TRUE)
mapview::mapview(pax::pax_def_strata("new_strata_autumn"), zcol = "stratum", burst = TRUE)
```

...There are potentially more, but they are currently not saved as a shapefile.

Similarly, ``old_strata`` has all unused overlapping strata removed:

```r
mapview::mapview(pax::pax_def_strata("old_strata"), zcol = "stratum", burst = TRUE)
```

Unlike tidypax, ocean depth is fetched directly from NOAA via. ``pax_marmap_ocean_depth()``.
However, a default data set in the package is used when no lat/lon bounding area is supplied.
Depth data has to be imported like any other data source before querying:

```r
pax::pax_import(pcon, pax::pax_marmap_ocean_depth())
```

### ALK pre/post scaling

Instead of an implicit scaling by strata and optional post-scaling by same scale, the operations are now explicit:

```r
  dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type %in% c(1, 2, 8)) |>
    dplyr::mutate(mfdb_gear_code = coalesce(mfdb_gear_code, 'BMT')) |>
    pax_si_by_length() |>
    pax_si_scale_by_strata("old_strata") |>
    pax_si_scale_by_alk(
      tgroup = list(t1 = 1:6, t2 = 7:12),
      gear_group = list(...)
      alk = newpax_comm_alk
    ) |>
    pax_si_scale_by_landings(
      tgroup = list(t1 = 1:6, t2 = 7:12),
      gear_group = list(...)
    ) |>
```
