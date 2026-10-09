# targets format helpers for pax objects

Custom `targets` storage formats for pax database connections and
data.frames, for use with
[`targets::tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html).

## Usage

``` r
pax_tar_format_duckdb()

pax_tar_format_parquet()
```

## Value

### pax_tar_format_duckdb

A targets format object that reads and writes a pax DuckDB connection to
a file on disk

### pax_tar_format_parquet

A targets format object that reads and writes a data.frame to a Parquet
file, automatically dropping geometry (`geom`) and H3 cell array
(`h3_cells`) columns
