# Import a table into a pax database

Import a data.frame, sf spatial object, or CSV file into a pax DuckDB
database. Geometry and H3 spatial index columns are added automatically
when the data contains a geometry column, `lat`/`lon` columns, or
`begin_lat`/`begin_lon`/`end_lat`/`end_lon` columns.

## Usage

``` r
pax_import(
  pcon,
  tbl,
  overwrite = FALSE,
  name = attr(tbl, "pax_name"),
  cite = attr(tbl, "pax_cite")
)
```

## Arguments

- pcon:

  A pax DBI connection, as returned by
  [`pax_connect()`](https://hafro.github.io/pax/reference/pax_connect.md)

- tbl:

  Data to import: a data.frame, sf object, dplyr query, or path to a CSV
  file

- overwrite:

  Boolean, overwrite an existing table with the same name?

- name:

  Name to use for the imported table. Defaults to the variable name of
  `tbl`, or the `pax_name` attribute set by
  [`pax_decorate()`](https://hafro.github.io/pax/reference/pax_decorate.md)

- cite:

  Citation string for the data source. Defaults to the `pax_cite`
  attribute set by
  [`pax_decorate()`](https://hafro.github.io/pax/reference/pax_decorate.md)

## Value

Invisibly returns `NULL`
