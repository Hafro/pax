# Make a table available as a DuckDB query

Converts an R data.frame or string table reference into a dplyr SQL
query against the pax database. If the input is already a database
query, it is returned unchanged. String names beginning with `paxdat_`
refer to package-internal datasets which are loaded on demand.

## Usage

``` r
pax_temptbl(pcon, tbl)
```

## Arguments

- pcon:

  A pax DBI connection, as returned by
  [`pax_connect()`](https://hafro.github.io/pax/reference/pax_connect.md)

- tbl:

  A data.frame, a table name string, or an existing dplyr SQL query

## Value

A dplyr SQL query referencing the table within `pcon`
