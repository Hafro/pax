# List contents of a pax database

Returns a dplyr query of all tables that have been imported into the
database with
[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md).

## Usage

``` r
pax_contents(pcon)
```

## Arguments

- pcon:

  A pax DBI connection, as returned by
  [`pax_connect()`](https://hafro.github.io/pax/reference/pax_connect.md)

## Value

A dplyr query of the `pax_citation` table, with columns `tbl_name` and
`citation`
