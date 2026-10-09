# Connect to a pax DuckDB database

Create or open a DuckDB database for use with pax, installing and
loading the required `spatial` and `h3` extensions as needed.

## Usage

``` r
pax_connect(dbdir = ":memory:", read_only = FALSE, h3_resolution = 8)
```

## Arguments

- dbdir:

  Path to a DuckDB database file, or `":memory:"` for an in-memory
  database

- read_only:

  Boolean, open the database in read-only mode?

- h3_resolution:

  H3 cell resolution to use for spatial indexing, see
  <https://h3geo.org/docs/core-library/restable/>

## Value

A DBI database connection object
