# Attach metadata to a table for use with pax_import

Add citation and/or name attributes to a data.frame or dplyr query.
These attributes are used as defaults by
[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md).

## Usage

``` r
pax_decorate(tbl, cite = deparse1(sys.call(-1)), name = NULL)
```

## Arguments

- tbl:

  A data.frame or dplyr query to decorate

- cite:

  Citation string for the data source. Defaults to the calling
  expression

- name:

  Table name to use when importing, or `NULL` to leave unset

## Value

`tbl` with `pax_cite` and/or `pax_name` attributes attached
