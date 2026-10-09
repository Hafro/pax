# Quota transfer summaries and plots

Functions to tabulate and visualise quota transfer data.

## Usage

``` r
pax_quotatransfer_summary(tbl)

pax_quotatransfer_plot(tbl)
```

## Arguments

- tbl:

  A dplyr query from a quota transfer table, as returned by
  [`pax_mar_quotatransfer()`](https://hafro.github.io/pax/reference/pax_mar.md)

## Value

### pax_quotatransfer_summary

A dplyr query with columns `Period`, `TAC`, `Catch`, `Diff`, `TACtrans`,
and `Diff_trans`

### pax_quotatransfer_plot

A ggplot2 four-panel bar chart showing between-year and between-species
quota transfers in absolute and percentage terms
