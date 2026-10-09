# Summarise measurement data

Functions to extract structured summaries from a measurement table.

## Usage

``` r
pax_measurement_agelen_summary(tbl)

pax_measurement_type_summary(tbl)
```

## Arguments

- tbl:

  A dplyr query from a measurement table, as returned by
  [`pax_mar_measurement()`](https://hafro.github.io/pax/reference/pax_mar.md)

## Value

### pax_measurement_agelen_summary

A dplyr query of otolith-aged individuals, with columns `species`,
`sample_id`, `measurement_id`, `age`, `maturity_stage`, `length`,
`weight`, and `count`

### pax_measurement_type_summary

A dplyr query aggregated by `sample_id` with count columns for each
measurement type: `n_total`, `n_LENC`, `n_CNT`, `n_LENM`, `n_SAMP`,
`n_OTOL`, `n_LEN`, `n_CATC`, and `n_TOTC`
