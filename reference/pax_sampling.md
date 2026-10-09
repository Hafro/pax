# Summarise and visualise sampling data

Functions to extract position summaries, overview plots, and data
quality tables from commercial sampling data.

## Usage

``` r
pax_sampling_detail(
  tbl,
  mfdb_gear_code = c("BMT", "LLN", "DSE"),
  sampling_type = c(1, 2, 3, 4, 8),
  measurement_type = c("LEN", "LENM", "OTOL")
)

pax_sampling_age_reading_status(tbl, measurement_type = c("OTOL"))
```

## Arguments

- tbl:

  A dplyr query from a sampling table, as returned by
  [`pax_mar_sampling()`](https://hafro.github.io/pax/reference/pax_mar.md)

- mfdb_gear_code:

  Character vector of gear codes to include

- sampling_type:

  Integer vector of sampling type codes to include

- measurement_type:

  Character vector of measurement types to include

## Value

### pax_sampling_detail

A data.frame wide-pivoted by gear, with columns `year` and per-gear
columns for number of samples (`n`), total lengths (`n_lengths`), and
otolith readings (`n_otol`)

### pax_sampling_age_reading_status

A dplyr query with columns `year`, `species`, `sampling_type`, `total`
(number of otolith samples), `read` (number with an age assigned), and
`p` (proportion read)
