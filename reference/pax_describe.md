# Add description columns for known vocabularies

Add columns containing descriptions to a dplyr query

## Usage

``` r
pax_describe_sampling_type(tbl, lang = getOption("pax.lang", "en"))

pax_describe_mfdb_gear_code(tbl, lang = getOption("pax.lang", "en"))
```

## Arguments

- tbl:

  A dplyr query to apply descriptions to

- lang:

  Desired language for descriptions, either `en` or `is`.

## Value

### pax_describe_sampling_type

A dplyr query, with a `sampling_type_desc` column describing
`sampling_type` values

### pax_describe_mfdb_gear_code

A dplyr query, with a `mfdb_gear_code_desc` column describing
`mfdb_gear_code` values
