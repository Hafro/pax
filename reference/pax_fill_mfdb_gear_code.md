# Fill missing MFDB gear codes from the gear mapping

Gives rows with a missing `mfdb_gear_code` the MFDB gear code of their
gear (`gear_id`) in the
[gear_mapping](https://hafro.github.io/pax/reference/gear_mapping.md)
dataset, e.g. gear 91 (anglerfish gillnet), which `biota.gear_mapping`
leaves unmapped, as `GIL`. Other rows are unchanged.

## Usage

``` r
pax_fill_mfdb_gear_code(
  tbl,
  gear_mapping_tbl = pax_temptbl(dbplyr::remote_con(tbl), "paxdat_gear_mapping")
)
```

## Arguments

- tbl:

  A dplyr query with `gear_id` and `mfdb_gear_code` columns, e.g. the
  pax `station` or `landings` table

- gear_mapping_tbl:

  The gear mapping, by default the
  [gear_mapping](https://hafro.github.io/pax/reference/gear_mapping.md)
  dataset

## Value

`tbl` with `mfdb_gear_code` filled where it was missing
