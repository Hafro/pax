# Gear code (veidarfaeri) to MFDB gear code mapping

The mapping of biota and landings gear codes (`veidarfaeri`, the
`gear_id` of the pax station and landings tables) to MFDB gear codes.

## Format

A data.frame with columns

- gear_id:

  Gear code (`veidarfaeri`)

- mfdb_gear_code:

  MFDB gear code, as `biota.gear_mapping`, with gear 91 (anglerfish
  gillnet) added as `GIL`

- source:

  `"biota.gear_mapping"`, or `"pax"` for gear 91, which is missing from
  `biota.gear_mapping`. Rows with `source == "biota.gear_mapping"`
  reproduce that table exactly

- mfdb_gear_code_weight:

  MFDB gear code of `ops$bthe."gear_mapping"`, used by the old cod code
  for the weights at length of the commercial samples

## Source

`biota.gear_mapping` and `ops$bthe."gear_mapping"` in mar, extracted 8
October 2026 by `pax:::data_update_gear_mapping()`
