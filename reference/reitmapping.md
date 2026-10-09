# Statistical rectangle mapping (reitmapping)

The full gridcell (subrectangle) to division and subdivision mapping, as
used for the region maps of the tech reports.
[gridcell](https://hafro.github.io/pax/reference/gridcell.md) is the
subset of rows with a gridcell, division, subdivision and position.

## Format

A data.frame with columns `id`, `gridcell` (10 x rectangle +
subrectangle), `division`, `subdivision`, `lat` and `lon` (centre of the
gridcell) and `size` (area, square nautical miles)

## Source

`ops$bthe."reitmapping"` in mar, extracted 8 October 2026 by
`pax:::data_update_reitmapping()`
