# Changelog

## pax (development version)

### Survey indices by length range (October 2026)

- New
  [`pax_si_strata_stations()`](https://hafro.github.io/pax/reference/pax_si_index.md),
  [`pax_si_scale_by_strata_stations()`](https://hafro.github.io/pax/reference/pax_si_index.md),
  [`pax_si_by_strata()`](https://hafro.github.io/pax/reference/pax_si_index.md)
  and
  [`pax_si_length_index()`](https://hafro.github.io/pax/reference/pax_si_index.md):
  the survey indices by length range of the category 3 and SPiCT stocks
  (04-whg, 12-rjr, 13-cas, 14-mon, 24-lem, 25-wit, 26-meg, 27-dab,
  28-pla, 60-norway-redfish), which each kept a copy of the same
  `*_survey_strata()` / `*_survey_index()` wrappers. Stations get their
  stratum from the fixed station list (`strata_stations`), each survey
  has its own stratum areas, and length ranges have exclusive ends, as
  tidypax. The stock differences are arguments: `tow_number` (or `NULL`
  for all tows), `gear_id`, `skip_years`, `fixed_only`, and
  `complete_years` (26-meg: a year without fish in the range gets an
  index of 0). The station table can be given as a query, so a stock can
  add columns first.
- [`pax_si_scale_by_strata_stations()`](https://hafro.github.io/pax/reference/pax_si_index.md)
  is `hafroreports::hr_si_scale_by_strata_stations()` with the stratum
  areas given in the station list, so that the index does not need
  hafroreports.

AI use: Claude (Anthropic) wrote these functions, their tests and this
entry from the stock repositories’ wrappers (9 October 2026); the
results were checked by script against the stocks’ targets (identical on
the same database snapshot).
