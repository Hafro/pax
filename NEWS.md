# pax (development version)

## Bug fixes (review, October 2026)

* `pax_si_scale_winsorize()` took the quantile of a column `B` that doesn't
  exist (a leftover from tidypax's `si_winsorize()`), so no station was ever
  above it and nothing changed. It now takes the `q` quantile of the station
  biomass (`si_biomass` summed by `sample_id`) within each year and species,
  and scales every station above it down to the quantile (quantile / station
  biomass, as the old 22-ghl and 07-bli scripts; tidypax scaled to the
  smallest station above the quantile instead).
* `pax_landings_fishingyear_summary()`: the column `catch_kt` is renamed
  `catch_t`. The value, `round(sum(catch) / 1000)`, is in tonnes when
  `catch` is in kg (as in the `landings` table), not in thousand tonnes.
* The default `gear_group` of `pax_ldist_alk()`, `pax_si_scale_by_landings()`
  and `pax_landings_by_gear()` had `Other = 'Var'`, but the gear code is
  `'VAR'` (`gear_mapping`) and the join is case-sensitive, so 'Var' matched
  nothing. It is now `'VAR'`. `pax_landings_by_gear()` gives the same result
  (its default also has a `pax_add_other()` group, which took VAR before);
  with the defaults of `pax_ldist_alk()` and `pax_si_scale_by_landings()`,
  VAR samples and landings now form the "Other" group instead of getting no
  group.

## Survey indices by length range (October 2026)

* New `pax_si_strata_stations()`, `pax_si_scale_by_strata_stations()`,
  `pax_si_by_strata()` and `pax_si_length_index()`: the survey indices by
  length range of the category 3 and SPiCT stocks (04-whg, 12-rjr, 13-cas,
  14-mon, 24-lem, 25-wit, 26-meg, 27-dab, 28-pla, 60-norway-redfish), which
  each kept a copy of the same `*_survey_strata()` / `*_survey_index()`
  wrappers. Stations get their stratum from the fixed station list
  (`strata_stations`), each survey has its own stratum areas, and length
  ranges have exclusive ends, as tidypax. The stock differences are
  arguments: `tow_number` (or `NULL` for all tows), `gear_id`, `skip_years`,
  `fixed_only`, and `complete_years` (26-meg: a year without fish in the
  range gets an index of 0). The station table can be given as a query, so
  a stock can add columns first.
* `pax_si_scale_by_strata_stations()` is `hafroreports::hr_si_scale_by_strata_stations()`
  with the stratum areas given in the station list, so that the index does
  not need hafroreports.

AI use: Claude (Anthropic) wrote these functions, their tests and this entry
from the stock repositories' wrappers (9 October 2026); the results were
checked by script against the stocks' targets (identical on the same
database snapshot).
