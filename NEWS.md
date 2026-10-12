# pax (development version)

## Inshore shrimp surveys (October 2026, branch `iag`)

For the IAG shrimp assessments (41-pra-arn first; Isafjardardjup,
Snaefellsnes and offshore shrimp to follow). Existing stocks are unchanged:
every new argument defaults to the old behaviour.

* `pax_from_mar()`: `station_columns` adds optional station columns
  (`pax_station_columns()`: `survey_area` (skiki), `survey_subarea`
  (fjardarreitur), `sample_name` (syni_heiti), station number,
  temperatures); `only_trips` keeps the stations of listed cruises;
  `survey_species` and three new `extra_tables`: `ldist_maturity` (length
  counts by sex and maturity stage, not raised), `sample_weight` (sample and
  catch weights from `biota.skalar_v`) and `landings_stock` (landings by
  stock code `kfteg` and by management stock from `agf.aflagrunnur_v`,
  aggregated, without vessels, sellers or buyers).
* `catch_weight` (`vigt_afla`) is usable in the shrimp surveys: for sampling
  types 31 and 37 it is positive in every sample with a sample weight, in
  all years (checked 12 October 2026); the zeros of the groundfish surveys
  since 2010 don't occur there.
* `pax_ldist_scale_catch_weight()` (as `mar::skala_med_afla()`),
  `pax_ldist_scale_counted()` (as `mar::skala_med_taldir()`),
  `pax_ldist_round_length()` (cm to mm classes, halves up as Oracle) and
  `pax_ldist_add_selection()` (a knife-edge ramp, e.g. the fishable shrimp).
* `pax_ldist_add_weight()`: `by` (e.g. species, sex and maturity stage, `NA`
  in the coefficients matching any value) and `missing = "stop"` or
  `"zero"` instead of the default a = 0.01, b = 3, which is wrong for
  shrimp lengths in mm. The default is unchanged.
* `pax_si_station_strata()`, `pax_si_catch_weight_biomass()` and
  `pax_si_stratified_index()`: the shrimp survey index by fjord sub-area, as
  the old `leidangur.r` scripts (sum of sub-area means, variance from the
  sub-areas). `pax_shrimp_towdims()` gives the shrimp tow standards (1 nm,
  14.9 m trawl, tows 0.01-8 nm).
* Package data `shrimp_strata` (sub-areas and areas of Arnarfjordur,
  Isafjardardjup by year, and Snaefellsnes) and `shrimp_offshore_strata` (42
  rectangles), with `pax_shrimp_strata()`.
* `shrimp_strata` stock `"isafjardardjup_february"`: the areas of the old
  Isafjardardjup February survey script (they differ slightly from the
  autumn areas), for the February survey figures of 41-pra-isa.
* `pax_si_catch_weight_biomass(tow_length_missing = "zero")`: a station
  without a tow length counts as a zero in the stratum mean, as in the old
  shrimp scripts (`mar::skala_med_toglengd()` gives `NA`, then `nvl(N, 0)`);
  `"one"` takes a 1 nm tow as `pax_ldist_scale_tow_area()`. Five
  Isafjardardjup stations (gear 30, 1989 and 1993) have no tow length; there
  are none in Arnarfjordur, so 41-pra-arn is unchanged.

AI use: Claude (Anthropic) wrote these functions, their tests and this entry
(12 October 2026). The 41-pra-arn pipeline built on them reproduces the old
Arnarfjordur code run on the same database; nobody has reviewed the code
yet.

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
||||||| parent of 637ca75 (Shrimp survey index by fjord sub-area: catch-weight raising, L-W by maturity, fishable ramp, strata data)

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
