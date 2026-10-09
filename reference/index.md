# Package index

## The pax database

Create, open and fill a pax DuckDB database, and keep it in targets.

- [`pax_connect()`](https://hafro.github.io/pax/reference/pax_connect.md)
  : Connect to a pax DuckDB database
- [`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md)
  : Import a table into a pax database
- [`pax_contents()`](https://hafro.github.io/pax/reference/pax_contents.md)
  : List contents of a pax database
- [`pax_decorate()`](https://hafro.github.io/pax/reference/pax_decorate.md)
  : Attach metadata to a table for use with pax_import
- [`pax_temptbl()`](https://hafro.github.io/pax/reference/pax_temptbl.md)
  : Make a table available as a DuckDB query
- [`pax_from_mar()`](https://hafro.github.io/pax/reference/pax_from_mar.md)
  [`pax_from_mar_extra_tables()`](https://hafro.github.io/pax/reference/pax_from_mar.md)
  : Create a pax database populated from the MAR database
- [`pax_tar_format_duckdb()`](https://hafro.github.io/pax/reference/pax_tar_format.md)
  [`pax_tar_format_parquet()`](https://hafro.github.io/pax/reference/pax_tar_format.md)
  : targets format helpers for pax objects

## Import from mar

Tables of the MFRI database, ready for
[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md)
(need the mar package).

- [`pax_mar_logbook()`](https://hafro.github.io/pax/reference/pax_mar.md)
  [`pax_mar_landings()`](https://hafro.github.io/pax/reference/pax_mar.md)
  [`pax_mar_ldist()`](https://hafro.github.io/pax/reference/pax_mar.md)
  [`pax_mar_aldist()`](https://hafro.github.io/pax/reference/pax_mar.md)
  [`pax_mar_lw_coeffs()`](https://hafro.github.io/pax/reference/pax_mar.md)
  [`pax_mar_measurement()`](https://hafro.github.io/pax/reference/pax_mar.md)
  [`pax_mar_quotatransfer()`](https://hafro.github.io/pax/reference/pax_mar.md)
  [`pax_mar_sampling()`](https://hafro.github.io/pax/reference/pax_mar.md)
  [`pax_mar_station()`](https://hafro.github.io/pax/reference/pax_mar.md)
  [`pax_mar_strata_stations()`](https://hafro.github.io/pax/reference/pax_mar.md)
  : Import tables from the MAR database
- [`pax_mar_landings_vessel()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)
  [`pax_mar_vessel()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)
  [`pax_mar_logbook_release()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)
  [`pax_mar_research_landings()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)
  [`pax_mar_catch_disposition()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)
  [`pax_mar_landings_old()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)
  [`pax_mar_sample()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)
  [`pax_mar_logbook_old()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)
  : Import optional tables from the MAR database
- [`pax_quotatransfer_summary()`](https://hafro.github.io/pax/reference/pax_quotatransfer.md)
  [`pax_quotatransfer_plot()`](https://hafro.github.io/pax/reference/pax_quotatransfer.md)
  : Quota transfer summaries and plots

## Foreign data

Greenland, Faroese and German data, read when the database is built.

- [`pax_foreign_mar_tables()`](https://hafro.github.io/pax/reference/pax_foreign.md)
  [`pax_mar_foreign()`](https://hafro.github.io/pax/reference/pax_foreign.md)
  [`pax_foreign_file_types()`](https://hafro.github.io/pax/reference/pax_foreign.md)
  [`pax_foreign_file()`](https://hafro.github.io/pax/reference/pax_foreign.md)
  : Import foreign (non-MFRI) data
- [`pax_ger_survey()`](https://hafro.github.io/pax/reference/pax_ger.md)
  [`pax_ger_station_fix()`](https://hafro.github.io/pax/reference/pax_ger.md)
  [`pax_ger_ldist_impute()`](https://hafro.github.io/pax/reference/pax_ger.md)
  [`pax_ger_strata()`](https://hafro.github.io/pax/reference/pax_ger.md)
  : German (Walther Herwig) Greenland groundfish survey

## Survey indices

- [`pax_si_scale_by_alk()`](https://hafro.github.io/pax/reference/pax_si.md)
  [`pax_si_scale_by_landings()`](https://hafro.github.io/pax/reference/pax_si.md)
  [`pax_si_by_length()`](https://hafro.github.io/pax/reference/pax_si.md)
  [`pax_si_scale_winsorize()`](https://hafro.github.io/pax/reference/pax_si.md)
  [`pax_si_scale_by_strata()`](https://hafro.github.io/pax/reference/pax_si.md)
  [`pax_si_strata_summary()`](https://hafro.github.io/pax/reference/pax_si.md)
  [`pax_si_year_summary()`](https://hafro.github.io/pax/reference/pax_si.md)
  : Survey index computation functions
- [`pax_si_strata_stations()`](https://hafro.github.io/pax/reference/pax_si_index.md)
  [`pax_si_scale_by_strata_stations()`](https://hafro.github.io/pax/reference/pax_si_index.md)
  [`pax_si_by_strata()`](https://hafro.github.io/pax/reference/pax_si_index.md)
  [`pax_si_length_index()`](https://hafro.github.io/pax/reference/pax_si_index.md)
  : Survey indices by length range from a fixed station list
- [`pax_station_location_summary()`](https://hafro.github.io/pax/reference/pax_station_location_summary.md)
  : Summarise station locations and catch

## Length distributions and age-length keys

- [`pax_ldist_alk()`](https://hafro.github.io/pax/reference/pax_ldist.md)
  [`pax_ldist_scale_round()`](https://hafro.github.io/pax/reference/pax_ldist.md)
  [`pax_ldist_add_weight()`](https://hafro.github.io/pax/reference/pax_ldist.md)
  [`pax_ldist_scale_tow_area()`](https://hafro.github.io/pax/reference/pax_ldist.md)
  [`pax_ldist_by_year()`](https://hafro.github.io/pax/reference/pax_ldist.md)
  [`pax_ldist_scale_abund()`](https://hafro.github.io/pax/reference/pax_ldist.md)
  [`pax_ldist_plot()`](https://hafro.github.io/pax/reference/pax_ldist.md)
  [`pax_ldist_joy_plot()`](https://hafro.github.io/pax/reference/pax_ldist.md)
  : Length distribution functions
- [`pax_measurement_agelen_summary()`](https://hafro.github.io/pax/reference/pax_measurement.md)
  [`pax_measurement_type_summary()`](https://hafro.github.io/pax/reference/pax_measurement.md)
  : Summarise measurement data
- [`pax_sampling_detail()`](https://hafro.github.io/pax/reference/pax_sampling.md)
  [`pax_sampling_age_reading_status()`](https://hafro.github.io/pax/reference/pax_sampling.md)
  : Summarise and visualise sampling data

## Landings and logbooks

- [`pax_landings_by_gear()`](https://hafro.github.io/pax/reference/pax_landings.md)
  [`pax_landings_boat_summary()`](https://hafro.github.io/pax/reference/pax_landings.md)
  [`pax_landings_significantboats_summary()`](https://hafro.github.io/pax/reference/pax_landings.md)
  [`pax_add_fishing_year()`](https://hafro.github.io/pax/reference/pax_landings.md)
  [`pax_landings_fishingyear_summary()`](https://hafro.github.io/pax/reference/pax_landings.md)
  : Summarise and visualise landings data
- [`pax_add_cpue()`](https://hafro.github.io/pax/reference/pax_logbook.md)
  [`pax_logbook_cpue_plot()`](https://hafro.github.io/pax/reference/pax_logbook.md)
  : Create CPUE plot from logbook
- [`pax_fill_mfdb_gear_code()`](https://hafro.github.io/pax/reference/pax_fill_mfdb_gear_code.md)
  : Fill missing MFDB gear codes from the gear mapping

## Groupings and descriptions

Bin rows by length, region, gear, season, year and depth, and label
codes.

- [`pax_add_groupings()`](https://hafro.github.io/pax/reference/pax_add_groupings.md)
  [`pax_def_groupings()`](https://hafro.github.io/pax/reference/pax_add_groupings.md)
  [`pax_add_lgroups()`](https://hafro.github.io/pax/reference/pax_add_groupings.md)
  [`pax_add_regions()`](https://hafro.github.io/pax/reference/pax_add_groupings.md)
  [`pax_add_ocean_depth_class()`](https://hafro.github.io/pax/reference/pax_add_groupings.md)
  [`pax_add_gear_group()`](https://hafro.github.io/pax/reference/pax_add_groupings.md)
  [`pax_add_temporal_grouping()`](https://hafro.github.io/pax/reference/pax_add_groupings.md)
  [`pax_add_yearly_grouping()`](https://hafro.github.io/pax/reference/pax_add_groupings.md)
  [`pax_add_other()`](https://hafro.github.io/pax/reference/pax_add_groupings.md)
  : Add multiple groupings simulataneously
- [`pax_describe_sampling_type()`](https://hafro.github.io/pax/reference/pax_describe.md)
  [`pax_describe_mfdb_gear_code()`](https://hafro.github.io/pax/reference/pax_describe.md)
  : Add description columns for known vocabularies

## Bootstrap

- [`pax_bootstrap_group()`](https://hafro.github.io/pax/reference/pax_bootstrap.md)
  [`pax_bootstrap_table()`](https://hafro.github.io/pax/reference/pax_bootstrap.md)
  [`pax_bootstrap_resample()`](https://hafro.github.io/pax/reference/pax_bootstrap.md)
  [`pax_bootstrap_lognormal()`](https://hafro.github.io/pax/reference/pax_bootstrap.md)
  : Spatial bootstrap of survey and commercial samples

## Strata, depth and maps

- [`pax_def_crs()`](https://hafro.github.io/pax/reference/pax_strata.md)
  [`pax_def_strata_list()`](https://hafro.github.io/pax/reference/pax_strata.md)
  [`pax_def_strata()`](https://hafro.github.io/pax/reference/pax_strata.md)
  : Strata definitions
- [`pax_marmap_ocean_depth()`](https://hafro.github.io/pax/reference/pax_marmap_ocean_depth.md)
  : Fetch and aggregate ocean depth data
- [`pax_map_base()`](https://hafro.github.io/pax/reference/pax_map.md)
  [`pax_map_layer_depth()`](https://hafro.github.io/pax/reference/pax_map.md)
  [`pax_map_layer_catch()`](https://hafro.github.io/pax/reference/pax_map.md)
  : Map plotting functions

## Package data

Lookup tables shipped with pax.

- [`gear_mapping`](https://hafro.github.io/pax/reference/gear_mapping.md)
  : Gear code (veidarfaeri) to MFDB gear code mapping
- [`mfdb_gear_code_desc`](https://hafro.github.io/pax/reference/mfdb_gear_code_desc.md)
  : Lookup table of MFDB gear code descriptions
- [`sampling_type_desc`](https://hafro.github.io/pax/reference/sampling_type_desc.md)
  : Lookup table of sampling type descriptions
- [`gridcell`](https://hafro.github.io/pax/reference/gridcell.md) :
  Gridcell / division / subdivision mapping
- [`reitmapping`](https://hafro.github.io/pax/reference/reitmapping.md)
  : Statistical rectangle mapping (reitmapping)
- [`noaa_bathymetry`](https://hafro.github.io/pax/reference/noaa_bathymetry.md)
  : NOAA bathymetry grid
- [`raw_ocean_depth_defbounds`](https://hafro.github.io/pax/reference/raw_ocean_depth_defbounds.md)
  : Cached ocean bathymetry for default area bounds
- [`diel_correction_reg`](https://hafro.github.io/pax/reference/diel_correction_reg.md)
  : Diel (time of day) correction of the golden redfish survey indices
- [`stationlist_hafro_smb`](https://hafro.github.io/pax/reference/stationlist_hafro_smb.md)
  : Station tables from the SMB survey handbook
- [`stationlist_hafro_smh_deep`](https://hafro.github.io/pax/reference/stationlist_hafro_smh_deep.md)
  : Station tables from the SMH survey handbook - deep survey
- [`stationlist_hafro_smh_shelf`](https://hafro.github.io/pax/reference/stationlist_hafro_smh_shelf.md)
  : Station tables from the SMH survey handbook - shelf survey
