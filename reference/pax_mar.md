# Import tables from the MAR database

Functions to extract and standardise individual tables from the Hafro
MAR Oracle database into pax-compatible data.frames. The returned tables
carry `pax_name` and `pax_cite` attributes set by
[`pax_decorate()`](https://hafro.github.io/pax/reference/pax_decorate.md)
and can be passed directly to
[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md).

## Usage

``` r
pax_mar_logbook(mar, species, year_start = NULL, year_end = NULL)

pax_mar_landings(
  mar,
  species,
  ices_area_like = "5a%",
  year_start = NULL,
  year_end = NULL
)

pax_mar_ldist(mar, species)

pax_mar_aldist(mar, species)

pax_mar_lw_coeffs(mar, species)

pax_mar_measurement(
  mar,
  species,
  year_start = NULL,
  year_end = NULL,
  measurement_type = NULL
)

pax_mar_quotatransfer(mar, species, quota_species = species)

pax_mar_sampling(
  mar,
  species,
  year_start = NULL,
  year_end = NULL,
  mfdb_gear_code = c("BMT", "LLN", "DSE"),
  sampling_type = c(1, 2, 3, 4, 8),
  skip_trips = c("MAG%", "MO%")
)

pax_mar_station(
  mar,
  species = NULL,
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL,
  skip_trips = c("MAG%", "MO%"),
  gridcell_from_position = FALSE,
  only_trips = NULL
)

pax_mar_strata_stations(mar)
```

## Arguments

- mar:

  A MAR database connection, as returned by `mar::connect_mar()`

- species:

  Integer vector of species codes to filter by

- year_start:

  Optional integer, earliest year to include

- year_end:

  Optional integer, latest year to include

- ices_area_like:

  Character vector of SQL LIKE patterns for filtering ICES areas, e.g.
  `"5a%"`

- measurement_type:

  Character vector of measurement types to include, e.g.
  `c("LEN", "OTOL")`

- quota_species:

  Quota species codes (`fteg`) of the quota transfers, when they differ
  from the species codes (e.g. `95` for demersal beaked redfish, species
  61). Default `species`

- mfdb_gear_code:

  Character vector of gear codes to include, `NULL` for all gears
  (including samples with unknown gear)

- sampling_type:

  Integer vector of sampling type codes to include

- skip_trips:

  SQL LIKE patterns of trips (`leidangur`) to leave out. By default the
  stomach-sampling trips `MAG*` and `MO*` (MAGEI, MOGUN), which should
  be a separate sampling type. `NULL` keeps all trips, as tidypax
  `si_stations()` did

- gridcell_from_position:

  If `TRUE`, stations without a rectangle or subrectangle (`reitur`,
  `smareitur`) get the gridcell of their position (as `geo::d2sr()`).
  Otherwise (default) their gridcell is `NA`, they get no region, and
  e.g. match no age-length key

- only_trips:

  SQL LIKE patterns of trips to keep, `NULL` (default) for all. With
  `skip_trips = NULL` and `only_trips = c("MAG%", "MO%")`, the stations
  of the stomach-sampling trips, see the `extra_tables` of
  [`pax_from_mar()`](https://hafro.github.io/pax/reference/pax_from_mar.md)

## Value

### pax_mar_logbook

A dplyr query with columns `logbook_id`, `species`, `year`, `month`,
`vessel_nr`, `mfdb_gear_code`, `gear_size`, `gridcell`, `lat`, `lon`,
`tow_area`, `tow_time`, `tow_hooks`, `tow_num_nets`, `tow_num_traps`,
`ocean_depth`, `catch`, and `catch_total`

### pax_mar_landings

A dplyr query with columns `year`, `month`, `species`, `ices_area`,
`country`, `mfdb_gear_code`, `gear_id` (landings register gear code,
`veidarfaeri`), `boat_id`, and `catch`

### pax_mar_ldist

A dplyr query with columns `sample_id`, `species`, `length`, `sex`, and
`count`

### pax_mar_aldist

A dplyr query with columns `sample_id`, `species`, `length`, `weight`,
`age`, and `count`

### pax_mar_lw_coeffs

A dplyr query of length-weight coefficients, filtered to the requested
species

### pax_mar_measurement

A dplyr query with columns `individual_id`, `sample_id`, `species`,
`measurement_type`, `length`, `age`, `sex`, `maturity_stage`,
`weight_g`, `gonad_weight`, `gut_weight`, `liver_weight`, and `count`

### pax_mar_quotatransfer

A data.frame of quota transfer records for the requested species,
arranged by species and period

### pax_mar_sampling

A dplyr query with columns `sample_id`, `lat`, `lon`, `year`, `month`,
`sampling_type`, `mfdb_gear_code`, and `trip`, filtered to samples with
length measurements for the requested species

### pax_mar_station

A dplyr query with columns `sample_id`, `haul_id` (the station visit,
biota `stod_id`; in the gillnet survey one per set of nets), `year`,
`month`, `station`, `trip`, `sampling_type`, `gridcell`, `begin_lat`,
`begin_lon`, `end_lat`, `end_lon`, `mfdb_gear_code`, `gear_id`,
`tow_depth`, `tow_number`, `tow_length`, `tow_start`, `tow_end`,
`fixed`, and `vessel_id` (vessel number, `skip_nr`)

### pax_mar_strata_stations

A dplyr query of the fixed survey station to stratum lists, with columns
`sampling_type`, `stratification`, `station` and `stratum`. The
groundfish surveys come from `biota.strata_stations`, the gillnet survey
(`smn_strata`) from `ops$bthe."strata_stations"`
