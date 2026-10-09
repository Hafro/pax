# Import optional tables from the MAR database

Functions for the optional tables of
[`pax_from_mar()`](https://hafro.github.io/pax/reference/pax_from_mar.md)
(argument `extra_tables`): data that only some stocks need, for tech
report figures, advice tables or model input, so that the assessment
repositories need no direct access to mar. Like the
[pax_mar](https://hafro.github.io/pax/reference/pax_mar.md) functions,
they return tables decorated for
[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md).

## Usage

``` r
pax_mar_landings_vessel(mar, species, year_start = NULL, year_end = NULL)

pax_mar_vessel(mar)

pax_mar_logbook_release(mar, species, medafli_species = NULL)

pax_mar_research_landings(
  mar,
  species,
  sampling_type = c(10, 11, 30, 35, 40, 34, 20, 21, 19, 31, 37)
)

pax_mar_catch_disposition(mar, species)

pax_mar_landings_old(mar)

pax_mar_sample(mar, species)

pax_mar_logbook_old(mar, species, year_start = NULL, year_end = NULL)
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

- medafli_species:

  Species codes of the old by-catch table `afli.medafli` to include
  (e.g. `2021`, released halibut), `NULL` for none

- sampling_type:

  Sampling types (`synaflokkur_nr`) of the research trips

## Value

### pax_mar_landings_vessel

Landings register by vessel and month (Directorate of Fisheries,
`kvoti.lods_oslaegt`, and the Fisheries Association's landings before
1994, `fiskifelagid.landed_catch_pre94`, via `mar::lods_oslaegt()` and
`mar::fiskifelag_oslaegt()`), summed over landings. Columns `source`
(`"lods"` or `"fiskifelag"`; the two overlap in 1992 and 1993), `year`,
`month`, `species` (`fteg`), `vessel_id` (`skip_nr`), `gear_id`
(landings gear code, `veidarfaeri`), `fishing_area` (`veidisvaedi`, e.g.
`"I"` for Icelandic waters), `period` (quota period, mar's `timabil`,
e.g. `"20242025"`; the year for `fiskifelag`), `fishing_year` (label as
the old advice code, e.g. `"2024/2025"`, the calendar year before
September 1991) and `catch` (ungutted, kg)

### pax_mar_vessel

The vessel register (`vessel.vessel_v` with `usage_category` and
`construction_year` from `vessel.vessel`), one row per vessel:
`vessel_id` (registration number, the `vessel_id`/`boat_id` of the other
pax tables), `name`, `status` (e.g. `"Erlent"` for foreign vessels),
`usage_category_no`, `usage_category`, `operational_category_no`,
`region_no`, `home_port_no`, `power_kw`, `length`, `brutto_grt`,
`brutto_weight_tons` and `construction_year`. Owner names, addresses and
identity numbers are left out

### pax_mar_logbook_release

Logbook catch records of the species with their condition (released
fish, `condition == "RELE"`): one row per electronic logbook catch
record (`source = "adb"`, `ADB.CATCH_V` with `ADB.STATION_V` and
`ADB.TRIP_V`, all conditions) and per record of the old by-catch table
(`source = "medafli"`, `afli.medafli` with `afli.stofn`, species
`medafli_species`). Columns `source`, `catch_id`, `station_id`,
`vessel_id`, `year` (adb: year registered; medafli: year of the fishing
day), `gear_id`, `species`, `condition`, `catch` (kg) and `count`
(medafli)

### pax_mar_research_landings

Landings (gutted weight, `kvoti.lods_slaegt`) of the research vessels
during research trips: the vessel's landings in the landings register
between the trip's departure and return (`brottfor`, `koma`), as the old
cod `research_landings()`. A landing is counted once per sampling type
of the trip. Columns `vessel_id`, `period` (quota period of the landing
date, e.g. `"20242025"`), `sampling_type`, `species` (`fteg`) and
`catch` (gutted, kg)

### pax_mar_catch_disposition

Landed catch by disposition (`agf.aflagrunnur` with `ask.afdrif`, e.g.
catch landed outside the vessel's quota, "VS-afli"), summed by `species`
(`fisktegund`), `year` and `month` of the landing, `period` (quota
period of the landing date, e.g. `"20242025"`), `disposition` (`afdrif`)
and `disposition_name` (`heiti`). Columns `catch` (gutted,
`magn_slaegt`, kg) and `catch_ungutted` (`magn_oslaegt`, kg). Buyers and
sellers are left out

### pax_mar_landings_old

The cod landings "written in stone" (`ops$pamela."lnd_old"`, tonnes), by
`year`, `month`, `gid` (gear group), `country` (`ccode`), `fishing_year`
(`yearf`), `native` and `warning`, as used for the cod catch at age
until 2022. The table holds cod only and has no species column

### pax_mar_sample

One row per biota sample (all years and trips, the stomach-sampling
trips included) with length or age measurements of the species:
`sample_id`, `haul_id`, `year`, `month`, `sampling_type`, `trip`,
`gear_id`, `reitur` (rectangle), `smareitur` (subrectangle), `gridcell`
(`10 * reitur + smareitur`) and `vessel_id`. Use it to join the `aldist`
and `ldist` tables, which hold all samples of the species, to their year
and sampling type where the `station` table does not reach (before
`year_start`, or the trips left out by `skip_trips`)

### pax_mar_logbook_old

The old logbook tables (`afli.afli` with `afli.stofn`, 1950-2024), one
row per tow and species: `logbook_id` (`visir`), `species` (`tegund`),
`year` and `month` (of the fishing day `vedags`), `veman` (the month
column of `afli.stofn`), `vessel_id` (`skipnr`), `gear_id` (`veidarf`),
`depth_fathoms` (`dypi`) and `catch` (`afli`, kg). Used e.g. for the
monthly catch shares of the years before the compiled logbooks separate
the redfish species
