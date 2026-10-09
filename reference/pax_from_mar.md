# Create a pax database populated from the MAR database

Opens a connection to the Hafro MAR Oracle database and imports all
standard pax tables (station, measurement, logbook, landings, sampling,
aldist, ldist, lw_coeffs, ocean depth, strata, and strata_stations) into
a new pax DuckDB database.

## Usage

``` r
pax_from_mar(
  species,
  year_start = NULL,
  year_end = NULL,
  sampling_type = c(1, 2, 8, 10, 11, 30, 35),
  ices_area_like = "5a%",
  landings_year_start = year_start,
  strata = pax_def_strata_list(),
  quota_species = species,
  sampling_gear = c("BMT", "LLN", "DSE"),
  skip_trips = c("MAG%", "MO%"),
  gridcell_from_position = FALSE,
  logbook_species = species,
  logbook_year_start = year_start,
  extra_tables = character(0),
  medafli_species = NULL,
  foreign_tables = character(0),
  foreign_files = list(),
  ger_survey_path = NULL,
  ger_species = NULL,
  ger_species_code = NULL,
  mar_opts = list(),
  dbdir = ":memory:"
)

pax_from_mar_extra_tables()
```

## Arguments

- species:

  Integer vector of species codes to import

- year_start:

  Optional integer, earliest year to include

- year_end:

  Optional integer, latest year to include

- sampling_type:

  Integer vector of sampling type codes to include

- ices_area_like:

  Character vector of SQL LIKE patterns for filtering ICES areas, e.g.
  `"5a%"`

- landings_year_start:

  Optional integer, earliest year of landings to include (default
  `year_start`). The landings go back to 1903, e.g. for figures of the
  landings history.

- strata:

  Character vector of strata names to import, from
  [`pax_def_strata_list()`](https://hafro.github.io/pax/reference/pax_strata.md)

- quota_species:

  Quota species codes (`fteg`) of the quotatransfer table, see
  [`pax_mar_quotatransfer()`](https://hafro.github.io/pax/reference/pax_mar.md).
  Default `species`

- sampling_gear:

  Gear codes (`mfdb_gear_code`) of the commercial samples in the
  `sampling` table, `NULL` for all, see
  [`pax_mar_sampling()`](https://hafro.github.io/pax/reference/pax_mar.md).
  Default bottom trawl, longline and Danish seine

- skip_trips:

  SQL LIKE patterns of trips to leave out of the station and sampling
  tables, see
  [`pax_mar_station()`](https://hafro.github.io/pax/reference/pax_mar.md).
  By default the stomach-sampling trips `MAG*` and `MO*`; `NULL` keeps
  all

- gridcell_from_position:

  If `TRUE`, stations without a rectangle get the gridcell of their
  position, see
  [`pax_mar_station()`](https://hafro.github.io/pax/reference/pax_mar.md)

- logbook_species:

  Species codes of the `logbook` table (and of the optional
  `logbook_old`), default `species`. Add other species for e.g. CPUE of
  the tows of another species, or the share of the stock's catch in them

- logbook_year_start:

  Optional integer, earliest year of the `logbook` table (and
  `logbook_old`), default `year_start`. The compiled logbooks go back to
  1969 for some species

- extra_tables:

  Character vector of optional tables to add, none by default (see
  [pax_mar_extra](https://hafro.github.io/pax/reference/pax_mar_extra.md)):

  `"landings_vessel"`

  :   landings register by vessel, month, landings gear code, fishing
      area and fishing year, from `landings_year_start`,
      [`pax_mar_landings_vessel()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)

  `"vessel"`

  :   the vessel register,
      [`pax_mar_vessel()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)

  `"logbook_release"`

  :   logbook catch records with their condition (released fish),
      [`pax_mar_logbook_release()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)

  `"research_landings"`

  :   landings of the research vessels during research trips,
      [`pax_mar_research_landings()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)

  `"catch_disposition"`

  :   landed catch by disposition,
      [`pax_mar_catch_disposition()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)

  `"landings_old"`

  :   the old cod landings,
      [`pax_mar_landings_old()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)

  `"station_skipped"`

  :   the stations of the trips left out by `skip_trips` (the
      stomach-sampling trips), with the columns of `station`,
      [`pax_mar_station()`](https://hafro.github.io/pax/reference/pax_mar.md)

  `"sample"`

  :   all samples with measurements of the species, all years and trips,
      [`pax_mar_sample()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)

  `"logbook_old"`

  :   the old logbook tables (`afli.afli`) of `logbook_species`,
      [`pax_mar_logbook_old()`](https://hafro.github.io/pax/reference/pax_mar_extra.md)

- medafli_species:

  Species codes of the old by-catch table (`afli.medafli`) for the
  `logbook_release` table, e.g. `2021` (released halibut)

- foreign_tables:

  Names of the foreign data tables in mar to import (Greenland and
  Faroese surveys, catches and samples), see
  [`pax_foreign_mar_tables()`](https://hafro.github.io/pax/reference/pax_foreign.md).
  None by default

- foreign_files:

  Named list of foreign data files to import, read from the given paths
  when the database is built: names are the file types of
  [`pax_foreign_file()`](https://hafro.github.io/pax/reference/pax_foreign.md)
  (e.g. `list(greenland_logbooks = "path/to/logbooks_00-25.csv")`),
  values the paths. None by default

- ger_survey_path:

  Directories or files of the Thünen-Institut exports of the German
  (Walther Herwig) Greenland survey, read when the database is built,
  see
  [`pax_ger_survey()`](https://hafro.github.io/pax/reference/pax_ger.md).
  `NULL` (default) for none. Adds the tables `ger_station`, `ger_catch`,
  `ger_ldist` and `ger_strata`
  ([`pax_ger_strata()`](https://hafro.github.io/pax/reference/pax_ger.md))

- ger_species, ger_species_code:

  The `species` and `species_code` of
  [`pax_ger_survey()`](https://hafro.github.io/pax/reference/pax_ger.md)

- mar_opts:

  Named list of additional options passed to `mar::connect_mar()`

- dbdir:

  Path to a DuckDB database file, or `":memory:"` for an in-memory
  database

## Value

A pax DBI connection containing all imported tables

### pax_from_mar_extra_tables

The names of the optional tables of `pax_from_mar()` (argument
`extra_tables`)
