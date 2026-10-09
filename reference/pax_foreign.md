# Import foreign (non-MFRI) data

Functions to read data of other countries (Greenland, the Faroe Islands)
into a pax database, in the same way as the MFRI data: from the tables
in mar where they are kept today (the `ops$will` schema), or from the
files delivered by the other institutes, read from a path given when the
database is built. The data are never part of the package. Like the
[pax_mar](https://hafro.github.io/pax/reference/pax_mar.md) functions,
they return tables decorated for
[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md);
see the `foreign_tables` and `foreign_files` arguments of
[`pax_from_mar()`](https://hafro.github.io/pax/reference/pax_from_mar.md).

## Usage

``` r
pax_foreign_mar_tables()

pax_mar_foreign(mar, table, schema = "ops$will")

pax_foreign_file_types()

pax_foreign_file(path, type)
```

## Arguments

- mar:

  A MAR database connection, as returned by `mar::connect_mar()`

- table:

  A `name` of `pax_foreign_mar_tables()`

- schema:

  The schema in mar that holds the table

- path:

  Path(s) of the file(s)

- type:

  The type of file, one of `pax_foreign_file_types()`:

  `"greenland_logbooks"`

  :   the logbooks of the Greenland Institute of Natural Resources, all
      species (CSV with columns
      `code, year, gear, time1, time2, country, area, area_detail, catch_t, trawltime_h, lon, lat, eez_grl, eez_ice`);
      pax table `greenland_logbooks`, the columns as in the file,
      `time1` and `time2` date-times (UTC)

  `"greenland_catch"`

  :   Greenland catch by year, month and area (CSV with columns
      `year, month, area, catch_tot`, e.g. the cod
      `data_GrL_catch.csv`); pax table `greenland_catch`

  `"faroese_logbooks"`

  :   Faroese logbook records (the Vorn `.xlsx` extracts with
      `STARTLATT`, `STARTLONG` in ddmm, `DAYDATE`, `LOGBOOKTYPE`,
      `WEIGHTKG`, and the newer `.csv` extracts with
      `GEARSHOT_LATITUDE`, `GEARSHOT_LONGITUDE`, `GEARSHOT_DATETIME`,
      `LOGBOOKINFO_TYPE`, `WEIGHT_KG`); pax table `faroese_logbooks`
      with columns `file`, `date` (date-time, UTC), `year`, `month`,
      `lat`, `lon` (decimal degrees, west negative), `logbook_type`
      (`"LongLine"`, `"Trawl"`, ...) and `catch` (kg). Needs the readxl
      package for the `.xlsx` files

## Value

### pax_foreign_mar_tables

A data.frame of the foreign data tables in mar that `pax_mar_foreign()`
reads: `name` (the pax table name), `mar_table` and `description`

### pax_mar_foreign

The whole table, collected, with the column names of mar (dots replaced
by underscores), decorated with the pax table name

### pax_foreign_file_types

The types of foreign files `pax_foreign_file()` reads

### pax_foreign_file

A data.frame decorated with the pax table name
