# Building and opening a pax database

Every pax calculation runs on a local DuckDB database, the `pax_db` of a
stock repo. It is built once from the MFRI Oracle database (`mar`) with
[`pax_from_mar()`](https://hafro.github.io/pax/reference/pax_from_mar.md),
kept as a targets object, and everything downstream reads only it. This
vignette shows how it is built in the stock repos, what goes in it, and
how to open, extend and inspect one. The database parts need `mar` and
are not run here; the rest runs on a toy database.

## What `pax_connect()` needs

[`pax_connect()`](https://hafro.github.io/pax/reference/pax_connect.md)
opens (or creates) a DuckDB file, or an in-memory database by default,
and loads two DuckDB extensions: `spatial` and the community extension
`h3`. The first time on a machine DuckDB downloads them from the
internet (to `~/.duckdb`); after that it works offline.

``` r

library(pax)
library(dplyr, warn.conflicts = FALSE)

path <- tempfile(fileext = ".duckdb")
pcon <- pax_connect(path)
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
```

## Building from mar

[`pax_from_mar()`](https://hafro.github.io/pax/reference/pax_from_mar.md)
connects to mar (VPN when working remotely) and imports the standard
tables for the given species and years: `station`, `measurement`,
`ldist`, `aldist`, `lw_coeffs`, `sampling`, `landings`, `logbook`,
`quotatransfer`, the fixed survey station lists `strata_stations`, the
bathymetry `ocean_depth` and the strata polygons (`old_strata`,
`new_strata_spring`, …). The saithe call (03-sai):

``` r

pax_db <- pax_from_mar(
  species = 3,
  year_start = 1980,
  year_end = 2026,
  # Commercial (1, 2, 4, 8), survey otoliths (10, 11), spring (30) and
  # autumn (35) surveys
  sampling_type = c(1, 2, 4, 8, 10, 11, 30, 35),
  # Landings since 1903, for the landings history of the tech report
  landings_year_start = 1903,
  dbdir = "pax.duckdb"
)
```

Other arguments the stock repos use:

- `ices_area_like = c("5a%", "14%")`: landings of more areas (07-bli,
  08-usk);
- `skip_trips`: by default the stomach-sampling trips (`MAG%`, `MO%`)
  are left out of `station`, which tidypax kept; cod lost up to 26% of
  its commercial samples, see `extra_tables = "station_skipped"` below;
- `gridcell_from_position = TRUE`: stations without a rectangle get the
  one of their position (otherwise they get no region in the age–length
  keys);
- `logbook_species`, `logbook_year_start`: logbooks of other species or
  years than the stock’s.

### Extra tables

Tables the stock repos used to read from mar directly are optional:

``` r

pax_from_mar_extra_tables()
#> [1] "landings_vessel"   "vessel"            "logbook_release"  
#> [4] "research_landings" "catch_disposition" "landings_old"     
#> [7] "station_skipped"   "sample"            "logbook_old"
```

01-cod takes most of them (TAC left, catch outside the quotas, old
landings, stomach-trip samples, all samples for length–weight); 07-bli
takes `sample`. See
[`?pax_from_mar`](https://hafro.github.io/pax/reference/pax_from_mar.md)
for each.

### Foreign data

Data of other countries are never part of the package. They are read
when the database is built, either from tables in mar
(`foreign_tables`):

``` r

pax_foreign_mar_tables()$name
#>  [1] "ghl_catch"                  "ghl_comm_stations"         
#>  [3] "ghl_comm_samples"           "ghl_greenland_stations"    
#>  [5] "ghl_greenland_lengths"      "ghl_faroes_survey_stations"
#>  [7] "ghl_faroes_survey_length"   "ghl_faroes_survey_age"     
#>  [9] "ghl_strata_mapping"         "bli_gss_greenland_stations"
#> [11] "bli_gss_greenland_lengths"  "bli_gss_strata_mapping_smh"
#> [13] "bli_gss_strata_mapping_smb" "mdas_catch"                
#> [15] "reb_landings_1950_2010"
```

or from files delivered by the other institutes (`foreign_files`, a
named list of file type to path):

``` r

pax_foreign_file_types()
#> [1] "greenland_logbooks" "greenland_catch"    "faroese_logbooks"
```

The German (Walther Herwig) survey off Greenland is read from the
Thünen-Institut exports with `ger_survey_path` (a directory or files),
`ger_species` and `ger_species_code`, into the tables `ger_station`,
`ger_catch`, `ger_ldist` and `ger_strata` (see
[`?pax_ger_survey`](https://hafro.github.io/pax/reference/pax_ger.md)).
05-reg and 08-usk:

``` r

pax_from_mar(
  species, year_start, year_end,
  sampling_type = c(1, 2, 4, 8, 30, 34, 35),
  ices_area_like = c("5a%", "14%"),
  ger_survey_path = ger_survey_files,            # config.R, from an env var
  ger_species = "BROSME BROSME",
  ger_species_code = species,
  foreign_files = list(greenland_logbooks = path.expand(greenland_logbook_file))
)
```

All file paths are checked before the (slow) import from mar starts.
Keep the paths in `config.R`, set from environment variables, and never
commit the files. A `pax_db` built with foreign data holds those data:
MFRI may work with them but not share them, so a copy of the database
must not leave MFRI (see the 01-cod README).

## In a targets pipeline

The stock repos build `pax_db` as the first target of
`script_assessment_model.R`:

``` r

tar_target(
  pax_db,
  if (nzchar(Sys.getenv("PAX_SOURCE_DB"))) {
    pax_connect(Sys.getenv("PAX_SOURCE_DB"))
  } else {
    pax_from_mar(species, year_start, year_end, ...)
  },
  format = pax_tar_format_duckdb()
)
```

- [`pax_tar_format_duckdb()`](https://hafro.github.io/pax/reference/pax_tar_format.md)
  writes the database into the targets store (copying an in-memory
  database to the file) and opens it **read-only** in every target that
  uses it; reading it doesn’t change its hash.
- `PAX_SOURCE_DB` points at an existing `pax_db` file (e.g. a copy of
  `_assessment_model/objects/pax_db`). With it set, the pipeline runs
  without mar or the VPN; the repos check in `run.R` that no other code
  reads mar (`check_no_mar()`, e.g. 01-cod, 03-sai).
- Derived tables go in Parquet with
  [`pax_tar_format_parquet()`](https://hafro.github.io/pax/reference/pax_tar_format.md),
  which drops the `geom` and `h3_cells` columns Parquet can’t store.
- targets doesn’t track package versions: after installing a new pax,
  run `targets::tar_invalidate(pax_db)` to rebuild it. Also check after
  `renv::restore()` that you got the pinned pax branch, e.g.
  `exists("pax_si_length_index", asNamespace("pax"))`.

The vignette “Incorporating external data” shows a live targets run with
extra imports.

## Importing and inspecting

[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md)
writes a data.frame, an sf object, a lazy query or a CSV file into the
database, and records where it came from. A table with `lat`/`lon` (or
`begin_lat`, `begin_lon`, `end_lat`, `end_lon`) gets a `geom` column and
its h3 cells, polygons get the cells they cover; that is what the strata
and depth joins use.

``` r

tows <- data.frame(
  sample_id = 1:3,
  year = 2025,
  lat = c(64.1, 65.3, 66.6),
  lon = c(-23.5, -25.1, -18.2)
)
pax_import(pcon, tows, cite = "Toy tows, made up for this vignette")
pax_import(
  pcon,
  pax_decorate(
    data.frame(species = 1, a = 0.01, b = 3),
    cite = "Toy length-weight coefficients",
    name = "lw_coeffs"
  )
)
colnames(tbl(pcon, "tows"))
#> [1] "sample_id" "year"      "lat"       "lon"       "geom"      "h3_cells"
pax_contents(pcon) |> collect()
#> # A tibble: 2 × 2
#>   tbl_name  citation                           
#>   <chr>     <chr>                              
#> 1 tows      Toy tows, made up for this vignette
#> 2 lw_coeffs Toy length-weight coefficients
```

The table name defaults to the variable name, or the `name` given to
[`pax_decorate()`](https://hafro.github.io/pax/reference/pax_decorate.md),
and the citation to the one given to
[`pax_decorate()`](https://hafro.github.io/pax/reference/pax_decorate.md)
(the `pax_mar_*` functions set both). Importing over an existing table
needs `overwrite = TRUE`.

The database is a plain DuckDB file, so it can be opened again later,
read-only as targets does:

``` r

DBI::dbDisconnect(pcon)
pcon <- pax_connect(path, read_only = TRUE)
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
DBI::dbListTables(pcon)
#> [1] "h3_resolution" "lw_coeffs"     "pax_citation"  "tows"
tbl(pcon, "tows") |> select(sample_id, lat, lon) |> collect()
#> # A tibble: 3 × 3
#>   sample_id   lat   lon
#>       <int> <dbl> <dbl>
#> 1         1  64.1 -23.5
#> 2         2  65.3 -25.1
#> 3         3  66.6 -18.2
DBI::dbDisconnect(pcon)
```

## AI use

This vignette was drafted with Claude (Anthropic) in October 2026 and
has not yet been checked by a person. (MFRI policy on AI use.)
