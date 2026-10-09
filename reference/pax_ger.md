# German (Walther Herwig) Greenland groundfish survey

Import the Thünen-Institut exports of the German groundfish survey off
Greenland (R/V Walther Herwig, ICES survey code `GER(GRL)-GFS-Q4`) into
pax-shaped tables, and the steps of the old East Greenland index
(`05-redfish/R/Get_Data/Greenland_Cochran.R`) as explicit functions.

## Usage

``` r
pax_ger_survey(path, species = NULL, species_code = NULL, length_add = 0.5)

pax_ger_station_fix(
  tbl,
  tow_length_default = 2.5,
  tow_length_max = 5,
  lon_end_fix = c(`199100721` = 8, `199100770` = 2)
)

pax_ger_ldist_impute(ldist, station, catch, sample_ids = NULL)

pax_ger_strata(area = 24331.73, lon_range = c(-44, -26), lat_range = c(61, 66))
```

## Arguments

- path:

  Character vector of directories and/or files. Every `.csv`, `.xlsx`
  and `.xls` file in a directory (not recursively) is read and kept if
  it is one of the four Thünen tables

- species:

  Character vector of Thünen species names (FishFi `FARTNAME`, e.g.
  `"SEBASTES MARINUS"`) or species codes (`ARTCODE`) to keep, `NULL`
  (default) for all rows of the files (the Thünen exports are usually
  one species per file)

- species_code:

  Value of the `species` column of `ger_catch` and `ger_ldist`, e.g. the
  MFRI species code. `NULL` (default) uses the Thünen `ARTCODE`

- length_add:

  Added to `LAENGE` to give `length`. `LAENGE` is the cm-below class +
  0.5 (e.g. 18.5 for 18-19 cm); the default 0.5 gives the upper bound of
  the class in whole cm (19), as `Greenland_Cochran.R`

- tbl:

  A dplyr query or data.frame of `ger_station` rows

- tow_length_default:

  Distance (nm) given to tows with a distance of 0, missing, or over
  `tow_length_max`

- tow_length_max:

  Longest plausible tow (nm)

- lon_end_fix:

  Named vector of corrections added to `end_lon`, by `sample_id`

- ldist:

  A dplyr query or data.frame of `ger_ldist` rows

- station:

  A dplyr query or data.frame of `ger_station` rows, with `sample_id`,
  `year` and `ger_stratum`

- catch:

  A dplyr query or data.frame of `ger_catch` rows

- sample_ids:

  Stations to impute, `NULL` (default) for all stations with a catch
  (`catch_count > 0`) and no length measurements. Or a data.frame with
  columns `sample_id`, `year` and `ger_stratum` giving the stations and
  the year and stratum to take the lengths from

- area:

  Area of the East Greenland stratum (km²). The default 24331.73 km²
  (7093.997 nm²) is the constant of `Greenland_Cochran.R`; its source is
  undocumented

- lon_range, lat_range:

  The box of the stratum polygon (decimal degrees)

## Value

### pax_ger_survey

A list of three data.frames, decorated with
[`pax_decorate()`](https://hafro.github.io/pax/reference/pax_decorate.md)
for
[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md):

- `ger_station`:

  One row per StatFi station (or NetzFi haul when there is no StatFi
  file): `sample_id` (`JAHR * 100000 + STATION`, e.g. 201001064 for
  station 1064 of 2010), `haul_id` (`NETZID`), `year`, `month`,
  `station` (`STATION`), `trip` (`REISENR`), `sampling_type` (101, see
  below), `gridcell` (`NA`), `begin_lat`, `begin_lon`, `end_lat`,
  `end_lon` (decimal degrees, as in the file: West negative unless the
  file says `E`), `lat`, `lon` (mid-tow, from the positions as in the
  file), `mfdb_gear_code` (`"BMT"` for `NETZTYP` `OTB`), `gear_id`
  (`NA`), `tow_depth` (mean of the minimum and maximum depth, fishing
  depth `FTMIN`/`FTMAX` before bottom depth `TIEFEMIN`/`TIEFEMAX`, as
  the old loader), `tow_length` (`DISTANZ`, nautical miles, as in the
  file), `tow_start`, `tow_end` (hhmm), `ger_area` (`AREA`, 21 West, 27
  East Greenland) and `ger_stratum` (`STRATUMNR`)

- `ger_catch`:

  `sample_id`, `species`, `catch_count` (`GESAMTSTCK`) and
  `catch_weight` (`GESAMTKG`, kg), summed over nets (`NETZ`)

- `ger_ldist`:

  `sample_id`, `species`, `length`, `sex`, `count` (`LANZAHL` raised to
  the station's total catch, `LANZAHL * catch_count / sum(LANZAHL)` by
  station and species; `NA` when the station has no total catch) and
  `count_measured` (`LANZAHL`)

Stations without catch are kept in `ger_station` and have no rows in
`ger_ldist` (zero stations in
[`pax_si_by_length()`](https://hafro.github.io/pax/reference/pax_si.md)).
Stations with a catch but no length measurements have no `ger_ldist`
rows either, see `pax_ger_ldist_impute()`. `sampling_type` is 101, a
code not used by the MFRI surveys (`synaflokkur`), after division 101
(`r101`) that the East Greenland survey was given in the redfish
assessment. No position or distance is corrected, see
`pax_ger_station_fix()`

### pax_ger_station_fix

`tbl` with the position and distance corrections of the old loader
(`05-redfish/Data_Raw/R/SurveyData-GreenGermany.R`), and `lat`, `lon`
recomputed as the mid-tow position:

- all longitudes West: the old loader dropped the hemisphere letter, and
  the 2025 export marks the East Greenland positions `E`

- latitudes under 50 times 10 (positions with a digit missing, two West
  Greenland tows in 1988)

- `end_lon` + 8 for 199100721 and + 2 for 199100770 (1991 stations 721
  and 770)

- `tow_length` over 5 nm, 0 or missing set to 2.5 nm (not used by the
  old index, which takes a fixed swept area)

These are errors in the Thünen data that should be fixed at the source;
they are corrected here, not in `pax_ger_survey()`, so the imported
tables stay as delivered. `geom` and `h3_cells` of an imported
`ger_station` come from the positions as delivered

### pax_ger_ldist_impute

`ldist` with rows added for stations with a catch but no length
measurements (the rule of `Greenland_Cochran.R`): the length
distribution is the mean of `count_measured` by length over the `ldist`
rows (by sex) of the same species, year and German stratum
(`ger_stratum`), raised to the station's `catch_count`. The added rows
have `sex` `NA`. Stations without a stratum, or whose stratum has no
lengths that year, get no rows. `Greenland_Cochran.R` imputed 1986
stations 725, 750 and 751 and 2010 station 1064 only (`sample_id`
198600725, 198600750, 198600751 and 201001064), with the lengths of 1986
stratum 6.2, 1986 stratum 7.2 (twice) and 2011 stratum 6.2. The old
script's id of station 1064 of 2010 was `JAHR * 1000 + STATION` =
2011064, so its lengths came from the year after. 1985 station 526 (East
Greenland, one fish) was left without lengths and counted as a zero
station

### pax_ger_strata

An sf data.frame with one stratum, `stratum` 1, `name`
`"East Greenland"` and `rall_area` = `area`, decorated with `pax_name`
`"ger_strata"` for
[`pax_import()`](https://hafro.github.io/pax/reference/pax_import.md)
and use as `strata_tbl` of
[`pax_si_scale_by_strata()`](https://hafro.github.io/pax/reference/pax_si.md).
The polygon is the box of the station selection of the old index (west
of 44°W to the east limit of the stations, 61-66°N); its area is not
`rall_area`. As in the old index, stations are assigned to the stratum
by selection (`strata_stations`), not by position, see the example

## Details

The Thünen export tables are:

- StatFi:

  station metadata (`STATID`, `JAHR`, `STATION`, `AREA` 21 West / 27
  East Greenland, `STRATUMNR`)

- NetzFi:

  hauls (`NETZID`, positions `GBFANGB`/`GLFANGB` (start) and
  `GBHIEV`/`GLHIEV` (end) as `DDMMmm` + hemisphere, `DISTANZ` in
  nautical miles, depths)

- FishFi:

  total catch per station and species (`FISHID`, `GESAMTKG`,
  `GESAMTSTCK`), e.g. `CatchesNorvegicus.csv`

- LengFi:

  length frequencies (`LENGID`, `LAENGE`, the cm-below class + 0.5,
  `LANZAHL` the number measured, `SEX`), e.g. `LengthFreqNorvegicus.csv`

Files are recognised by their id column (`STATID`, `NETZID`, `FISHID`,
`LENGID`), so both the comma-separated CSV layout and the newer xlsx
layout (`StatFi.xlsx`, `NetzFi_*.xlsx`, `FishFi*.xlsx`, `LengFi_*.xlsx`)
can be read. Files of the same table are row-bound. Column names are
matched ignoring case; the CSV NetzFi columns
`depthmin1`/`depthmax1`/`depthmin2`/`depthmax2`/`timebeginning`/
`timeend` are read as the xlsx `TIEFEMIN`/`TIEFEMAX`/`FTMIN`/
`FTMAX`/`SZANF`/`SZENDE`. `-9` is missing.

The tables are kept apart from the MFRI `station` and `ldist` tables
(`ger_station`, `ger_ldist`, `ger_catch`), as the stations follow other
conventions (a German stratum, positions that need fixing, a fixed swept
area in the index) and the pax_si\_\* functions take the station and
ldist tables as arguments. They can be combined with the MFRI tables
with
[`dplyr::union_all()`](https://dplyr.tidyverse.org/reference/setops.html)
after selecting common columns.

## Examples

``` r
if (FALSE) { # \dontrun{
pcon <- pax_connect()
ger <- pax_ger_survey(
  "Data_Raw/Greenland/Germany/SurveyData",
  species_code = 5
)
for (t in ger) pax_import(pcon, t)
pax_import(pcon, pax_ger_strata())
} # }
if (FALSE) { # \dontrun{
# The East Greenland golden redfish index of Greenland_Cochran.R
# (data/survey_greenland_by_sample.csv and survey_greenland_by_length.csv
# of 05-reg, before their fill-ins of years without a survey)
ger_station <- dplyr::tbl(pcon, "ger_station") |>
  pax_ger_station_fix() |>
  dplyr::filter(
    lon > -44, lat > 61, lat < 66, year > 1983,
    is.na(ger_stratum) | ger_stratum != 3.2
  ) |>
  # Fixed swept area of 2.2 nm x 41 m (in nm^2), not the tow distance.
  # pax_ldist_scale_tow_area() uses a gear width of 1 for sampling types
  # without tow dimensions
  dplyr::mutate(tow_length = 2.2 * 41 / 1852)
ger_ldist <- dplyr::tbl(pcon, "ger_ldist") |>
  pax_ger_ldist_impute(
    station = dplyr::tbl(pcon, "ger_station"),
    catch = dplyr::tbl(pcon, "ger_catch"),
    # The stations and lengths imputed by Greenland_Cochran.R
    sample_ids = data.frame(
      sample_id = c(198600725, 198600750, 198600751, 201001064),
      year = c(1986, 1986, 1986, 2011),
      ger_stratum = c(6.2, 7.2, 7.2, 6.2)
    )
  ) |>
  dplyr::filter(length < 72) |>
  pax_ldist_add_weight(data.frame(species = 5, a = 0.0109, b = 3.07))
by_sample <- ger_station |>
  pax_si_by_length(ldist = ger_ldist) |>
  pax_si_scale_by_strata(
    "ger_strata",
    strata_stations = dplyr::tbl(pcon, "ger_station") |>
      dplyr::distinct(station, sampling_type) |>
      dplyr::mutate(stratum = 1L)
  )
} # }
```
