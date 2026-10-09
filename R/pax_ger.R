#' German (Walther Herwig) Greenland groundfish survey
#'
#' Import the Thünen-Institut exports of the German groundfish survey off
#' Greenland (R/V Walther Herwig, ICES survey code ``GER(GRL)-GFS-Q4``) into
#' pax-shaped tables, and the steps of the old East Greenland index
#' (``05-redfish/R/Get_Data/Greenland_Cochran.R``) as explicit functions.
#'
#' The Thünen export tables are:
#' \describe{
#'   \item{StatFi}{station metadata (``STATID``, ``JAHR``, ``STATION``,
#'     ``AREA`` 21 West / 27 East Greenland, ``STRATUMNR``)}
#'   \item{NetzFi}{hauls (``NETZID``, positions ``GBFANGB``/``GLFANGB``
#'     (start) and ``GBHIEV``/``GLHIEV`` (end) as ``DDMMmm`` + hemisphere,
#'     ``DISTANZ`` in nautical miles, depths)}
#'   \item{FishFi}{total catch per station and species (``FISHID``,
#'     ``GESAMTKG``, ``GESAMTSTCK``), e.g. ``CatchesNorvegicus.csv``}
#'   \item{LengFi}{length frequencies (``LENGID``, ``LAENGE``, the cm-below
#'     class + 0.5, ``LANZAHL`` the number measured, ``SEX``), e.g.
#'     ``LengthFreqNorvegicus.csv``}
#' }
#' Files are recognised by their id column (``STATID``, ``NETZID``,
#' ``FISHID``, ``LENGID``), so both the comma-separated CSV layout and the
#' newer xlsx layout (``StatFi.xlsx``, ``NetzFi_*.xlsx``, ``FishFi*.xlsx``,
#' ``LengFi_*.xlsx``) can be read. Files of the same table are row-bound.
#' Column names are matched ignoring case; the CSV NetzFi columns
#' ``depthmin1``/``depthmax1``/``depthmin2``/``depthmax2``/``timebeginning``/
#' ``timeend`` are read as the xlsx ``TIEFEMIN``/``TIEFEMAX``/``FTMIN``/
#' ``FTMAX``/``SZANF``/``SZENDE``. ``-9`` is missing.
#'
#' The tables are kept apart from the MFRI ``station`` and ``ldist`` tables
#' (``ger_station``, ``ger_ldist``, ``ger_catch``), as the stations follow
#' other conventions (a German stratum, positions that need fixing, a fixed
#' swept area in the index) and the pax_si_* functions take the station and
#' ldist tables as arguments. They can be combined with the MFRI tables with
#' ``dplyr::union_all()`` after selecting common columns.
#'
#' @param path Character vector of directories and/or files. Every ``.csv``,
#'   ``.xlsx`` and ``.xls`` file in a directory (not recursively) is read and
#'   kept if it is one of the four Thünen tables
#' @param species Character vector of Thünen species names (FishFi
#'   ``FARTNAME``, e.g. ``"SEBASTES MARINUS"``) or species codes
#'   (``ARTCODE``) to keep, ``NULL`` (default) for all rows of the files
#'   (the Thünen exports are usually one species per file)
#' @param species_code Value of the ``species`` column of ``ger_catch`` and
#'   ``ger_ldist``, e.g. the MFRI species code. ``NULL`` (default) uses the
#'   Thünen ``ARTCODE``
#' @param length_add Added to ``LAENGE`` to give ``length``. ``LAENGE`` is
#'   the cm-below class + 0.5 (e.g. 18.5 for 18-19 cm); the default 0.5
#'   gives the upper bound of the class in whole cm (19), as
#'   ``Greenland_Cochran.R``
#' @return \subsection{pax_ger_survey}{A list of three data.frames, decorated
#'   with [pax_decorate()] for [pax_import()]:
#'   \describe{
#'     \item{``ger_station``}{One row per StatFi station (or NetzFi haul when
#'       there is no StatFi file): ``sample_id`` (``JAHR * 100000 +
#'       STATION``, e.g. 201001064 for station 1064 of 2010),
#'       ``haul_id`` (``NETZID``), ``year``, ``month``, ``station``
#'       (``STATION``), ``trip`` (``REISENR``), ``sampling_type`` (101, see
#'       below), ``gridcell`` (``NA``), ``begin_lat``, ``begin_lon``,
#'       ``end_lat``, ``end_lon`` (decimal degrees, as in the file: West
#'       negative unless the file says ``E``), ``lat``, ``lon`` (mid-tow, from
#'       the positions as in the file), ``mfdb_gear_code`` (``"BMT"`` for
#'       ``NETZTYP`` ``OTB``), ``gear_id`` (``NA``), ``tow_depth`` (mean of
#'       the minimum and maximum depth, fishing depth ``FTMIN``/``FTMAX``
#'       before bottom depth ``TIEFEMIN``/``TIEFEMAX``, as the old loader),
#'       ``tow_length`` (``DISTANZ``, nautical miles, as in the file),
#'       ``tow_start``, ``tow_end`` (hhmm), ``ger_area`` (``AREA``, 21 West,
#'       27 East Greenland) and ``ger_stratum`` (``STRATUMNR``)}
#'     \item{``ger_catch``}{``sample_id``, ``species``, ``catch_count``
#'       (``GESAMTSTCK``) and ``catch_weight`` (``GESAMTKG``, kg), summed
#'       over nets (``NETZ``)}
#'     \item{``ger_ldist``}{``sample_id``, ``species``, ``length``, ``sex``,
#'       ``count`` (``LANZAHL`` raised to the station's total catch,
#'       ``LANZAHL * catch_count / sum(LANZAHL)`` by station and species;
#'       ``NA`` when the station has no total catch) and ``count_measured``
#'       (``LANZAHL``)}
#'   }
#'   Stations without catch are kept in ``ger_station`` and have no rows in
#'   ``ger_ldist`` (zero stations in [pax_si_by_length()]). Stations with a
#'   catch but no length measurements have no ``ger_ldist`` rows either, see
#'   [pax_ger_ldist_impute()]. ``sampling_type`` is 101, a code not used by
#'   the MFRI surveys (``synaflokkur``), after division 101 (``r101``) that
#'   the East Greenland survey was given in the redfish assessment. No
#'   position or distance is corrected, see [pax_ger_station_fix()]}
#' @examples
#' \dontrun{
#' pcon <- pax_connect()
#' ger <- pax_ger_survey(
#'   "Data_Raw/Greenland/Germany/SurveyData",
#'   species_code = 5
#' )
#' for (t in ger) pax_import(pcon, t)
#' pax_import(pcon, pax_ger_strata())
#' }
#' @name pax_ger
NULL

#' @rdname pax_ger
pax_ger_survey <- function(
  path,
  species = NULL,
  species_code = NULL,
  length_add = 0.5
) {
  tbls <- ger_read_tables(path)
  if (is.null(tbls$netz)) {
    stop("No NetzFi file (with a NETZID column) found in: ", toString(path))
  }
  if (is.null(tbls$fish)) {
    stop("No FishFi file (with a FISHID column) found in: ", toString(path))
  }
  if (!is.null(species_code) && length(species_code) != 1) {
    stop("species_code should be NULL or a single value")
  }

  # Stations ------------------------------------------------------------------
  netz <- tbls$netz
  netz <- data.frame(
    sample_id = ger_sample_id(netz),
    haul_id = ger_num(netz$NETZID),
    year = as.integer(ger_num(netz$JAHR)),
    month = as.integer(ger_num(netz$MONAT)),
    station = as.integer(ger_num(netz$STATION)),
    begin_lat = ger_pos(netz$GBFANGB),
    begin_lon = ger_pos(netz$GLFANGB),
    end_lat = ger_pos(netz$GBHIEV),
    end_lon = ger_pos(netz$GLHIEV),
    mfdb_gear_code = ifelse(
      ger_col(netz, "NETZTYP") %in% "OTB",
      "BMT",
      NA_character_
    ),
    depth_min = dplyr::coalesce(
      ger_num(ger_col(netz, "FTMIN")),
      ger_num(ger_col(netz, "TIEFEMIN"))
    ),
    depth_max = dplyr::coalesce(
      ger_num(ger_col(netz, "FTMAX")),
      ger_num(ger_col(netz, "TIEFEMAX"))
    ),
    tow_length = ger_num(netz$DISTANZ),
    tow_start = ger_num(ger_col(netz, "SZANF")),
    tow_end = ger_num(ger_col(netz, "SZENDE")),
    stringsAsFactors = FALSE
  )
  ger_check_unique(netz$sample_id, "NetzFi")

  if (is.null(tbls$stat)) {
    message("No StatFi file, stations from NetzFi without area and stratum")
    station <- netz
    station$trip <- NA_character_
    station$ger_area <- NA_integer_
    station$ger_stratum <- NA_real_
  } else {
    stat <- tbls$stat
    stat <- data.frame(
      sample_id = ger_sample_id(stat),
      trip = as.character(stat$REISENR),
      ger_area = as.integer(ger_num(stat$AREA)),
      ger_stratum = ger_num(ger_col(stat, "STRATUMNR")),
      stringsAsFactors = FALSE
    )
    ger_check_unique(stat$sample_id, "StatFi")
    # NB: The old loader also joined on month and day, they agree in all
    #     the files seen
    station <- merge(stat, netz, by = "sample_id", all.x = TRUE, sort = FALSE)
    missing_netz <- is.na(station$year)
    station$year[missing_netz] <- station$sample_id[missing_netz] %/% ger_id_mult
    station$station[missing_netz] <- station$sample_id[missing_netz] %% ger_id_mult
  }
  station$tow_depth <- ifelse(
    is.na(station$depth_max),
    station$depth_min,
    (station$depth_min + station$depth_max) / 2
  )
  station$lat <- (station$begin_lat + station$end_lat) / 2
  station$lon <- (station$begin_lon + station$end_lon) / 2
  station$sampling_type <- 101L
  station$gridcell <- NA_integer_
  station$gear_id <- NA_integer_
  station <- station[
    order(station$year, station$station),
    c(
      "sample_id",
      "haul_id",
      "year",
      "month",
      "station",
      "trip",
      "sampling_type",
      "gridcell",
      "begin_lat",
      "begin_lon",
      "end_lat",
      "end_lon",
      "lat",
      "lon",
      "mfdb_gear_code",
      "gear_id",
      "tow_depth",
      "tow_length",
      "tow_start",
      "tow_end",
      "ger_area",
      "ger_stratum"
    )
  ]
  rownames(station) <- NULL

  # Catch ---------------------------------------------------------------------
  fish <- tbls$fish
  keep_fish <- if (is.null(species)) {
    rep(TRUE, nrow(fish))
  } else {
    ger_col(fish, "FARTNAME") %in% species | fish$ARTCODE %in% species
  }
  fish <- fish[keep_fish, , drop = FALSE]
  catch <- data.frame(
    sample_id = ger_sample_id(fish),
    species = if (is.null(species_code)) {
      as.character(fish$ARTCODE)
    } else {
      rep(species_code, nrow(fish))
    },
    catch_count = ger_num(fish$GESAMTSTCK),
    catch_weight = ger_num(fish$GESAMTKG),
    stringsAsFactors = FALSE
  )
  catch <- ger_sum_by(catch, c("sample_id", "species"))
  catch <- catch[order(catch$sample_id, catch$species), ]
  rownames(catch) <- NULL

  # Lengths -------------------------------------------------------------------
  if (is.null(tbls$leng)) {
    warning("No LengFi file (with a LENGID column), ger_ldist is empty")
    leng <- data.frame(
      JAHR = numeric(0),
      STATION = numeric(0),
      ARTCODE = character(0),
      SEX = character(0),
      LAENGE = numeric(0),
      LANZAHL = numeric(0)
    )
  } else {
    leng <- tbls$leng
  }
  keep_leng <- if (is.null(species)) {
    rep(TRUE, nrow(leng))
  } else {
    leng$ARTCODE %in% c(species, tbls$fish$ARTCODE[keep_fish])
  }
  leng <- leng[keep_leng, , drop = FALSE]
  ldist <- data.frame(
    sample_id = ger_sample_id(leng),
    species = if (is.null(species_code)) {
      as.character(leng$ARTCODE)
    } else {
      rep(species_code, nrow(leng))
    },
    length = ger_num(leng$LAENGE) + length_add,
    sex = as.character(ger_col(leng, "SEX")),
    count_measured = ger_num(leng$LANZAHL),
    stringsAsFactors = FALSE
  )
  ldist <- ger_sum_by(ldist, c("sample_id", "species", "length", "sex"))

  # Raise to the total catch of the station
  key <- function(df) paste(df$sample_id, df$species)
  n_measured <- tapply(ldist$count_measured, key(ldist), sum, na.rm = TRUE)
  r <- catch$catch_count[match(key(ldist), key(catch))] /
    as.numeric(n_measured[key(ldist)])
  ldist$count <- ldist$count_measured * r
  ldist <- ldist[
    order(ldist$sample_id, ldist$species, ldist$length, ldist$sex),
    c("sample_id", "species", "length", "sex", "count", "count_measured")
  ]
  rownames(ldist) <- NULL

  cite <- paste0(
    "German groundfish survey off Greenland (Thünen-Institut, Walther ",
    "Herwig), read from ",
    toString(path)
  )
  list(
    ger_station = pax_decorate(station, cite = cite, name = "ger_station"),
    ger_catch = pax_decorate(catch, cite = cite, name = "ger_catch"),
    ger_ldist = pax_decorate(ldist, cite = cite, name = "ger_ldist")
  )
}

#' @param tbl A dplyr query or data.frame of ``ger_station`` rows
#' @param tow_length_default Distance (nm) given to tows with a distance of
#'   0, missing, or over ``tow_length_max``
#' @param tow_length_max Longest plausible tow (nm)
#' @param lon_end_fix Named vector of corrections added to ``end_lon``, by
#'   ``sample_id``
#' @return \subsection{pax_ger_station_fix}{``tbl`` with the position and
#'   distance corrections of the old loader
#'   (``05-redfish/Data_Raw/R/SurveyData-GreenGermany.R``), and ``lat``,
#'   ``lon`` recomputed as the mid-tow position:
#'   \itemize{
#'     \item all longitudes West: the old loader dropped the hemisphere
#'       letter, and the 2025 export marks the East Greenland positions
#'       ``E``
#'     \item latitudes under 50 times 10 (positions with a digit missing,
#'       two West Greenland tows in 1988)
#'     \item ``end_lon`` + 8 for 199100721 and + 2 for 199100770 (1991
#'       stations 721 and 770)
#'     \item ``tow_length`` over 5 nm, 0 or missing set to 2.5 nm (not used
#'       by the old index, which takes a fixed swept area)
#'   }
#'   These are errors in the Thünen data that should be fixed at the source;
#'   they are corrected here, not in [pax_ger_survey()], so the imported
#'   tables stay as delivered. ``geom`` and ``h3_cells`` of an imported
#'   ``ger_station`` come from the positions as delivered}
#' @rdname pax_ger
pax_ger_station_fix <- function(
  tbl,
  tow_length_default = 2.5,
  tow_length_max = 5,
  lon_end_fix = c("199100721" = 8, "199100770" = 2)
) {
  # NSE variables
  begin_lat <- end_lat <- begin_lon <- end_lon <- tow_length <- NULL
  sample_id <- NULL

  fix_ids <- as.numeric(names(lon_end_fix))
  fix_values <- unname(lon_end_fix)
  fix_end_lon <- quote(end_lon)
  for (i in seq_along(fix_ids)) {
    fix_end_lon <- substitute(
      dplyr::if_else(sample_id == id, end_lon + v, prev),
      list(id = fix_ids[i], v = fix_values[i], prev = fix_end_lon)
    )
  }

  tbl |>
    dplyr::mutate(
      begin_lat = dplyr::if_else(begin_lat < 50, begin_lat * 10, begin_lat),
      end_lat = dplyr::if_else(end_lat < 50, end_lat * 10, end_lat),
      begin_lon = -abs(begin_lon),
      end_lon = -abs(end_lon)
    ) |>
    dplyr::mutate(end_lon = !!fix_end_lon) |>
    dplyr::mutate(
      tow_length = dplyr::if_else(
        is.na(tow_length) |
          tow_length == 0 |
          tow_length > local(tow_length_max),
        local(tow_length_default),
        tow_length
      ),
      lat = (begin_lat + end_lat) / 2,
      lon = (begin_lon + end_lon) / 2
    )
}

#' @param ldist A dplyr query or data.frame of ``ger_ldist`` rows
#' @param station A dplyr query or data.frame of ``ger_station`` rows, with
#'   ``sample_id``, ``year`` and ``ger_stratum``
#' @param catch A dplyr query or data.frame of ``ger_catch`` rows
#' @param sample_ids Stations to impute, ``NULL`` (default) for all stations
#'   with a catch (``catch_count > 0``) and no length measurements. Or a
#'   data.frame with columns ``sample_id``, ``year`` and ``ger_stratum``
#'   giving the stations and the year and stratum to take the lengths from
#' @return \subsection{pax_ger_ldist_impute}{``ldist`` with rows added for
#'   stations with a catch but no length measurements (the rule of
#'   ``Greenland_Cochran.R``): the length distribution is the mean of
#'   ``count_measured`` by length over the ``ldist`` rows (by sex) of the
#'   same species, year and German stratum (``ger_stratum``), raised to the
#'   station's ``catch_count``. The added rows have ``sex`` ``NA``. Stations
#'   without a stratum, or whose stratum has no lengths that year, get no
#'   rows. ``Greenland_Cochran.R`` imputed 1986 stations 725, 750 and 751
#'   and 2010 station 1064 only (``sample_id`` 198600725, 198600750,
#'   198600751 and 201001064), with the lengths of 1986 stratum 6.2, 1986
#'   stratum 7.2 (twice) and 2011 stratum 6.2. The old script's id of
#'   station 1064 of 2010 was ``JAHR * 1000 + STATION`` = 2011064, so its
#'   lengths came from the year after. 1985 station 526 (East Greenland, one
#'   fish) was left without lengths and counted as a zero station}
#' @rdname pax_ger
pax_ger_ldist_impute <- function(ldist, station, catch, sample_ids = NULL) {
  # NSE variables
  sample_id <- species <- year <- ger_stratum <- catch_count <- NULL
  count_measured <- count <- sex <- NULL

  ldist_cols <- colnames(ldist)
  if (is.data.frame(ldist)) {
    # Work in the same place as ldist
    station <- dplyr::collect(station)
    catch <- dplyr::collect(catch)
  } else {
    pcon <- dbplyr::remote_con(ldist)
    station <- pax_temptbl(pcon, station)
    catch <- pax_temptbl(pcon, catch)
  }
  station <- dplyr::select(station, sample_id, year, ger_stratum)

  targets <- catch |>
    dplyr::filter(catch_count > 0) |>
    dplyr::anti_join(
      dplyr::distinct(ldist, sample_id, species),
      by = c("sample_id", "species")
    )
  target_cells <- station
  if (is.data.frame(sample_ids)) {
    target_cells <- sample_ids[, c("sample_id", "year", "ger_stratum")]
    if (!is.data.frame(ldist)) {
      target_cells <- pax_temptbl(pcon, target_cells)
    }
    sample_ids <- sample_ids$sample_id
  }
  if (!is.null(sample_ids)) {
    targets <- dplyr::filter(targets, sample_id %in% local(sample_ids))
  }

  donors <- ldist |>
    dplyr::inner_join(station, by = "sample_id", na_matches = "never") |>
    dplyr::group_by(species, year, ger_stratum, length) |>
    dplyr::summarise(
      count_measured = mean(count_measured, na.rm = TRUE),
      .groups = "drop"
    )

  imputed <- targets |>
    dplyr::select(sample_id, species, catch_count) |>
    dplyr::inner_join(target_cells, by = "sample_id", na_matches = "never") |>
    dplyr::inner_join(
      donors,
      by = c("species", "year", "ger_stratum"),
      na_matches = "never"
    ) |>
    dplyr::group_by(sample_id, species) |>
    dplyr::mutate(
      count = count_measured * catch_count / sum(count_measured, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(sex = NA_character_) |>
    dplyr::select(dplyr::all_of(ldist_cols))

  dplyr::union_all(ldist, imputed)
}

#' @param area Area of the East Greenland stratum (km²). The default
#'   24331.73 km² (7093.997 nm²) is the constant of ``Greenland_Cochran.R``;
#'   its source is undocumented
#' @param lon_range,lat_range The box of the stratum polygon (decimal
#'   degrees)
#' @return \subsection{pax_ger_strata}{An sf data.frame with one stratum,
#'   ``stratum`` 1, ``name`` ``"East Greenland"`` and ``rall_area`` =
#'   ``area``, decorated with ``pax_name`` ``"ger_strata"`` for
#'   [pax_import()] and use as ``strata_tbl`` of [pax_si_scale_by_strata()].
#'   The polygon is the box of the station selection of the old index
#'   (west of 44°W to the east limit of the stations, 61-66°N); its area is
#'   not ``rall_area``. As in the old index, stations are assigned to the
#'   stratum by selection (``strata_stations``), not by position, see the
#'   example}
#' @examples
#' \dontrun{
#' # The East Greenland golden redfish index of Greenland_Cochran.R
#' # (data/survey_greenland_by_sample.csv and survey_greenland_by_length.csv
#' # of 05-reg, before their fill-ins of years without a survey)
#' ger_station <- dplyr::tbl(pcon, "ger_station") |>
#'   pax_ger_station_fix() |>
#'   dplyr::filter(
#'     lon > -44, lat > 61, lat < 66, year > 1983,
#'     is.na(ger_stratum) | ger_stratum != 3.2
#'   ) |>
#'   # Fixed swept area of 2.2 nm x 41 m (in nm^2), not the tow distance.
#'   # pax_ldist_scale_tow_area() uses a gear width of 1 for sampling types
#'   # without tow dimensions
#'   dplyr::mutate(tow_length = 2.2 * 41 / 1852)
#' ger_ldist <- dplyr::tbl(pcon, "ger_ldist") |>
#'   pax_ger_ldist_impute(
#'     station = dplyr::tbl(pcon, "ger_station"),
#'     catch = dplyr::tbl(pcon, "ger_catch"),
#'     # The stations and lengths imputed by Greenland_Cochran.R
#'     sample_ids = data.frame(
#'       sample_id = c(198600725, 198600750, 198600751, 201001064),
#'       year = c(1986, 1986, 1986, 2011),
#'       ger_stratum = c(6.2, 7.2, 7.2, 6.2)
#'     )
#'   ) |>
#'   dplyr::filter(length < 72) |>
#'   pax_ldist_add_weight(data.frame(species = 5, a = 0.0109, b = 3.07))
#' by_sample <- ger_station |>
#'   pax_si_by_length(ldist = ger_ldist) |>
#'   pax_si_scale_by_strata(
#'     "ger_strata",
#'     strata_stations = dplyr::tbl(pcon, "ger_station") |>
#'       dplyr::distinct(station, sampling_type) |>
#'       dplyr::mutate(stratum = 1L)
#'   )
#' }
#' @rdname pax_ger
pax_ger_strata <- function(
  area = 24331.73,
  lon_range = c(-44, -26),
  lat_range = c(61, 66)
) {
  box <- matrix(
    c(
      lon_range[1],
      lat_range[1],
      lon_range[2],
      lat_range[1],
      lon_range[2],
      lat_range[2],
      lon_range[1],
      lat_range[2],
      lon_range[1],
      lat_range[1]
    ),
    ncol = 2,
    byrow = TRUE
  )
  out <- sf::st_sf(
    stratum = 1L,
    name = "East Greenland",
    rall_area = area,
    geometry = sf::st_sfc(sf::st_polygon(list(box)), crs = pax_def_crs())
  )
  pax_decorate(
    out,
    cite = "East Greenland stratum of Greenland_Cochran.R (05-redfish)",
    name = "ger_strata"
  )
}

# Helpers ---------------------------------------------------------------------

# Read all Thünen tables in path, list(stat, netz, fish, leng) of data.frames
# of character columns with upper-case names
ger_read_tables <- function(path) {
  files <- unlist(lapply(path, function(p) {
    if (dir.exists(p)) {
      list.files(
        p,
        pattern = "\\.(csv|xlsx|xls)$",
        ignore.case = TRUE,
        full.names = TRUE
      )
    } else if (file.exists(p)) {
      p
    } else {
      stop("No such file or directory: ", p)
    }
  }))
  # Skip Excel lock files
  files <- files[!startsWith(basename(files), "~$")]

  id_cols <- c(stat = "STATID", netz = "NETZID", fish = "FISHID", leng = "LENGID")
  out <- list()
  for (f in files) {
    df <- ger_read_file(f, id_cols)
    if (is.null(df)) {
      next
    }
    type <- names(id_cols)[match(TRUE, id_cols %in% colnames(df))]
    out[[type]] <- if (is.null(out[[type]])) {
      df
    } else {
      dplyr::bind_rows(out[[type]], df)
    }
  }
  out
}

# Read one file as character columns, NULL if it isn't a Thünen table
ger_read_file <- function(f, id_cols) {
  rename <- c(
    DEPTHMIN1 = "TIEFEMIN",
    DEPTHMAX1 = "TIEFEMAX",
    DEPTHMIN2 = "FTMIN",
    DEPTHMAX2 = "FTMAX",
    TIMEBEGINNING = "SZANF",
    TIMEEND = "SZENDE"
  )
  fix_names <- function(df) {
    colnames(df) <- toupper(colnames(df))
    i <- colnames(df) %in% names(rename)
    colnames(df)[i] <- rename[colnames(df)[i]]
    df
  }

  if (grepl("\\.xlsx?$", f, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("readxl package needed to read ", f)
    }
    for (sheet in readxl::excel_sheets(f)) {
      hdr <- toupper(colnames(readxl::read_excel(f, sheet = sheet, n_max = 0)))
      if (any(id_cols %in% hdr)) {
        df <- readxl::read_excel(f, sheet = sheet, col_types = "text")
        return(fix_names(as.data.frame(df, stringsAsFactors = FALSE)))
      }
    }
    return(NULL)
  }

  hdr <- toupper(strsplit(readLines(f, n = 1, warn = FALSE), ",")[[1]])
  hdr <- gsub('^"|"$', "", trimws(hdr))
  if (!any(id_cols %in% hdr)) {
    return(NULL)
  }
  df <- utils::read.csv(
    f,
    colClasses = "character",
    na.strings = c("", "NA"),
    check.names = FALSE,
    encoding = "UTF-8"
  )
  fix_names(df)
}

# Column of df, or NA if missing
ger_col <- function(df, col) {
  if (col %in% colnames(df)) df[[col]] else rep(NA, nrow(df))
}

# Numeric, -9 is missing
ger_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[x %in% -9] <- NA
  x
}

# sample_id of a German station: JAHR * 1e5 + STATION. Station numbers go
# over 1000 (up to 1386 by 2025), so the earlier JAHR * 1000 + STATION ran
# into the next year's ids. The ids (about 2e8) are far above the MFRI
# synis_id range (under 1e6), so they can be combined with MFRI stations
ger_id_mult <- 1e5

ger_sample_id <- function(df) {
  station <- ger_num(df$STATION)
  if (any(station < 0 | station >= ger_id_mult, na.rm = TRUE)) {
    stop(
      "STATION outside 0-",
      ger_id_mult - 1,
      ", sample_id (JAHR * ",
      ger_id_mult,
      " + STATION) would not be unique"
    )
  }
  ger_num(df$JAHR) * ger_id_mult + station
}

# Thünen position "DDMMmmH" / "DDDMMmmH" (degrees, minutes and hundredths of
# minutes, hemisphere N/S/E/W) to decimal degrees, as geo::geoconvert(). S
# and W negative
ger_pos <- function(x) {
  x <- trimws(as.character(x))
  hemi <- toupper(substring(x, nchar(x)))
  v <- suppressWarnings(as.numeric(gsub("[NSEWnsew]", "", x)))
  v[v %in% -9] <- NA
  deg <- trunc(v / 10000)
  min <- (v - deg * 10000) / 100
  out <- deg + min / 60
  ifelse(hemi %in% c("S", "W"), -out, out)
}

ger_check_unique <- function(sample_id, what) {
  dup <- unique(sample_id[duplicated(sample_id)])
  if (length(dup) > 0) {
    stop(
      what,
      " has more than one row per station (JAHR and STATION), ",
      "is the same table in more than one file? ",
      toString(utils::head(dup, 10))
    )
  }
}

# Sum the numeric columns of df by the by columns (NA if all NA)
ger_sum_by <- function(df, by) {
  if (nrow(df) == 0) {
    return(df)
  }
  value_cols <- setdiff(colnames(df), by)
  key <- do.call(paste, c(lapply(df[by], as.character), sep = "\r"))
  first <- !duplicated(key)
  out <- df[first, by, drop = FALSE]
  g <- factor(key, levels = key[first])
  for (col in value_cols) {
    v <- df[[col]]
    s <- tapply(v, g, function(x) {
      if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
    })
    out[[col]] <- as.numeric(s)
  }
  rownames(out) <- NULL
  out
}
