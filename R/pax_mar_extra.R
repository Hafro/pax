#' Import optional tables from the MAR database
#'
#' Functions for the optional tables of [pax_from_mar()] (argument
#' ``extra_tables``): data that only some stocks need, for tech report
#' figures, advice tables or model input, so that the assessment repositories
#' need no direct access to mar. Like the [pax_mar] functions, they return
#' tables decorated for [pax_import()].
#'
#' @param mar A MAR database connection, as returned by ``mar::connect_mar()``
#' @param species Integer vector of species codes to filter by
#' @param year_start Optional integer, earliest year to include
#' @param year_end Optional integer, latest year to include
#' @name pax_mar_extra
NULL

# Fishing year label from year (ar), month (man) and landing date (l_dags),
# as the old advice code (cod the_numbers.R, halibut advice/getData.R):
# calendar years before 1991 (and January-August 1991), then September-August
mar_fishing_year_label <- function(tbl) {
  # NSE variables
  ar <- man <- l_dags <- fishing_year <- NULL

  dplyr::mutate(
    tbl,
    fishing_year = dplyr::case_when(
      ar < 1991 ~ to_char(ar),
      ar == 1991 & man < 9 ~ to_char(ar),
      man < 9 ~
        paste(
          to_char(to_number(to_char(l_dags, "YYYY")) - 1),
          to_char(l_dags, "YYYY"),
          sep = '/'
        ),
      TRUE ~
        paste(
          to_char(l_dags, "YYYY"),
          to_char(to_number(to_char(l_dags, "YYYY")) + 1),
          sep = '/'
        )
    )
  )
}

# Quota period (timabil, e.g. "20242025") of a date column, as mar's
# lods_oslaegt()
mar_period_of <- function(tbl, date_col) {
  # NSE variables
  period <- NULL
  d <- rlang::ensym(date_col)

  dplyr::mutate(
    tbl,
    period = dplyr::if_else(
      to_number(to_char(!!d, "MM")) < 9,
      concat(
        to_number(to_char(!!d, "YYYY")) - 1,
        to_number(to_char(!!d, "YYYY"))
      ),
      concat(
        to_number(to_char(!!d, "YYYY")),
        to_number(to_char(!!d, "YYYY")) + 1
      )
    )
  )
}

# The Oracle logbook views of mar's old wrappers adb_*() and afli_stofn(),
# removed from mar in October 2026: mar reads the logbooks from the logbook
# parquet now (mar::lb_*()). That dataset has other ids, no sex, other
# condition codes and no afli.medafli, so the logbook tables here still read
# the Oracle views directly while they are there.
mar_afli_stofn <- function(mar) {
  # NSE variables
  vedags <- lengd <- breidd <- lengd_lok <- breidd_lok <- reitur <- NULL
  smareitur <- lat <- lon <- NULL

  mar::tbl_mar(mar, "afli.stofn") |>
    dplyr::mutate(
      ar = to_number(to_char(vedags, "YYYY")),
      man = to_number(to_char(vedags, "MM")),
      lengd = geoconvert1(-lengd * 100),
      breidd = geoconvert1(breidd * 100),
      lengd_lok = geoconvert1(-lengd_lok * 100),
      breidd_lok = geoconvert1(breidd_lok * 100)
    ) |>
    # Fix missing square info
    dplyr::mutate(
      reitur = nvl(reitur, d2r(breidd, lengd)),
      smareitur = nvl(
        nvl(smareitur, d2sr(breidd, lengd) - d2r(breidd, lengd) * 10),
        1
      )
    ) |>
    dplyr::left_join(mar::les_reit(mar), by = c("reitur", "smareitur")) |>
    dplyr::mutate(lengd = nvl(lengd, lon), breidd = nvl(breidd, lat)) |>
    dplyr::mutate(toglengd = arcdist(breidd, lengd, breidd_lok, lengd_lok)) |>
    dplyr::select(-c(lat, lon))
}

#' @return \subsection{pax_mar_landings_vessel}{Landings register by vessel
#'   and month, summed over landings: the landing base of the mar parquet
#'   datasets (``mar::ln_landings()``: the Directorate of Fisheries'
#'   ``kvoti.lods_slaegt`` from September 1992, ``agf.aflagrunnur`` from
#'   September 2007) and the Fisheries Association's landings 1982-1993
#'   (``fiskifelagid.landed_catch_pre94``, ``mar::ln_agg()``), which
#'   replaced mar's old ``lods_oslaegt()`` and ``fiskifelag_oslaegt()``.
#'   Columns ``source`` (``"lods"`` or ``"fiskifelag"``; the two overlap in
#'   1992 and 1993), ``year``, ``month``, ``species`` (``fteg``),
#'   ``vessel_id`` (``skip_nr``), ``gear_id`` (landings gear code,
#'   ``veidarfaeri``), ``fishing_area`` (``veidisvaedi``, e.g. ``"I"`` for
#'   Icelandic waters; the AGF areas mapped to these, see
#'   [agf_fishing_area]), ``period`` (quota period, mar's ``timabil``, e.g.
#'   ``"20242025"``; the year for ``fiskifelag``), ``fishing_year`` (label as
#'   the old advice code, e.g. ``"2024/2025"``, the calendar year before
#'   September 1991) and ``catch`` (ungutted, kg). From September 2007 the
#'   AGF landings, where ``lods_oslaegt()`` had the old register's}
#' @rdname pax_mar_extra
pax_mar_landings_vessel <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL
) {
  # NSE variables
  source <- year <- month <- vessel_id <- gear_id <- fishing_area <- NULL
  period <- fishing_year <- catch <- NULL

  mar_landings_register(
    species,
    year_start,
    year_end,
    sources = c("fiskifelag", "lods")
  ) |>
    dplyr::group_by(
      source,
      year,
      month,
      species,
      vessel_id,
      gear_id,
      fishing_area,
      period,
      fishing_year
    ) |>
    dplyr::summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(source, year, month, species, vessel_id) |>
    as.data.frame() |>
    decorate_mar()
}

#' @return \subsection{pax_mar_vessel}{The vessel register (AWS Postgres
#'   since October 2026, ``mar::vessel()`` and the current-state tables
#'   ``vessel_identification``, ``vessel_operational`` and
#'   ``vessel_measurement``; before, Oracle ``vessel.vessel_v``), one row
#'   per vessel: ``vessel_id``
#'   (registration number, the ``vessel_id``/``boat_id`` of the other pax
#'   tables), ``name``, ``status`` (e.g. ``"Erlent"`` for foreign vessels),
#'   ``usage_category_no``, ``usage_category``, ``operational_category_no``,
#'   ``region_no``, ``home_port_no``, ``power_kw``, ``length``,
#'   ``brutto_grt``, ``brutto_weight_tons`` and ``construction_year``. Owner
#'   names, addresses and identity numbers are left out. The ``mar``
#'   argument is not used (kept for [pax_from_mar()])}
#' @rdname pax_mar_extra
pax_mar_vessel <- function(mar) {
  mar_vessel_register() |>
    dplyr::select(
      "vessel_id",
      "name",
      "status",
      "usage_category_no",
      "usage_category",
      "operational_category_no",
      "region_no",
      "home_port_no",
      "power_kw",
      "length",
      "brutto_grt",
      "brutto_weight_tons",
      "construction_year"
    ) |>
    dplyr::arrange(dplyr::across("vessel_id")) |>
    as.data.frame() |>
    decorate_mar()
}

#' @param medafli_species Species codes of the old by-catch table
#'   ``afli.medafli`` to include (e.g. ``2021``, released halibut), ``NULL``
#'   for none
#' @return \subsection{pax_mar_logbook_release}{Logbook catch records of the
#'   species with their condition (released fish, ``condition == "RELE"``):
#'   one row per electronic logbook catch record (``source = "adb"``,
#'   ``ADB.CATCH_V`` with ``ADB.STATION_V`` and ``ADB.TRIP_V``, all
#'   conditions) and per record of the old by-catch table (``source =
#'   "medafli"``, ``afli.medafli`` with ``afli.stofn``, species
#'   ``medafli_species``). Columns ``source``, ``catch_id``, ``station_id``,
#'   ``vessel_id``, ``year`` (adb: year registered; medafli: year of the
#'   fishing day), ``gear_id``, ``species``, ``condition``, ``catch`` (kg)
#'   and ``count`` (medafli)}
#' @rdname pax_mar_extra
pax_mar_logbook_release <- function(mar, species, medafli_species = NULL) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  species_no <- registered <- station_id <- trip_id <- catch_id <- NULL
  vessel_no <- gear_no <- condition <- quantity <- year <- NULL
  visir <- skipnr <- ar <- veidarf <- tegund <- fjoldi <- NULL

  adb <- mar::tbl_mar(mar, "ADB.CATCH_V") |>
    dplyr::filter(species_no %in% local(species)) |>
    dplyr::mutate(year = year(registered)) |>
    dplyr::left_join(mar::tbl_mar(mar, "ADB.STATION_V"), by = 'station_id') |>
    dplyr::left_join(mar::tbl_mar(mar, "ADB.TRIP_V"), by = 'trip_id') |>
    dplyr::select(
      catch_id,
      station_id,
      vessel_id = vessel_no,
      year,
      gear_id = gear_no,
      species = species_no,
      condition,
      catch = quantity
    ) |>
    dplyr::collect() |>
    dplyr::mutate(source = "adb", .before = 1)

  out <- adb
  if (length(medafli_species) > 0) {
    medafli <- mar::tbl_mar(mar, "afli.medafli") |>
      dplyr::left_join(mar_afli_stofn(mar), by = "visir") |>
      dplyr::filter(tegund %in% local(medafli_species)) |>
      dplyr::select(
        station_id = visir,
        vessel_id = skipnr,
        year = ar,
        gear_id = veidarf,
        species = tegund,
        count = fjoldi
      ) |>
      dplyr::collect() |>
      dplyr::mutate(source = "medafli", .before = 1)
    out <- dplyr::bind_rows(out, medafli)
  }
  out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "catch_id",
          "station_id",
          "vessel_id",
          "year",
          "gear_id",
          "species",
          "catch",
          "count"
        )),
        as.numeric
      )
    ) |>
    decorate_mar()
}

#' @param sampling_type Sampling types (``synaflokkur_nr``) of the research
#'   trips
#' @return \subsection{pax_mar_research_landings}{Landings (gutted weight,
#'   ``kvoti.lods_slaegt``) of the research vessels during research trips:
#'   the vessel's landings in the landings register between the trip's
#'   departure and return (``brottfor``, ``koma``), as the old cod
#'   ``research_landings()``. A landing is counted once per sampling type of
#'   the trip. Columns ``vessel_id``, ``period`` (quota period of the landing
#'   date, e.g. ``"20242025"``), ``sampling_type``, ``species`` (``fteg``)
#'   and ``catch`` (gutted, kg)}
#' @rdname pax_mar_extra
# NB: kvoti.lods_slaegt (Oracle) for the gutted weights, which the landings
#     parquet of mar does not have
pax_mar_research_landings <- function(
  mar,
  species,
  sampling_type = c(10, 11, 30, 35, 40, 34, 20, 21, 19, 31, 37)
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  synaflokkur_nr <- ar <- skip_nr <- leidangur <- brottfor <- koma <- NULL
  l_dags <- period <- fteg <- magn <- NULL

  mar::les_stod(mar) |>
    dplyr::left_join(mar::les_syni(mar), by = "stod_id") |>
    dplyr::left_join(
      mar::les_leidangur(mar),
      by = c("leidangur_id", "leidangur")
    ) |>
    dplyr::filter(synaflokkur_nr %in% local(sampling_type)) |>
    dplyr::select(ar, skip_nr, leidangur, brottfor, koma, synaflokkur_nr) |>
    dplyr::distinct() |>
    dplyr::left_join(
      mar::tbl_mar(mar, 'kvoti.lods_slaegt') |>
        dplyr::mutate(ar = to_number(to_char(l_dags, "YYYY"))),
      by = c('ar', 'skip_nr')
    ) |>
    mar_period_of(l_dags) |>
    dplyr::filter(l_dags <= koma, l_dags >= brottfor) |>
    dplyr::filter(fteg %in% local(species)) |>
    dplyr::group_by(
      vessel_id = skip_nr,
      period,
      sampling_type = synaflokkur_nr,
      species = fteg
    ) |>
    dplyr::summarise(catch = sum(magn, na.rm = TRUE), .groups = 'drop') |>
    dplyr::collect() |>
    dplyr::mutate(
      period = as.character(period),
      dplyr::across(
        c("vessel_id", "sampling_type", "species", "catch"),
        as.numeric
      )
    ) |>
    decorate_mar()
}

#' @return \subsection{pax_mar_catch_disposition}{Landed catch by
#'   disposition (``agf.aflagrunnur`` with ``ask.afdrif``, e.g. catch landed
#'   outside the vessel's quota, "VS-afli"), summed by ``species``
#'   (``fisktegund``), ``year`` and ``month`` of the landing, ``period``
#'   (quota period of the landing date, e.g. ``"20242025"``),
#'   ``disposition`` (``afdrif``) and ``disposition_name`` (``heiti``).
#'   Columns ``catch`` (gutted, ``magn_slaegt``, kg) and ``catch_ungutted``
#'   (``magn_oslaegt``, kg). Buyers and sellers are left out}
#' @rdname pax_mar_extra
# NB: agf.aflagrunnur (Oracle) for the gutted weights, which the landings
#     parquet of mar does not have
pax_mar_catch_disposition <- function(mar, species) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  afdrif <- heiti <- londun_hefst <- fisktegund <- period <- NULL
  disposition_name <- magn_slaegt <- magn_oslaegt <- NULL

  mar::tbl_mar(mar, 'agf.aflagrunnur') |>
    dplyr::filter(fisktegund %in% local(species)) |>
    dplyr::left_join(
      mar::tbl_mar(mar, 'ask.afdrif') |>
        dplyr::select(afdrif, disposition_name = heiti),
      by = "afdrif"
    ) |>
    mar_period_of(londun_hefst) |>
    dplyr::group_by(
      species = fisktegund,
      year = to_number(to_char(londun_hefst, "YYYY")),
      month = to_number(to_char(londun_hefst, "MM")),
      period,
      disposition = afdrif,
      disposition_name
    ) |>
    dplyr::summarise(
      catch = sum(magn_slaegt, na.rm = TRUE),
      catch_ungutted = sum(magn_oslaegt, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      period = as.character(period),
      dplyr::across(
        c("species", "year", "month", "catch", "catch_ungutted"),
        as.numeric
      )
    ) |>
    decorate_mar()
}

#' @return \subsection{pax_mar_landings_old}{The cod landings "written in
#'   stone" (``ops$pamela."lnd_old"``, tonnes), by ``year``, ``month``,
#'   ``gid`` (gear group), ``country`` (``ccode``), ``fishing_year``
#'   (``yearf``), ``native`` and ``warning``, as used for the cod catch at age
#'   until 2022. The table holds cod only and has no species column}
#' @rdname pax_mar_extra
pax_mar_landings_old <- function(mar) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  year <- month <- gid <- catch <- ccode <- yearf <- native <- NULL
  warning <- NULL

  mar::tbl_mar(mar, 'ops$pamela."lnd_old"') |>
    dplyr::select(
      year,
      month,
      gid,
      country = ccode,
      fishing_year = yearf,
      native,
      warning,
      catch
    ) |>
    dplyr::collect() |>
    dplyr::mutate(dplyr::across(c("year", "month", "gid", "catch"), as.numeric)) |>
    decorate_mar()
}

#' @return \subsection{pax_mar_sample}{One row per biota sample (all years
#'   and trips, the stomach-sampling trips included) with length or age
#'   measurements of the species: ``sample_id``, ``haul_id``, ``year``,
#'   ``month``, ``sampling_type``, ``trip``, ``gear_id``, ``reitur``
#'   (rectangle), ``smareitur`` (subrectangle), ``gridcell`` (``10 * reitur +
#'   smareitur``) and ``vessel_id``. Use it to join the ``aldist`` and ``ldist`` tables,
#'   which hold all samples of the species, to their year and sampling type
#'   where the ``station`` table does not reach (before ``year_start``, or
#'   the trips left out by ``skip_trips``)}
#' @rdname pax_mar_extra
pax_mar_sample <- function(mar, species) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  synis_id <- stod_id <- ar <- man <- synaflokkur_nr <- leidangur <- NULL
  veidarfaeri <- reitur <- smareitur <- skip_nr <- tegund_nr <- NULL

  with_species <- dplyr::union(
    mar::les_lengd(mar) |>
      dplyr::filter(tegund_nr %in% local(species)) |>
      dplyr::select(synis_id),
    mar::les_aldur(mar) |>
      dplyr::filter(tegund_nr %in% local(species)) |>
      dplyr::select(synis_id)
  )

  mar::les_stod(mar) |>
    dplyr::inner_join(mar::les_syni(mar), by = 'stod_id') |>
    dplyr::semi_join(with_species, by = "synis_id") |>
    dplyr::transmute(
      sample_id = synis_id,
      haul_id = stod_id,
      year = ar,
      month = man,
      sampling_type = synaflokkur_nr,
      trip = leidangur,
      gear_id = veidarfaeri,
      reitur,
      smareitur,
      gridcell = 10 * reitur + smareitur,
      vessel_id = skip_nr
    ) |>
    decorate_mar()
}

#' @return \subsection{pax_mar_logbook_old}{The old logbook tables
#'   (``afli.afli`` with ``afli.stofn``, 1950-2024), one row per tow and
#'   species: ``logbook_id`` (``visir``), ``species`` (``tegund``),
#'   ``year`` and ``month`` (of the fishing day ``vedags``), ``veman``
#'   (the month column of ``afli.stofn``), ``vessel_id`` (``skipnr``),
#'   ``gear_id`` (``veidarf``), ``depth_fathoms`` (``dypi``) and ``catch``
#'   (``afli``, kg). Used e.g. for the monthly catch shares of the years
#'   before the compiled logbooks separate the redfish species. Optional
#'   ``columns``: ``"tow_time"`` (``afli.toga.togtimi``, minutes),
#'   ``"tow_length"`` (``afli.toga.togle``; for traps the length of the
#'   line),
#'   ``"num_traps"`` and ``"trap_hours"`` (``afli.gildra.gildrur`` and
#'   ``klst``), ``"lat"`` and ``"lon"`` (the start position, decimal
#'   degrees, as ``mar::afli_stofn()``), ``"rectangle"`` and
#'   ``"subrectangle"`` (``reitur``, ``smareitur``) and ``"n_same_catch"``
#'   (the number of catch rows of the record, any species, with the same
#'   catch as this one: the old scripts joined ``afli.afli`` back on
#'   ``visir`` and ``afli``, which repeats a row this many times),
#'   ``"num_nets"`` and ``"mesh_size"`` (``afli.lineha.dregin`` and
#'   ``moskvi``: nets hauled and mesh size of gillnet records)}
#' @param columns Optional extra columns of ``pax_mar_logbook_old()`` or
#'   ``pax_mar_logbook_adb()``, none by default
#' @rdname pax_mar_extra
pax_mar_logbook_old <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL,
  columns = character(0)
) {
  unknown <- setdiff(columns, pax_logbook_old_columns())
  if (length(unknown) > 0) {
    stop("Unknown logbook_old columns: ", paste(unknown, collapse = ", "))
  }
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  visir <- tegund <- vedags <- veman <- skipnr <- veidarf <- dypi <- NULL
  afli <- year <- togtimi <- gildrur <- klst <- breidd <- lengd <- NULL
  reitur <- smareitur <- togle <- dregin <- moskvi <- NULL

  # NB: afli.stofn directly, mar::afli_stofn() also converts the positions,
  #     which is slow and not needed here. year as mar::afli_stofn() (ar)
  out <- mar::tbl_mar(mar, "afli.afli") |>
    dplyr::filter(tegund %in% local(species)) |>
    dplyr::left_join(
      mar::tbl_mar(mar, "afli.stofn") |>
        dplyr::select(visir, vedags, veman, skipnr, veidarf, dypi),
      by = "visir"
    ) |>
    dplyr::mutate(
      year = to_number(to_char(vedags, "YYYY")),
      month = to_number(to_char(vedags, "MM"))
    )
  if (!is.null(year_start)) {
    out <- dplyr::filter(out, year >= local(year_start))
  }
  if (!is.null(year_end)) {
    out <- dplyr::filter(out, year <= local(year_end))
  }
  if (any(c("tow_time", "tow_length") %in% columns)) {
    out <- dplyr::left_join(
      out,
      mar::tbl_mar(mar, "afli.toga") |>
        dplyr::select(visir, tow_time = togtimi, tow_length = togle),
      by = "visir"
    )
  }
  if (any(c("num_traps", "trap_hours") %in% columns)) {
    out <- dplyr::left_join(
      out,
      mar::tbl_mar(mar, "afli.gildra") |>
        dplyr::select(visir, num_traps = gildrur, trap_hours = klst),
      by = "visir"
    )
  }
  if (any(c("num_nets", "mesh_size") %in% columns)) {
    # Nets hauled and mesh size of the gillnet records (one row per visir)
    out <- dplyr::left_join(
      out,
      mar::tbl_mar(mar, "afli.lineha") |>
        dplyr::select(visir, num_nets = dregin, mesh_size = moskvi),
      by = "visir"
    )
  }
  if (any(c("lat", "lon", "rectangle", "subrectangle") %in% columns)) {
    out <- dplyr::left_join(
      out,
      mar_afli_stofn(mar) |>
        dplyr::select(
          visir,
          lat = breidd,
          lon = lengd,
          rectangle = reitur,
          subrectangle = smareitur
        ),
      by = "visir"
    )
  }
  if ("n_same_catch" %in% columns) {
    out <- dplyr::left_join(
      out,
      mar::tbl_mar(mar, "afli.afli") |>
        dplyr::group_by(visir, afli) |>
        dplyr::summarise(n_same_catch = dplyr::n(), .groups = "drop"),
      by = c("visir", "afli")
    )
  }
  out |>
    dplyr::select(
      logbook_id = visir,
      species = tegund,
      year,
      month,
      veman,
      vessel_id = skipnr,
      gear_id = veidarf,
      depth_fathoms = dypi,
      catch = afli,
      dplyr::any_of(columns)
    ) |>
    decorate_mar()
}

#' @return \subsection{pax_logbook_old_columns}{The optional columns of
#'   [pax_mar_logbook_old()]}
#' @rdname pax_mar_extra
pax_logbook_old_columns <- function() {
  c(
    "tow_time",
    "tow_length",
    "num_traps",
    "trap_hours",
    "lat",
    "lon",
    "rectangle",
    "subrectangle",
    "n_same_catch",
    "num_nets",
    "mesh_size"
  )
}

#' @return \subsection{pax_logbook_adb_columns}{The optional columns of
#'   [pax_mar_logbook_adb()]}
#' @rdname pax_mar_extra
pax_logbook_adb_columns <- function() {
  c("sex", "num_nets", "mesh_size")
}

#' @return \subsection{pax_mar_logbook_adb}{The electronic logbooks (ADB,
#'   ``adb_trip``, ``adb_station``, ``adb_catch``, ``adb_trap`` joined as the
#'   old IAG scripts did), one row per station, species and trap record:
#'   ``station_id``, ``trip_id``, ``vessel_id`` (registration number),
#'   ``fishing_start``, ``year``, ``month``, ``lat``, ``lon`` (start
#'   position, as recorded), ``gear_id``, ``species``, ``quantity`` (kg) and
#'   ``number_of_traps``, plus the optional ``columns``
#'   ([pax_logbook_adb_columns()]): ``"sex"`` (``adb_catch.sex_no``, e.g.
#'   lumpfish females and males), ``"num_nets"`` and ``"mesh_size"``
#'   (``adb_line_net``: nets and mesh size of gillnet stations). Without
#'   vessel names, call signs or companies. The compiled logbooks round the
#'   catch of some records (whelk, up to 0.36 kg)}
#' @rdname pax_mar_extra
pax_mar_logbook_adb <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL,
  columns = character(0)
) {
  unknown <- setdiff(columns, pax_logbook_adb_columns())
  if (length(unknown) > 0) {
    stop("Unknown logbook_adb columns: ", paste(unknown, collapse = ", "))
  }
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  # NSE variables
  trip_id <- station_id <- species_no <- fishing_start <- NULL
  out <- mar::tbl_mar(mar, "ADB.TRIP_V") |>
    dplyr::left_join(mar::tbl_mar(mar, "ADB.STATION_V"), by = "trip_id") |>
    dplyr::left_join(mar::tbl_mar(mar, "ADB.CATCH_V"), by = "station_id") |>
    dplyr::left_join(mar::tbl_mar(mar, "ADB.TRAP_V"), by = "station_id")
  if (any(c("num_nets", "mesh_size") %in% columns)) {
    out <- dplyr::left_join(out, mar::tbl_mar(mar, "ADB.LINE_AND_NET_V"), by = "station_id")
  }
  out <- out |>
    dplyr::filter(species_no %in% local(species)) |>
    dplyr::select(
      "station_id", "trip_id", "vessel_no", "fishing_start", "latitude",
      "longitude", "gear_no", "species_no", "quantity", "number_of_traps",
      dplyr::any_of(c(
        sex_no = if ("sex" %in% columns) "sex_no",
        nets = if ("num_nets" %in% columns) "nets",
        mesh_size = if ("mesh_size" %in% columns) "mesh_size"
      ))
    ) |>
    dplyr::collect()
  raw <- out
  num <- function(x) suppressWarnings(as.numeric(x))
  out <- tibble::tibble(
    station_id = num(out$station_id),
    trip_id = num(out$trip_id),
    vessel_id = num(out$vessel_no),
    fishing_start = out$fishing_start,
    year = as.numeric(format(out$fishing_start, "%Y")),
    month = as.numeric(format(out$fishing_start, "%m")),
    lat = num(out$latitude),
    lon = num(out$longitude),
    gear_id = num(out$gear_no),
    species = num(out$species_no),
    quantity = num(out$quantity),
    number_of_traps = num(out$number_of_traps)
  )
  if ("sex" %in% columns) out$sex <- num(raw$sex_no)
  if ("num_nets" %in% columns) out$num_nets <- num(raw$nets)
  if ("mesh_size" %in% columns) out$mesh_size <- num(raw$mesh_size)
  if (!is.null(year_start)) out <- out[out$year >= year_start, ]
  if (!is.null(year_end)) out <- out[out$year <= year_end, ]
  decorate_mar(out)
}

#' @return \subsection{pax_mar_logbook_lumpfish}{The lumpfish logbooks
#'   (``afli.grasl_sokn``, one row per haul of nets, joined to the season
#'   table ``afli.grasl_stofn`` on vessel and year): ``vessel_id``,
#'   ``date``, ``year``, ``month``, ``rectangle``, ``grasl_rectangle``,
#'   ``depth`` (fathoms), ``num_nets`` (nets hauled), ``nights`` (soak
#'   nights), ``n_female`` and ``n_male`` (lumpfish and lumpsucker males
#'   counted), ``roe_kg``, and from the season table ``processing`` (roe
#'   processing type, ``teg_verkunar``), ``net_type``, ``mesh_size``,
#'   ``barrels`` and ``harbour``. Without the comment and the sender's
#'   identifier}
#' @rdname pax_mar_extra
pax_mar_logbook_lumpfish <- function(mar, year_start = NULL, year_end = NULL) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  # NSE variables
  skipnr <- vear <- vedags <- reitur <- grasl_reitur <- dypi <- dregin <- NULL
  naetur <- fj_grasl <- fj_raudm <- kg_hrogn <- teg_verkunar <- NULL
  gerd_neta <- moskvi <- tunnur <- ahofn <- year <- NULL

  out <- mar::tbl_mar(mar, "afli.grasl_sokn") |>
    dplyr::select(
      skipnr, vedags, vear, reitur, grasl_reitur, dypi, dregin, naetur,
      fj_grasl, fj_raudm, kg_hrogn
    ) |>
    dplyr::left_join(
      mar::tbl_mar(mar, "afli.grasl_stofn") |>
        dplyr::select(skipnr, vear, teg_verkunar, gerd_neta, moskvi, tunnur, ahofn),
      by = c("skipnr", "vear")
    )
  if (!is.null(year_start)) {
    out <- dplyr::filter(out, vear >= local(year_start))
  }
  if (!is.null(year_end)) {
    out <- dplyr::filter(out, vear <= local(year_end))
  }
  out <- dplyr::collect(out)
  num <- function(x) suppressWarnings(as.numeric(x))
  tibble::tibble(
    vessel_id = num(out$skipnr),
    date = as.Date(out$vedags),
    year = num(out$vear),
    month = as.numeric(format(out$vedags, "%m")),
    rectangle = num(out$reitur),
    grasl_rectangle = num(out$grasl_reitur),
    depth = num(out$dypi),
    num_nets = num(out$dregin),
    nights = num(out$naetur),
    n_female = num(out$fj_grasl),
    n_male = num(out$fj_raudm),
    roe_kg = num(out$kg_hrogn),
    # Character: the codes are "1" to "4" (salted, frozen, ...)
    processing = as.character(out$teg_verkunar),
    net_type = as.character(out$gerd_neta),
    mesh_size = num(out$moskvi),
    barrels = num(out$tunnur),
    harbour = num(out$ahofn)
  ) |>
    decorate_mar()
}
