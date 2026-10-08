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

#' @return \subsection{pax_mar_landings_vessel}{Landings register by vessel
#'   and month (Directorate of Fisheries, ``kvoti.lods_oslaegt``, and the
#'   Fisheries Association's landings before 1994,
#'   ``fiskifelagid.landed_catch_pre94``, via ``mar::lods_oslaegt()`` and
#'   ``mar::fiskifelag_oslaegt()``), summed over landings. Columns
#'   ``source`` (``"lods"`` or ``"fiskifelag"``; the two overlap in 1992 and
#'   1993), ``year``, ``month``, ``species`` (``fteg``), ``vessel_id``
#'   (``skip_nr``), ``gear_id`` (landings gear code, ``veidarfaeri``),
#'   ``fishing_area`` (``veidisvaedi``, e.g. ``"I"`` for Icelandic waters),
#'   ``period`` (quota period, mar's ``timabil``, e.g. ``"20242025"``; the
#'   year for ``fiskifelag``), ``fishing_year`` (label as the old advice code,
#'   e.g. ``"2024/2025"``, the calendar year before September 1991) and
#'   ``catch`` (ungutted, kg)}
#' @rdname pax_mar_extra
pax_mar_landings_vessel <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  fteg <- ar <- man <- skip_nr <- veidarfaeri <- veidisvaedi <- NULL
  timabil <- fishing_year <- magn_oslaegt <- source <- year <- NULL

  one <- function(tbl, source) {
    tbl <- tbl |>
      dplyr::filter(fteg %in% local(species)) |>
      mar_fishing_year_label()
    if (!is.null(year_start)) {
      tbl <- dplyr::filter(tbl, ar >= local(year_start))
    }
    if (!is.null(year_end)) {
      tbl <- dplyr::filter(tbl, ar <= local(year_end))
    }
    tbl |>
      dplyr::group_by(
        year = ar,
        month = man,
        species = fteg,
        vessel_id = skip_nr,
        gear_id = veidarfaeri,
        fishing_area = veidisvaedi,
        period = timabil,
        fishing_year
      ) |>
      dplyr::summarise(
        catch = sum(magn_oslaegt, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::collect() |>
      dplyr::mutate(
        source = local(source),
        period = as.character(period),
        fishing_year = as.character(fishing_year),
        dplyr::across(
          c("year", "month", "species", "vessel_id", "gear_id", "catch"),
          as.numeric
        ),
        .before = 1
      )
  }

  dplyr::bind_rows(
    one(mar::lods_oslaegt(mar), "lods"),
    one(mar::fiskifelag_oslaegt(mar), "fiskifelag")
  ) |>
    dplyr::relocate("source") |>
    dplyr::arrange(source, year, month, species, vessel_id) |>
    decorate_mar()
}

#' @return \subsection{pax_mar_vessel}{The vessel register
#'   (``vessel.vessel_v`` with ``usage_category`` and ``construction_year``
#'   from ``vessel.vessel``), one row per vessel: ``vessel_id``
#'   (registration number, the ``vessel_id``/``boat_id`` of the other pax
#'   tables), ``name``, ``status`` (e.g. ``"Erlent"`` for foreign vessels),
#'   ``usage_category_no``, ``usage_category``, ``operational_category_no``,
#'   ``region_no``, ``home_port_no``, ``power_kw``, ``length``,
#'   ``brutto_grt``, ``brutto_weight_tons`` and ``construction_year``. Owner
#'   names, addresses and identity numbers are left out}
#' @rdname pax_mar_extra
pax_mar_vessel <- function(mar) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  registration_no <- name <- status <- usage_category_no <- NULL
  operational_category_no <- region_no <- home_port_no <- power_kw <- NULL
  length <- brutto_grt <- brutto_weight_tons <- vessel_id <- NULL
  usage_category <- construction_year <- NULL

  mar::les_skipaskra(mar) |>
    dplyr::left_join(
      mar::vessel(mar) |>
        dplyr::select(vessel_id, usage_category, construction_year),
      by = "vessel_id"
    ) |>
    dplyr::select(
      vessel_id = registration_no,
      name,
      status,
      usage_category_no,
      usage_category,
      operational_category_no,
      region_no,
      home_port_no,
      power_kw,
      length,
      brutto_grt,
      brutto_weight_tons,
      construction_year
    ) |>
    dplyr::collect() |>
    dplyr::arrange(vessel_id) |>
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

  adb <- mar::adb_catch(mar) |>
    dplyr::filter(species_no %in% local(species)) |>
    dplyr::mutate(year = year(registered)) |>
    dplyr::left_join(mar::adb_station(mar), by = 'station_id') |>
    dplyr::left_join(mar::adb_trip(mar), by = 'trip_id') |>
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
      dplyr::left_join(mar::afli_stofn(mar), by = "visir") |>
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
#'   before the compiled logbooks separate the redfish species}
#' @rdname pax_mar_extra
pax_mar_logbook_old <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  visir <- tegund <- vedags <- veman <- skipnr <- veidarf <- dypi <- NULL
  afli <- year <- NULL

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
      catch = afli
    ) |>
    decorate_mar()
}
