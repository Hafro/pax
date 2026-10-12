#' Survey station columns and invertebrate survey tables from MAR
#'
#' Optional columns of the ``station`` table and optional tables of
#' [pax_from_mar()] for the invertebrate surveys (inshore and offshore
#' shrimp, sampling types 31 and 37, and similar): fjord sub-areas, the
#' sample name, the length counts by maturity stage, the sample and catch
#' weights, and landings by stock.
#'
#' @param mar A MAR database connection, as returned by ``mar::connect_mar()``
#' @param species Integer vector of species codes
#' @param sampling_type Sampling types of the samples to import, ``NULL`` for
#'   all
#' @param year_start,year_end Optional years of the samples to import
#' @param skip_trips,only_trips SQL LIKE patterns of trips to leave out or to
#'   keep, as [pax_mar_station()]
#' @name pax_mar_survey
NULL

#' @return \subsection{pax_station_columns}{A named character vector: the
#'   optional columns of the ``station`` table (names) and the biota columns
#'   they come from (``channel.stod_v`` and ``channel.syni_v``)}
#' @rdname pax_mar_survey
pax_station_columns <- function() {
  c(
    # Survey area code (e.g. 52 Arnarfjordur, 53 Isafjardardjup)
    survey_area = "skiki",
    # Sub-area of the fjord (or survey area), the strata of the inshore
    # shrimp surveys
    survey_subarea = "fjardarreitur",
    # Name of the sample, e.g. "Varpa" (trawl) or "Skjoda" (bag) in the
    # shrimp survey
    sample_name = "syni_heiti",
    # Station number of the trip
    station_number = "stod_nr",
    # Statistical rectangle, the strata of the offshore shrimp survey
    rectangle = "reitur",
    bottom_temp = "botnhiti",
    surface_temp = "yfirbordshiti",
    bottom_depth_end = "botndypi_hift"
  )
}

# The extra columns of the station table, as a named vector for
# dplyr::select() (pax name = biota name)
station_columns_map <- function(columns) {
  map <- pax_station_columns()
  unknown <- setdiff(columns, names(map))
  if (length(unknown) > 0) {
    stop(
      "Unknown station columns: ",
      paste(unknown, collapse = ", "),
      " (see pax_station_columns())"
    )
  }
  map[columns]
}

# Sample ids of the stations of the given sampling types, years and trips
mar_survey_samples <- function(
  mar,
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL,
  skip_trips = NULL,
  only_trips = NULL
) {
  # NSE variables
  leidangur <- trip <- synis_id <- synaflokkur_nr <- ar <- year <- NULL

  out <- mar::les_stod(mar) |>
    dplyr::inner_join(mar::les_syni(mar), by = "stod_id") |>
    dplyr::select(
      sample_id = synis_id,
      trip = leidangur,
      sampling_type = synaflokkur_nr,
      year = ar
    ) |>
    mar_skip_trips(skip_trips)
  if (length(only_trips) > 0) {
    out <- mar_only_trips(out, only_trips)
  }
  if (!is.null(sampling_type)) {
    out <- dplyr::filter(out, sampling_type %in% local(sampling_type))
  }
  if (!is.null(year_start)) {
    out <- dplyr::filter(out, year >= local(year_start))
  }
  if (!is.null(year_end)) {
    out <- dplyr::filter(out, year <= local(year_end))
  }
  dplyr::select(out, "sample_id")
}

#' @return \subsection{pax_mar_ldist_maturity}{Length counts as measured
#'   (``biota.lengd_v``, not raised to the counted or the catch weight), by
#'   ``sample_id``, ``species``, ``length`` (as recorded, e.g. cm with 0.1 mm
#'   resolution for shrimp carapace length), ``sex``, ``maturity_stage``
#'   (``kynthroski_nr``) and ``count``, for the samples of the given
#'   sampling types and years. Raise with [pax_ldist_scale_catch_weight()]
#'   or [pax_ldist_scale_counted()]}
#' @rdname pax_mar_survey
pax_mar_ldist_maturity <- function(
  mar,
  species,
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL,
  skip_trips = NULL,
  only_trips = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  # NSE variables
  synis_id <- tegund_nr <- lengd <- kyn_nr <- kynthroski_nr <- fjoldi <- NULL
  sample_id <- sex <- maturity_stage <- count <- NULL

  mar::les_lengd(mar) |>
    dplyr::filter(tegund_nr %in% local(species)) |>
    dplyr::select(
      sample_id = synis_id,
      species = tegund_nr,
      length = lengd,
      sex = kyn_nr,
      maturity_stage = kynthroski_nr,
      count = fjoldi
    ) |>
    dplyr::semi_join(
      mar_survey_samples(
        mar,
        sampling_type,
        year_start,
        year_end,
        skip_trips,
        only_trips
      ),
      by = "sample_id"
    ) |>
    dplyr::group_by(sample_id, species, length, sex, maturity_stage) |>
    dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") |>
    decorate_mar()
}

#' @return \subsection{pax_mar_sample_weight}{The sample summary of
#'   ``biota.skalar_v`` by ``sample_id`` and ``species``: ``n_measured``
#'   (``maeldir``), ``n_counted`` (``taldir``), ``n_aged`` (``kvarnadir``),
#'   ``sample_weight`` (``vigt_synis``, weight of the measured sample, kg)
#'   and ``catch_weight`` (``vigt_afla``, weight of the catch of the species,
#'   kg), for the samples of the given sampling types and years. In the
#'   shrimp surveys (sampling types 31 and 37) ``catch_weight`` is positive
#'   in every sample with a ``sample_weight`` (checked October 2026); the
#'   zero catch weights of the groundfish surveys since 2010 don't occur
#'   there}
#' @rdname pax_mar_survey
pax_mar_sample_weight <- function(
  mar,
  species,
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL,
  skip_trips = NULL,
  only_trips = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  # NSE variables
  synis_id <- tegund_nr <- maeldir <- taldir <- kvarnadir <- NULL
  vigt_synis <- vigt_afla <- NULL

  mar::les_skala(mar) |>
    dplyr::filter(tegund_nr %in% local(species)) |>
    dplyr::select(
      sample_id = synis_id,
      species = tegund_nr,
      n_measured = maeldir,
      n_counted = taldir,
      n_aged = kvarnadir,
      sample_weight = vigt_synis,
      catch_weight = vigt_afla
    ) |>
    dplyr::semi_join(
      mar_survey_samples(
        mar,
        sampling_type,
        year_start,
        year_end,
        skip_trips,
        only_trips
      ),
      by = "sample_id"
    ) |>
    decorate_mar()
}

#' @return \subsection{pax_mar_landings_stock}{Landings by management stock
#'   or stock area code, aggregated (no vessel, port, seller or buyer):
#'   ``source`` (``"lods"`` and ``"fiskifelag"``: the landings register,
#'   ``mar::lods_oslaegt()`` and ``mar::fiskifelag_oslaegt()``, by the stock
#'   code ``kfteg``, e.g. 51 Arnarfjordur and 53 Isafjardardjup shrimp;
#'   ``"agf"``: ``agf.aflagrunnur_v`` by ``veidistofn``), ``year``,
#'   ``month``, ``species``, ``stock_code`` (``kfteg``, or ``veidistofn``
#'   for ``"agf"``), ``stock_name`` (``veidistofn_heiti``, ``NA`` for the
#'   register), ``gear_id`` (landings gear code), ``fishing_area``
#'   (``veidisvaedi``), ``period`` (quota period, e.g. ``"20242025"``, NA
#'   for ``"agf"``), ``fishing_year`` (as [pax_mar_landings_vessel()]) and
#'   ``catch`` (ungutted, kg)}
#' @rdname pax_mar_survey
pax_mar_landings_stock <- function(
  mar,
  species,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  # NSE variables
  fteg <- kfteg <- ar <- man <- veidarfaeri <- veidisvaedi <- timabil <- NULL
  fishing_year <- magn_oslaegt <- year <- month <- source <- NULL
  fisktegund <- veidistofn <- veidistofn_heiti <- londun_hefst <- NULL
  stock_code <- period <- fishing_area <- NULL

  years <- function(tbl) {
    if (!is.null(year_start)) {
      tbl <- dplyr::filter(tbl, year >= local(year_start))
    }
    if (!is.null(year_end)) {
      tbl <- dplyr::filter(tbl, year <= local(year_end))
    }
    tbl
  }
  register <- function(tbl, source) {
    tbl |>
      dplyr::filter(fteg %in% local(species)) |>
      mar_fishing_year_label() |>
      dplyr::rename(year = ar) |>
      years() |>
      dplyr::group_by(
        year,
        month = man,
        species = fteg,
        stock_code = kfteg,
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
        stock_name = NA_character_,
        period = as.character(period),
        fishing_year = as.character(fishing_year)
      )
  }
  agf <- mar::tbl_mar(mar, "agf.aflagrunnur_v") |>
    dplyr::filter(fisktegund %in% local(species)) |>
    dplyr::mutate(
      year = to_number(to_char(londun_hefst, "YYYY")),
      month = to_number(to_char(londun_hefst, "MM"))
    ) |>
    years() |>
    dplyr::group_by(
      year,
      month,
      species = fisktegund,
      stock_code = veidistofn,
      stock_name = veidistofn_heiti,
      gear_id = veidarfaeri,
      fishing_area = veidisvaedi
    ) |>
    dplyr::summarise(
      catch = sum(magn_oslaegt, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      source = "agf",
      period = NA_character_,
      # Fishing year September-August, as the register's label
      fishing_year = ifelse(
        month < 9,
        paste(year - 1, year, sep = "/"),
        paste(year, year + 1, sep = "/")
      ),
      fishing_area = as.character(fishing_area)
    )

  std <- function(x) {
    dplyr::mutate(
      x,
      dplyr::across(
        c("year", "month", "species", "stock_code", "gear_id", "catch"),
        as.numeric
      ),
      dplyr::across(
        c("stock_name", "fishing_area", "period", "fishing_year"),
        as.character
      )
    )
  }
  dplyr::bind_rows(
    std(register(mar::lods_oslaegt(mar), "lods")),
    std(register(mar::fiskifelag_oslaegt(mar), "fiskifelag")),
    std(agf)
  ) |>
    dplyr::select(
      source,
      year,
      month,
      species,
      stock_code,
      stock_name = "stock_name",
      gear_id = "gear_id",
      fishing_area = "fishing_area",
      period = "period",
      fishing_year,
      catch = "catch"
    ) |>
    dplyr::arrange(source, year, month, species, stock_code) |>
    decorate_mar()
}
