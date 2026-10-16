#' Import survey tables from the MAR database
#'
#' Optional tables of [pax_from_mar()] (argument ``extra_tables``) for the
#' survey products (the public survey site, stofnmaelingar): station
#' environment, temperature loggers, stomach contents, the catch of all
#' species and the species taxonomy. Like the [pax_mar] functions, they
#' return tables decorated for [pax_import()].
#'
#' @param mar A MAR database connection, as returned by ``mar::connect_mar()``
#' @param sampling_type Integer vector of sampling type codes, ``NULL`` for
#'   all
#' @param year_start Optional integer, earliest year to include
#' @param year_end Optional integer, latest year to include
#' @name pax_mar_survey
NULL

# biota stations and samples of the sampling types and years, all trips
mar_survey_samples <- function(mar, sampling_type, year_start, year_end) {
  # NSE variables
  synaflokkur_nr <- ar <- NULL

  out <- mar::les_stod(mar) |>
    dplyr::inner_join(mar::les_syni(mar), by = "stod_id")
  if (!is.null(sampling_type)) {
    out <- dplyr::filter(out, synaflokkur_nr %in% local(sampling_type))
  }
  if (!is.null(year_start)) {
    out <- dplyr::filter(out, ar >= local(year_start))
  }
  if (!is.null(year_end)) {
    out <- dplyr::filter(out, ar <= local(year_end))
  }
  out
}

#' @return \subsection{pax_mar_station_env}{One row per sample, with the
#'   station records the ``station`` table leaves out: ``sample_id``,
#'   ``date``, ``trip_station`` (station number within the trip),
#'   ``rectangle`` and ``subrectangle`` (``reitur``, ``smareitur``),
#'   ``end_depth`` (bottom depth at the end of the tow, m), ``bottom_temp``
#'   and ``surface_temp`` (deg C, recorded at the station). All trips, so
#'   it also covers the stations of the stomach-sampling trips}
#' @rdname pax_mar_survey
pax_mar_station_env <- function(
  mar,
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  synis_id <- dags <- stod_nr <- botndypi_hift <- botnhiti <- NULL
  yfirbordshiti <- reitur <- smareitur <- NULL

  mar_survey_samples(mar, sampling_type, year_start, year_end) |>
    dplyr::select(
      sample_id = synis_id,
      date = dags,
      trip_station = stod_nr,
      rectangle = reitur,
      subrectangle = smareitur,
      end_depth = botndypi_hift,
      bottom_temp = botnhiti,
      surface_temp = yfirbordshiti
    ) |>
    decorate_mar()
}

#' @return \subsection{pax_mar_temperature_logger}{The readings of the
#'   temperature loggers on the trawl (``siritar.hitastig``), one row per
#'   reading: ``sample_id``, ``time``, ``time_offset_min`` (logger clock
#'   correction, minutes), ``temperature`` (deg C) and ``depth`` (m)}
#' @rdname pax_mar_survey
pax_mar_temperature_logger <- function(
  mar,
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  synis_id <- timi <- timi_hlidrun_min <- hitastig <- dypi <- NULL

  samples <- mar_survey_samples(mar, sampling_type, year_start, year_end) |>
    # NB: synis_id is text in les_syni, a number in siritar.hitastig
    dplyr::transmute(synis_id = as.numeric(synis_id))

  mar::siritar_hitastig(mar) |>
    dplyr::semi_join(samples, by = "synis_id") |>
    dplyr::select(
      # Text, as sample_id of the station table
      sample_id = synis_id,
      time = timi,
      time_offset_min = timi_hlidrun_min,
      temperature = hitastig,
      depth = dypi
    ) |>
    dplyr::mutate(sample_id = as.character(sample_id)) |>
    decorate_mar()
}

#' @param species Integer vector of species codes to filter by, ``NULL`` for
#'   all
#' @return \subsection{pax_mar_stomach}{The predators of the stomach
#'   samples (``ranfiskur``), one row per predator record: ``stomach_id``,
#'   ``sample_id``, ``species``, ``measurement_type``, ``length``,
#'   ``length_class`` (length group code), ``sex``, ``maturity_stage``,
#'   ``weight``, ``age``, ``n_stomach`` (stomachs in the record),
#'   ``n_empty``, ``n_regurgitated``, ``n_undigested`` and ``n_everted``.
#'   All trips, the stomach-sampling trips included}
#' @rdname pax_mar_survey
pax_mar_stomach <- function(
  mar,
  species = NULL,
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  synis_id <- ranfiskur_id <- tegund_nr <- maeling_teg <- lengd <- NULL
  lengd_nr <- kyn_nr <- kynthroski_nr <- thyngd <- aldur <- magar <- NULL
  tomir <- aeldir <- omeltir <- uthverfir <- NULL

  samples <- mar_survey_samples(mar, sampling_type, year_start, year_end) |>
    dplyr::select(synis_id)

  out <- mar::les_ranfisk(mar) |>
    dplyr::semi_join(samples, by = "synis_id")
  if (!is.null(species)) {
    out <- dplyr::filter(out, tegund_nr %in% local(species))
  }
  out |>
    dplyr::select(
      stomach_id = ranfiskur_id,
      sample_id = synis_id,
      species = tegund_nr,
      measurement_type = maeling_teg,
      length = lengd,
      length_class = lengd_nr,
      sex = kyn_nr,
      maturity_stage = kynthroski_nr,
      weight = thyngd,
      age = aldur,
      n_stomach = magar,
      n_empty = tomir,
      n_regurgitated = aeldir,
      n_undigested = omeltir,
      n_everted = uthverfir
    ) |>
    decorate_mar()
}

# Prey of the stomachs of pax_mar_stomach()
mar_stomach_ids <- function(mar, species, sampling_type, year_start, year_end) {
  # NSE variables
  synis_id <- tegund_nr <- NULL

  samples <- mar_survey_samples(mar, sampling_type, year_start, year_end) |>
    dplyr::select(synis_id)
  out <- mar::les_ranfisk(mar) |>
    dplyr::semi_join(samples, by = "synis_id")
  if (!is.null(species)) {
    out <- dplyr::filter(out, tegund_nr %in% local(species))
  }
  dplyr::select(out, "ranfiskur_id")
}

#' @return \subsection{pax_mar_stomach_prey}{The prey weights of the
#'   stomachs (``brad_thyngd``), one row per prey group and stomach record:
#'   ``prey_id``, ``stomach_id``, ``prey_species``, ``prey_group`` (food
#'   group code, ``faeduhopur``), ``prey_maturity_stage``,
#'   ``prey_maturity``, ``measurement_type``, ``prey_count`` and
#'   ``prey_weight`` (g). The weights are as recorded: a weight missing or 0
#'   with a count is left to the user}
#' @rdname pax_mar_survey
pax_mar_stomach_prey <- function(
  mar,
  species = NULL,
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  brad_id <- ranfiskur_id <- brad_tegund_nr <- faeduhopur <- NULL
  brad_throski_nr <- brad_throski <- brad_maeling_teg <- NULL
  brad_fjoldi <- brad_thyngd <- NULL

  mar::les_brad_thyngd(mar) |>
    dplyr::semi_join(
      mar_stomach_ids(mar, species, sampling_type, year_start, year_end),
      by = "ranfiskur_id"
    ) |>
    dplyr::select(
      prey_id = brad_id,
      stomach_id = ranfiskur_id,
      prey_species = brad_tegund_nr,
      prey_group = faeduhopur,
      prey_maturity_stage = brad_throski_nr,
      prey_maturity = brad_throski,
      measurement_type = brad_maeling_teg,
      prey_count = brad_fjoldi,
      prey_weight = brad_thyngd
    ) |>
    decorate_mar()
}

#' @return \subsection{pax_mar_stomach_prey_length}{The prey length
#'   measurements of the stomachs (``brad_lengd``): ``prey_id``,
#'   ``stomach_id``, ``prey_species``, ``prey_group``,
#'   ``prey_maturity_stage``, ``prey_maturity``, ``measurement_type``,
#'   ``prey_count``, ``prey_length``, ``prey_length_class``, ``prey_sex``
#'   and ``prey_sex_maturity``}
#' @rdname pax_mar_survey
pax_mar_stomach_prey_length <- function(
  mar,
  species = NULL,
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  brad_id <- ranfiskur_id <- brad_tegund_nr <- faeduhopur <- NULL
  brad_throski_nr <- brad_throski <- brad_maeling_teg <- brad_fjoldi <- NULL
  brad_lengd <- brad_lengd_nr <- brad_kyn_nr <- brad_kynthroski_nr <- NULL

  mar::les_brad_lengd(mar) |>
    dplyr::semi_join(
      mar_stomach_ids(mar, species, sampling_type, year_start, year_end),
      by = "ranfiskur_id"
    ) |>
    dplyr::select(
      prey_id = brad_id,
      stomach_id = ranfiskur_id,
      prey_species = brad_tegund_nr,
      prey_group = faeduhopur,
      prey_maturity_stage = brad_throski_nr,
      prey_maturity = brad_throski,
      measurement_type = brad_maeling_teg,
      prey_count = brad_fjoldi,
      prey_length = brad_lengd,
      prey_length_class = brad_lengd_nr,
      prey_sex = brad_kyn_nr,
      prey_sex_maturity = brad_kynthroski_nr
    ) |>
    decorate_mar()
}

#' @return \subsection{pax_mar_survey_catch}{The catch of every species in
#'   the samples of the sampling types: ``sample_id``, ``species`` and
#'   ``count`` (measured fish raised by the counted ones, as the ``ldist``
#'   table, summed over lengths and sexes). For e.g. the species caught at
#'   each survey station; ``ldist`` holds only the species of
#'   [pax_from_mar()]}
#' @rdname pax_mar_survey
pax_mar_survey_catch <- function(
  mar,
  sampling_type = NULL,
  year_start = NULL,
  year_end = NULL
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  synis_id <- tegund_nr <- fjoldi <- sample_id <- species <- count <- NULL

  samples <- mar_survey_samples(mar, sampling_type, year_start, year_end) |>
    dplyr::select(synis_id)

  mar::les_lengd(mar) |>
    dplyr::semi_join(samples, by = "synis_id") |>
    mar::skala_med_taldir() |>
    dplyr::select(sample_id = synis_id, species = tegund_nr, count = fjoldi) |>
    dplyr::group_by(sample_id, species) |>
    dplyr::summarize(count = sum(count, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    decorate_mar()
}

#' @return \subsection{pax_mar_taxonomy}{The species register
#'   (``les_tegund``), one row per species code: ``species``, ``name``
#'   (Icelandic), ``name_latin``, ``name_en``, ``rank``, ``prey_group``
#'   (food group code of stomach prey), ``worms_id``, ``fao_3alpha``,
#'   ``genus``, ``family``, ``order``, ``class``, ``phylum`` and
#'   ``kingdom``}
#' @rdname pax_mar_survey
pax_mar_taxonomy <- function(mar) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  tegund_nr <- heiti <- visinda_heiti <- enskt_heiti <- stigveldi <- NULL
  faeduhopur <- worms_id <- fao_3alpha <- aettkvisl <- aett <- NULL
  aettbalkur <- flokkur <- fylking <- riki <- species <- NULL

  mar::les_tegund(mar) |>
    dplyr::select(
      species = tegund_nr,
      name = heiti,
      name_latin = visinda_heiti,
      name_en = enskt_heiti,
      rank = stigveldi,
      prey_group = faeduhopur,
      worms_id,
      fao_3alpha,
      genus = aettkvisl,
      family = aett,
      order = aettbalkur,
      class = flokkur,
      phylum = fylking,
      kingdom = riki
    ) |>
    dplyr::collect() |>
    dplyr::arrange(species) |>
    decorate_mar()
}
