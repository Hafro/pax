#' Survey biomass indices of invertebrate surveys by sub-area
#'
#' The index of the inshore shrimp surveys (and other surveys stratified by
#' a station column such as the fjord sub-area), as the old
#' ``Data/leidangur.r`` scripts of the shrimp assessments: stations get their
#' stratum and its area from a station column; the counts are raised to the
#' catch weight and to a standard tow; the biomass of each station is the
#' measured sample weight raised to the catch, and is split by length, sex
#' and maturity stage with length-weight relationships; the index is the sum
#' over strata of the mean station biomass (times the stratum area), with
#' the variance the sum of the strata's variances of the mean.
#'
#' - [pax_si_station_strata()] gives each station its stratum and area;
#' - [pax_si_catch_weight_biomass()] gives the numbers and biomass by station,
#'   length, sex and maturity stage, with the fishable, juvenile and female
#'   parts;
#' - [pax_si_stratified_index()] sums them to an index by year, with CVs and
#'   log-normal 95% intervals;
#' - [pax_shrimp_towdims()] gives the tow standards of the shrimp surveys.
#'
#' @param station_tbl A dplyr query of the station table (with the station
#'   columns of ``by``, see [pax_station_columns()]), already filtered to
#'   the survey's stations
#' @examples
#' \dontrun{
#' pcon <- pax_connect("pax.duckdb")
#' st <- dplyr::tbl(pcon, "station") |>
#'   dplyr::filter(sampling_type == 37, survey_area == 52, sample_name == "Varpa")
#' strata <- pax_shrimp_strata("arnarfjordur")
#' st <- pax_si_station_strata(st, strata)
#' lw <- data.frame(
#'   species = 41, sex = c(1, 2, 2), maturity_stage = c(NA, 1, 3),
#'   a = c(5.36e-4, 5.36e-4, 6.25e-4), b = c(3.01, 3.01, 2.99)
#' )
#' bio <- pax_si_catch_weight_biomass(
#'   st, species = 41, lw_coeffs = lw,
#'   selection = data.frame(length = seq(13, 15.5, 0.5), p = (1:6) / 7)
#' )
#' pax_si_stratified_index(bio, c("b", "b_fishable"))
#' }
#' @name pax_si_subarea
NULL

#' @param strata A data frame of strata: the columns of ``by``, ``stratum``,
#'   ``area`` and optionally ``year_start`` and ``year_end`` (``NA`` for no
#'   limit) where the areas change between years, e.g. [shrimp_strata]
#' @param by Station columns the strata are defined by
#' @return \subsection{pax_si_station_strata}{``station_tbl`` with the
#'   columns ``stratum`` and ``area`` added (``NA`` for stations outside the
#'   strata, which the index leaves out, as the old scripts did)}
#' @rdname pax_si_subarea
pax_si_station_strata <- function(
  station_tbl,
  strata,
  by = c("survey_area", "survey_subarea")
) {
  pcon <- dbplyr::remote_con(station_tbl)
  # NSE variables
  year <- NULL

  missing_cols <- setdiff(c(by, "stratum", "area"), names(strata))
  if (length(missing_cols) > 0) {
    stop("strata has no column(s) ", paste(missing_cols, collapse = ", "))
  }
  years <- station_tbl |>
    dplyr::distinct(year) |>
    dplyr::pull(year)
  if (!"year_start" %in% names(strata)) strata$year_start <- NA
  if (!"year_end" %in% names(strata)) strata$year_end <- NA
  # One row per year and stratum
  by_year <- do.call(
    rbind,
    lapply(years, function(y) {
      s <- strata[
        (is.na(strata$year_start) | strata$year_start <= y) &
          (is.na(strata$year_end) | strata$year_end >= y),
        c(by, "stratum", "area"),
        drop = FALSE
      ]
      if (nrow(s) == 0) {
        return(NULL)
      }
      s$year <- y
      s
    })
  )
  if (is.null(by_year)) {
    stop("No strata for the years of the stations")
  }
  dup <- duplicated(by_year[c(by, "year")])
  if (any(dup)) {
    stop("strata has more than one row for a station in a year")
  }
  station_tbl |>
    dplyr::left_join(
      pax_temptbl(pcon, as.data.frame(by_year)),
      by = c(by, "year")
    )
}

#' @param area_unit ``"km2"`` (default) for areas in km² (as the inshore
#'   shrimp strata), or ``"nm2"`` for square nautical miles
#' @return \subsection{pax_shrimp_towdims}{A data frame of tow standards for
#'   [pax_ldist_scale_tow_area()]: the inshore shrimp surveys (sampling types
#'   37, 10 and 11), as the old scripts (``mar::skala_med_toglengd(min_towlength =
#'   0.01, std_towlength = 1)``, maximum 8 nm, and a trawl of 14.9 m, so the
#'   swept area of a standard tow of 1 nm is 0.0149 × 1.852 km²). The pax
#'   default for sampling type 37 has the same trawl width (27.595 / 1.852² /
#'   1000 nm²) but tows of 0.5-4 nm}
#' @rdname pax_si_subarea
pax_shrimp_towdims <- function(area_unit = c("km2", "nm2")) {
  area_unit <- match.arg(area_unit)
  width <- 0.0149 * 1.852
  if (area_unit == "nm2") {
    width <- width / 1.852^2
  }
  data.frame(
    sampling_type = c(37, 10, 11),
    min_towlength = 0.01,
    max_towlength = 8,
    std_towlength = 1,
    std_width = width
  )
}

#' @param species The species (one)
#' @param ldist_tbl Length counts by sample, length, sex and maturity stage,
#'   not raised (the ``ldist_maturity`` table)
#' @param sample_weight_tbl The ``sample_weight`` table
#' @param lw_coeffs Length-weight coefficients for
#'   [pax_ldist_add_weight()] (``by = lw_by``, ``missing = "zero"``), for
#'   lengths after [pax_ldist_round_length()] (e.g. mm) and weight in g
#' @param lw_by Columns of ``lw_coeffs`` to match on
#' @param selection The fishable ramp, see [pax_ldist_add_selection()]
#' @param towdims_tbl Tow standards for [pax_ldist_scale_tow_area()], by
#'   default [pax_shrimp_towdims()]
#' @param length_scale,length_unit Passed to [pax_ldist_round_length()]
#' @param female_sex Sex code of females, for the female biomass
#' @param tow_length_missing What a station without a tow length gets:
#'   ``"zero"`` (default, as the old shrimp scripts, where
#'   ``mar::skala_med_toglengd()`` gives ``NA`` and the station's catch then
#'   counts as 0 in the stratum mean) or ``"one"`` (a tow of 1 nm, as
#'   [pax_ldist_scale_tow_area()])
#' @return \subsection{pax_si_catch_weight_biomass}{A dplyr query, one row
#'   per station and length, sex and maturity stage (one row with ``length``
#'   ``NA`` for stations without the species): ``sample_id``, ``year``,
#'   ``stratum``, ``area``, ``length``, ``sex``, ``maturity_stage``, ``n``
#'   (numbers in the stratum, the counts raised to the catch weight, per
#'   standard tow and times the area over the swept area), ``b_sample``
#'   (tonnes, from the mean weight of the measured sample), ``b`` (tonnes,
#'   from the length-weight relationships, scaled so that the station's
#'   total is that of ``b_sample``), ``b_fishable`` (``b`` times the
#'   fishable fraction), ``b_juvenile`` (``b - b_fishable``) and
#'   ``b_female``. Stations whose sample has no sample weight get biomass 0,
#'   as in the old scripts}
#' @rdname pax_si_subarea
pax_si_catch_weight_biomass <- function(
  station_tbl,
  species,
  ldist_tbl = dplyr::tbl(dbplyr::remote_con(station_tbl), "ldist_maturity"),
  sample_weight_tbl = "sample_weight",
  lw_coeffs,
  lw_by = c("species", "sex", "maturity_stage"),
  selection,
  towdims_tbl = pax_shrimp_towdims(),
  length_scale = 10,
  length_unit = 0.5,
  female_sex = 2,
  tow_length_missing = c("zero", "one")
) {
  tow_length_missing <- match.arg(tow_length_missing)
  # NSE variables
  sample_id <- count <- area <- n <- mean_weight <- b_sample <- b <- NULL
  weight <- p_fishable <- b_fishable <- sex <- stratum <- year <- NULL
  maturity_stage <- s_sample <- s_lw <- tow_length <- NULL

  if (length(species) != 1) {
    stop("pax_si_catch_weight_biomass() takes one species")
  }
  sp <- species
  if (!all(c("stratum", "area") %in% colnames(station_tbl))) {
    stop("station_tbl needs stratum and area, see pax_si_station_strata()")
  }
  st <- station_tbl |>
    dplyr::select(
      "sample_id",
      "year",
      "sampling_type",
      "gear_id",
      "tow_length",
      "stratum",
      "area"
    )
  ld <- ldist_tbl |>
    dplyr::filter(species == local(sp)) |>
    dplyr::select("sample_id", "length", "sex", "maturity_stage", "count")

  st |>
    dplyr::left_join(ld, by = "sample_id") |>
    # Stations without the species: one row with count 0
    dplyr::mutate(species = local(sp), count = dplyr::coalesce(count, 0)) |>
    pax_ldist_scale_catch_weight(sample_weight_tbl) |>
    pax_ldist_round_length(scale = length_scale, unit = length_unit) |>
    (function(tbl) {
      if (tow_length_missing == "zero") {
        # The old scripts: no tow length, no catch (the station stays, as 0)
        tbl <- dplyr::mutate(tbl, count = ifelse(is.na(tow_length), NA, count))
      }
      tbl
    })() |>
    pax_ldist_scale_tow_area(towdims_tbl = towdims_tbl) |>
    dplyr::mutate(
      n = dplyr::coalesce(count, 0) * area,
      b_sample = n * dplyr::coalesce(mean_weight, 0) / 1000
    ) |>
    pax_ldist_add_weight(lw_coeffs, by = lw_by, missing = "zero") |>
    dplyr::mutate(b = n * dplyr::coalesce(weight, 0) / 1e6) |>
    dplyr::group_by(sample_id) |>
    dplyr::mutate(s_sample = sum(b_sample, na.rm = TRUE), s_lw = sum(b, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(b = ifelse(s_lw == 0, 0, b * s_sample / s_lw)) |>
    pax_ldist_add_selection(selection, name = "p_fishable") |>
    dplyr::mutate(
      b_fishable = b * p_fishable,
      b_juvenile = b - b_fishable,
      b_female = ifelse(dplyr::coalesce(sex, 0) == local(female_sex), b, 0)
    ) |>
    dplyr::select(
      "sample_id",
      "year",
      "stratum",
      "area",
      "length",
      "sex",
      "maturity_stage",
      "n",
      "b_sample",
      "b",
      "b_fishable",
      "b_juvenile",
      "b_female"
    )
}

#' @param tbl Station values by stratum: a data frame or dplyr query with
#'   ``year``, ``stratum``, ``sample_id`` and the columns of ``values``,
#'   e.g. from [pax_si_catch_weight_biomass()]. Values are summed by
#'   station first. Rows with stratum or area ``NA`` are left out
#' @param values Columns to make indices of
#' @param z Quantile of the 95% intervals
#' @return \subsection{pax_si_stratified_index}{A tibble with one row per
#'   year and index (``index``, the name of the value column): ``value``
#'   (the sum over strata of the mean station value), ``var`` (the sum over
#'   strata of sd² / n, strata with one station adding none), ``cv``,
#'   ``lower`` and ``upper`` (``value × exp(∓ z cv)``), ``n_strata`` and
#'   ``n_stations``}
#' @rdname pax_si_subarea
pax_si_stratified_index <- function(tbl, values, z = 1.96) {
  # NSE variables
  year <- stratum <- sample_id <- value <- index <- m <- s <- k <- NULL
  var <- cv <- area <- NULL

  if (!inherits(tbl, "data.frame")) {
    tbl <- dplyr::collect(tbl)
  }
  if ("area" %in% names(tbl)) {
    tbl <- dplyr::filter(tbl, !is.na(area))
  }
  tbl |>
    dplyr::filter(!is.na(stratum)) |>
    dplyr::group_by(year, stratum, sample_id) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(values), function(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      dplyr::all_of(values),
      names_to = "index",
      values_to = "value"
    ) |>
    dplyr::group_by(index, year, stratum) |>
    dplyr::summarise(
      m = mean(value),
      s = stats::sd(value),
      k = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(index, year) |>
    dplyr::summarise(
      value = sum(m),
      var = sum((s / sqrt(k))^2, na.rm = TRUE),
      n_strata = dplyr::n(),
      n_stations = sum(k),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      cv = sqrt(var) / value,
      lower = value * exp(-z * cv),
      upper = value * exp(z * cv)
    ) |>
    dplyr::select(index, year, value, var, cv, lower, upper, n_strata, n_stations) |>
    dplyr::arrange(index, year)
}

#' @param stock Names of stocks to keep (``"arnarfjordur"``,
#'   ``"isafjardardjup"``, ``"isafjardardjup_february"`` (the February
#'   survey), ``"snaefellsnes"``; ``"offshore"`` with
#'   ``offshore = TRUE``), ``NULL`` for all
#' @param offshore If ``TRUE``, the rectangles of the offshore survey
#'   ([shrimp_offshore_strata]) instead of the inshore sub-areas
#' @return \subsection{pax_shrimp_strata}{The package data [shrimp_strata]
#'   (or [shrimp_offshore_strata]), the rows of ``stock``}
#' @rdname pax_si_subarea
pax_shrimp_strata <- function(stock = NULL, offshore = FALSE) {
  name <- if (isTRUE(offshore)) "shrimp_offshore_strata" else "shrimp_strata"
  env <- new.env(parent = emptyenv())
  utils::data(list = name, package = "pax", envir = env)
  out <- env[[name]]
  if (!is.null(stock)) {
    unknown <- setdiff(stock, out$stock)
    if (length(unknown) > 0) {
      stop("Unknown shrimp stock(s): ", paste(unknown, collapse = ", "))
    }
    out <- out[out$stock %in% stock, ]
  }
  rownames(out) <- NULL
  out
}
