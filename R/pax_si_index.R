#' Survey indices by length range from a fixed station list
#'
#' The survey indices of the category 3 and SPiCT stocks, computed as the old
#' tidypax `si_by_length() |> si_add_strata() |> si_by_strata() |>
#' si_by_year()`: stations get their stratum from a fixed station list (the
#' `strata_stations` table of [pax_from_mar()]), each survey has its own
#' stratum areas, and the index of a length range counts the fish strictly
#' within it (exclusive ends, as [pax_si_strata_summary()]).
#'
#' - [pax_si_strata_stations()] reads the station list of one stratification,
#'   with the stratum areas of each survey;
#' - [pax_si_scale_by_strata_stations()] scales station values to strata with
#'   that list (the areas come with the list);
#' - [pax_si_by_strata()] selects the survey stations (sampling type, tows,
#'   gears, years, fixed stations) and scales them to strata;
#' - [pax_si_length_index()] gives the biomass and abundance index with CVs
#'   by year for each length range.
#'
#' @param pcon A pax database connection.
#' @param stratification The ``stratification`` of the ``strata_stations``
#'   table. Default ``"new_strata"``.
#' @param area_tables Named character vector: for each sampling type (the
#'   names), the strata table with its stratum areas (``rall_area``, km²).
#'   Default the new strata of the spring (30) and autumn (35) surveys.
#' @return \subsection{pax_si_strata_stations}{A tibble with columns
#'   ``sampling_type``, ``station``, ``stratum`` and ``area`` (the stratum
#'   area of that survey in square nautical miles, 0 if the strata table has
#'   no area for the stratum)}
#' @examples
#' \dontrun{
#' pcon <- pax_connect("pax.duckdb")
#' strata <- pax_si_strata_stations(pcon)
#' pax_si_length_index(
#'   pcon,
#'   strata,
#'   length_ranges = list(total = c(1, 500), harv = c(30, 500)),
#'   sampling_type = 35,
#'   tow_number = 0:75,
#'   gear_id = 77:78,
#'   skip_years = 2011
#' )
#' }
#' @name pax_si_index
pax_si_strata_stations <- function(
  pcon,
  stratification = "new_strata",
  area_tables = c(`30` = "new_strata_spring", `35` = "new_strata_autumn")
) {
  # NSE variables
  stratum <- rall_area <- sampling_type <- station <- area <- NULL

  st_strat <- stratification
  areas <- lapply(area_tables, function(t) {
    dplyr::tbl(pcon, t) |>
      dplyr::select(stratum, rall_area) |>
      dplyr::collect()
  }) |>
    dplyr::bind_rows(.id = "sampling_type") |>
    dplyr::mutate(sampling_type = as.numeric(sampling_type)) |>
    dplyr::distinct(sampling_type, stratum, rall_area)

  dplyr::tbl(pcon, "strata_stations") |>
    dplyr::filter(stratification == local(st_strat)) |>
    dplyr::collect() |>
    dplyr::left_join(areas, by = c("sampling_type", "stratum")) |>
    # km^2 to square nautical miles (tow area units)
    dplyr::mutate(area = dplyr::coalesce(rall_area, 0) / 1.852^2) |>
    dplyr::select(sampling_type, station, stratum, area)
}

#' @param tbl A dplyr query of station values, from [pax_si_by_length()].
#' @param strata_stations A data frame with columns ``station``, ``stratum``
#'   and ``area`` (square nautical miles), as [pax_si_strata_stations()].
#'   [pax_si_scale_by_strata_stations()] uses all its rows, so give it the
#'   rows of one survey; the other functions keep the rows of
#'   ``sampling_type`` if it has that column.
#' @return \subsection{pax_si_scale_by_strata_stations}{A dplyr query as
#'   [pax_si_scale_by_strata()]: ``si_abund`` and ``si_biomass`` multiplied by
#'   the stratum area and divided by the number of stations of the stratum in
#'   the year. Stations not on the list get no stratum and area 0}
#' @rdname pax_si_index
pax_si_scale_by_strata_stations <- function(tbl, strata_stations) {
  # NSE variables
  sample_id <- station <- gridcell <- species <- year <- length <- NULL
  tow_depth <- stratum <- sampling_type <- area <- NULL
  si_abund <- si_biomass <- NULL

  pcon <- dbplyr::remote_con(tbl)
  stations_area <- dplyr::distinct(strata_stations, station, stratum, area)

  tbl |>
    dplyr::left_join(
      pax_temptbl(pcon, stations_area),
      by = "station"
    ) |>
    dplyr::mutate(area = dplyr::coalesce(area, 0)) |>
    dplyr::group_by(
      sample_id,
      station,
      gridcell,
      species,
      year,
      length,
      tow_depth,
      stratum,
      sampling_type,
      area
    ) |>
    dplyr::summarize(
      si_abund = sum(si_abund, na.rm = TRUE),
      si_biomass = sum(si_biomass, na.rm = TRUE)
    ) |>
    dplyr::group_by(species, year, stratum, sampling_type, area) |>
    dplyr::mutate(
      # NB: Not summarise, i.e. window function
      si_abund = area * si_abund / dplyr::n_distinct(sample_id, na.rm = TRUE),
      si_biomass = area *
        si_biomass /
        dplyr::n_distinct(sample_id, na.rm = TRUE)
    )
}

#' @param station_tbl A pax database connection (its ``station`` table is
#'   used), or a dplyr query of the station table, e.g. with columns added.
#' @param sampling_type The survey (one sampling type), e.g. 30 (spring
#'   survey) or 35 (autumn survey).
#' @param tow_number Tow numbers to keep; stations without a tow number count
#'   as tow 0, as tidypax. ``NULL`` keeps all tows.
#' @param gear_id Gears to keep, or ``NULL`` for all.
#' @param skip_years Years to leave out, e.g. 2011 for the autumn survey
#'   (only part of the area was covered).
#' @param fixed_only If ``TRUE``, only the fixed stations (``fixed == 1``).
#' @return \subsection{pax_si_by_strata}{A dplyr query of station values by
#'   length scaled to strata, as [pax_si_scale_by_strata_stations()]}
#' @rdname pax_si_index
pax_si_by_strata <- function(
  station_tbl,
  strata_stations,
  sampling_type,
  tow_number = NULL,
  gear_id = NULL,
  skip_years = NULL,
  fixed_only = FALSE
) {
  si_station_select(
    station_tbl,
    sampling_type,
    tow_number = tow_number,
    gear_id = gear_id,
    skip_years = skip_years,
    fixed_only = fixed_only
  ) |>
    si_by_strata_stations(strata_stations, sampling_type)
}

#' @param length_ranges Named list of length ranges, each ``c(lower,
#'   upper)``; an index counts the fish strictly within the range (``c(30,
#'   500)``: 31 cm and over in 1 cm classes).
#' @param complete_years If ``TRUE``, a year with survey stations but no fish
#'   in a length range gets an index of 0 (CVs ``NA``); otherwise it has no
#'   row (the default, as tidypax). The rfb rule takes the last five rows of
#'   the index, so a missing year shifts index A and B.
#' @return \subsection{pax_si_length_index}{A tibble with columns
#'   ``length_range`` (the names of ``length_ranges``), ``year``, ``srN``
#'   (strata with fish), ``n`` (abundance, thousands), ``n_cv``, ``b``
#'   (biomass, tonnes) and ``b_cv``, ordered by length range and year}
#' @rdname pax_si_index
pax_si_length_index <- function(
  station_tbl,
  strata_stations,
  length_ranges,
  sampling_type,
  tow_number = NULL,
  gear_id = NULL,
  skip_years = NULL,
  fixed_only = FALSE,
  complete_years = FALSE
) {
  # NSE variables
  year <- length_range <- si_N <- si_abund <- si_abund_cv <- NULL
  si_biomass <- si_biomass_cv <- NULL

  if (is.null(names(length_ranges)) || any(names(length_ranges) == "")) {
    stop("length_ranges must be a named list")
  }
  st <- si_station_select(
    station_tbl,
    sampling_type,
    tow_number = tow_number,
    gear_id = gear_id,
    skip_years = skip_years,
    fixed_only = fixed_only
  )
  by_strata <- si_by_strata_stations(st, strata_stations, sampling_type)

  if (isTRUE(complete_years)) {
    # Years with survey stations. pax_si_year_summary() leaves out the strata
    # without fish, which doesn't change the index, but a year without fish
    # in the length range would get no index at all
    survey_years <- st |>
      dplyr::distinct(year) |>
      dplyr::collect() |>
      dplyr::pull(year)
  }

  purrr::imap(length_ranges, function(lr, name) {
    out <- by_strata |>
      pax_si_strata_summary(length_range = lr) |>
      pax_si_year_summary() |>
      dplyr::collect() |>
      dplyr::ungroup() |>
      dplyr::transmute(
        length_range = name,
        year,
        srN = si_N,
        n = si_abund,
        n_cv = si_abund_cv,
        b = si_biomass,
        b_cv = si_biomass_cv
      )
    if (isTRUE(complete_years)) {
      out <- out |>
        tidyr::complete(
          length_range = name,
          year = survey_years,
          fill = list(srN = 0, n = 0, b = 0)
        )
    }
    out
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(length_range, year)
}

# The survey stations of one sampling type, with the tow, gear, year and
# fixed-station filters
si_station_select <- function(
  station_tbl,
  sampling_type,
  tow_number = NULL,
  gear_id = NULL,
  skip_years = NULL,
  fixed_only = FALSE
) {
  # NSE variables
  year <- fixed <- NULL

  if (inherits(station_tbl, "DBIConnection")) {
    station_tbl <- dplyr::tbl(station_tbl, "station")
  }
  st_type <- sampling_type
  tows <- tow_number
  gears <- gear_id
  if (is.null(tows)) {
    st <- station_tbl |>
      dplyr::filter(sampling_type == local(st_type))
  } else {
    st <- station_tbl |>
      dplyr::filter(
        sampling_type == local(st_type),
        # Stations without a tow number count as tow 0, as tidypax nvl()
        dplyr::coalesce(tow_number, 0) %in% local(tows)
      )
  }
  if (length(skip_years) > 0) {
    st <- st |> dplyr::filter(!(year %in% local(skip_years)))
  }
  if (!is.null(gears)) {
    st <- st |> dplyr::filter(gear_id %in% local(gears))
  }
  if (isTRUE(fixed_only)) {
    st <- st |> dplyr::filter(fixed == 1)
  }
  st
}

# Station values by length, scaled to strata with the station list of the
# sampling type
si_by_strata_stations <- function(st, strata_stations, sampling_type) {
  st_type <- sampling_type
  ss <- strata_stations
  if (!all(c("station", "stratum", "area") %in% colnames(ss))) {
    stop("strata_stations needs columns station, stratum and area")
  }
  if ("sampling_type" %in% colnames(ss)) {
    ss <- ss[ss$sampling_type == st_type, ] |>
      dplyr::select(-dplyr::all_of("sampling_type"))
  }
  st |>
    pax_si_by_length() |>
    pax_si_scale_by_strata_stations(ss)
}
