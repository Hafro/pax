#' Create a pax database populated from the MAR database
#'
#' Opens a connection to the Hafro MAR Oracle database and imports all
#' standard pax tables (station, measurement, logbook, landings, sampling,
#' aldist, ldist, lw_coeffs, ocean depth, strata, and strata_stations) into a
#' new pax DuckDB database.
#'
#' @param species Integer vector of species codes to import
#' @param year_start Optional integer, earliest year to include
#' @param year_end Optional integer, latest year to include
#' @param sampling_type Integer vector of sampling type codes to include
#' @param landings_year_start Optional integer, earliest year of landings to
#'   include (default \code{year_start}). The landings go back to 1903, e.g.
#'   for figures of the landings history.
#' @param ices_area_like Character vector of SQL LIKE patterns for filtering
#'   ICES areas, e.g. ``"5a%"``
#' @param strata Character vector of strata names to import, from
#'   [pax_def_strata_list()]
#' @param sampling_gear Gear codes (``mfdb_gear_code``) of the commercial
#'   samples in the ``sampling`` table, ``NULL`` for all, see
#'   [pax_mar_sampling()]. Default bottom trawl, longline and Danish seine
#' @param skip_trips SQL LIKE patterns of trips to leave out of the station
#'   and sampling tables, see [pax_mar_station()]. By default the
#'   stomach-sampling trips ``MAG*`` and ``MO*``; ``NULL`` keeps all
#' @param gridcell_from_position If ``TRUE``, stations without a
#'   rectangle get the gridcell of their position, see [pax_mar_station()]
#' @param mar_opts Named list of additional options passed to
#'   ``mar::connect_mar()``
#' @param dbdir Path to a DuckDB database file, or ``":memory:"`` for an
#'   in-memory database
#' @return A pax DBI connection containing all imported tables
pax_from_mar <- function(
  species,
  year_start = NULL,
  year_end = NULL,
  sampling_type = c(1, 2, 8, 10, 11, 30, 35),
  ices_area_like = "5a%",
  landings_year_start = year_start,
  strata = pax_def_strata_list(),
  sampling_gear = c("BMT", "LLN", "DSE"),
  skip_trips = c("MAG%", "MO%"),
  gridcell_from_position = FALSE,
  mar_opts = list(),
  dbdir = ":memory:"
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  pcon <- pax_connect(dbdir = dbdir)

  # Open a connection to upstream hafro DB
  mar <- do.call(mar::connect_mar, mar_opts)
  on.exit(DBI::dbDisconnect(mar), add = TRUE, after = TRUE)

  import_defs <- list(
    mar,
    species = species,
    year_start = year_start,
    year_end = year_end
  )

  pax_import(pcon, pax_marmap_ocean_depth())
  # Extract required tables, place into pcon
  for (s in strata) {
    pax_import(pcon, pax_def_strata(s))
  }
  pax_import(
    pcon,
    pax_mar_station(
      mar,
      species = species,
      year_start = year_start,
      year_end = year_end,
      sampling_type = sampling_type,
      skip_trips = skip_trips,
      gridcell_from_position = gridcell_from_position
    )
  )
  pax_import(pcon, do.call(pax_mar_measurement, import_defs))
  pax_import(pcon, do.call(pax_mar_logbook, import_defs))
  pax_import(
    pcon,
    pax_mar_landings(
      mar,
      species = import_defs$species,
      ices_area_like = ices_area_like,
      year_start = landings_year_start,
      year_end = import_defs$year_end
    )
  )
  pax_import(
    pcon,
    do.call(
      pax_mar_sampling,
      c(
        import_defs,
        list(mfdb_gear_code = sampling_gear, skip_trips = skip_trips)
      )
    )
  )
  pax_import(pcon, pax_mar_aldist(mar, species = import_defs$species))
  pax_import(pcon, pax_mar_ldist(mar, species = import_defs$species))
  pax_import(
    pcon,
    pax_mar_lw_coeffs(mar, species = import_defs$species)
  )
  pax_import(pcon, pax_mar_quotatransfer(mar, import_defs$species))
  pax_import(pcon, pax_mar_strata_stations(mar))
  return(pcon)
}
