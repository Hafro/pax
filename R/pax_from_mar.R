#' Create a pax database populated from the MAR database
#'
#' Opens a connection to the Hafro MAR Oracle database and imports all
#' standard pax tables (station, measurement, logbook, landings, sampling,
#' aldist, ldist, lw_coeffs, ocean depth, and strata) into a new pax DuckDB
#' database.
#'
#' @param species Integer vector of species codes to import
#' @param year_start Optional integer, earliest year to include
#' @param year_end Optional integer, latest year to include
#' @param sampling_type Integer vector of sampling type codes to include
#' @param ices_area_like Character vector of SQL LIKE patterns for filtering
#'   ICES areas, e.g. ``"5a%"``
#' @param strata Character vector of strata names to import, from
#'   [pax_def_strata_list()]
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
  strata = pax_def_strata_list(),
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
      sampling_type = sampling_type
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
      year_start = import_defs$year_start,
      year_end = import_defs$year_end
    )
  )
  pax_import(pcon, do.call(pax_mar_sampling, import_defs))
  pax_import(pcon, pax_mar_aldist(mar, species = import_defs$species))
  pax_import(pcon, pax_mar_ldist(mar, species = import_defs$species))
  pax_import(
    pcon,
    pax_mar_lw_coeffs(mar, species = import_defs$species)
  )
  return(pcon)
}
