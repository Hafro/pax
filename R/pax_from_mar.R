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
#' @param quota_species Quota species codes (``fteg``) of the quotatransfer
#'   table, see [pax_mar_quotatransfer()]. Default ``species``
#' @param sampling_gear Gear codes (``mfdb_gear_code``) of the commercial
#'   samples in the ``sampling`` table, ``NULL`` for all, see
#'   [pax_mar_sampling()]. Default bottom trawl, longline and Danish seine
#' @param skip_trips SQL LIKE patterns of trips to leave out of the station
#'   and sampling tables, see [pax_mar_station()]. By default the
#'   stomach-sampling trips ``MAG*`` and ``MO*``; ``NULL`` keeps all
#' @param gridcell_from_position If ``TRUE``, stations without a
#'   rectangle get the gridcell of their position, see [pax_mar_station()]
#' @param logbook_species Species codes of the ``logbook`` table (and of
#'   the optional ``logbook_old``), default ``species``. Add other species for
#'   e.g. CPUE of the tows of another species, or the share of the stock's
#'   catch in them
#' @param logbook_year_start Optional integer, earliest year of the
#'   ``logbook`` table (and ``logbook_old``), default ``year_start``. The
#'   compiled logbooks go back to 1969 for some species
#' @param extra_tables Character vector of optional tables to add, none by
#'   default (see [pax_mar_extra]):
#'   \describe{
#'     \item{``"landings_vessel"``}{landings register by vessel, month,
#'       landings gear code, fishing area and fishing year, from
#'       ``landings_year_start``, [pax_mar_landings_vessel()]}
#'     \item{``"vessel"``}{the vessel register, [pax_mar_vessel()]}
#'     \item{``"logbook_release"``}{logbook catch records with their
#'       condition (released fish), [pax_mar_logbook_release()]}
#'     \item{``"research_landings"``}{landings of the research vessels
#'       during research trips, [pax_mar_research_landings()]}
#'     \item{``"catch_disposition"``}{landed catch by disposition,
#'       [pax_mar_catch_disposition()]}
#'     \item{``"landings_old"``}{the old cod landings,
#'       [pax_mar_landings_old()]}
#'     \item{``"station_skipped"``}{the stations of the trips left out by
#'       ``skip_trips`` (the stomach-sampling trips), with the columns of
#'       ``station``, [pax_mar_station()]}
#'     \item{``"sample"``}{all samples with measurements of the species,
#'       all years and trips, [pax_mar_sample()]}
#'     \item{``"logbook_old"``}{the old logbook tables (``afli.afli``) of
#'       ``logbook_species``, [pax_mar_logbook_old()]}
#'   }
#' @param medafli_species Species codes of the old by-catch table
#'   (``afli.medafli``) for the ``logbook_release`` table, e.g. ``2021``
#'   (released halibut)
#' @param foreign_tables Names of the foreign data tables in mar to import
#'   (Greenland and Faroese surveys, catches and samples), see
#'   [pax_foreign_mar_tables()]. None by default
#' @param foreign_files Named list of foreign data files to import, read
#'   from the given paths when the database is built: names are the file
#'   types of [pax_foreign_file()] (e.g. ``list(greenland_logbooks =
#'   "path/to/logbooks_00-25.csv")``), values the paths. None by default
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
  quota_species = species,
  sampling_gear = c("BMT", "LLN", "DSE"),
  skip_trips = c("MAG%", "MO%"),
  gridcell_from_position = FALSE,
  logbook_species = species,
  logbook_year_start = year_start,
  extra_tables = character(0),
  medafli_species = NULL,
  foreign_tables = character(0),
  foreign_files = list(),
  mar_opts = list(),
  dbdir = ":memory:"
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }
  unknown <- setdiff(extra_tables, pax_from_mar_extra_tables())
  if (length(unknown) > 0) {
    stop("Unknown extra_tables: ", paste(unknown, collapse = ", "))
  }
  unknown <- setdiff(foreign_tables, pax_foreign_mar_tables()$name)
  if (length(unknown) > 0) {
    stop("Unknown foreign_tables: ", paste(unknown, collapse = ", "))
  }
  unknown <- setdiff(names(foreign_files), pax_foreign_file_types())
  if (length(unknown) > 0 || (length(foreign_files) > 0 && is.null(names(foreign_files)))) {
    stop("Unknown foreign_files: ", paste(unknown, collapse = ", "))
  }
  # Check the foreign files before the (slow) import from mar
  for (f in foreign_files) {
    if (!all(file.exists(f))) {
      stop("Foreign data file(s) not found: ", paste(f[!file.exists(f)], collapse = ", "))
    }
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
  pax_import(
    pcon,
    pax_mar_logbook(
      mar,
      species = logbook_species,
      year_start = logbook_year_start,
      year_end = year_end
    )
  )
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
  pax_import(
    pcon,
    pax_mar_quotatransfer(mar, import_defs$species, quota_species)
  )
  pax_import(pcon, pax_mar_strata_stations(mar))

  # Optional tables
  if ("landings_vessel" %in% extra_tables) {
    pax_import(
      pcon,
      pax_mar_landings_vessel(
        mar,
        species = species,
        year_start = landings_year_start,
        year_end = year_end
      )
    )
  }
  if ("vessel" %in% extra_tables) {
    pax_import(pcon, pax_mar_vessel(mar))
  }
  if ("logbook_release" %in% extra_tables) {
    pax_import(
      pcon,
      pax_mar_logbook_release(
        mar,
        species = species,
        medafli_species = medafli_species
      )
    )
  }
  if ("research_landings" %in% extra_tables) {
    pax_import(pcon, pax_mar_research_landings(mar, species = species))
  }
  if ("catch_disposition" %in% extra_tables) {
    pax_import(pcon, pax_mar_catch_disposition(mar, species = species))
  }
  if ("landings_old" %in% extra_tables) {
    pax_import(pcon, pax_mar_landings_old(mar))
  }
  if ("station_skipped" %in% extra_tables && length(skip_trips) > 0) {
    pax_import(
      pcon,
      pax_mar_station(
        mar,
        year_start = year_start,
        year_end = year_end,
        sampling_type = sampling_type,
        skip_trips = NULL,
        only_trips = skip_trips,
        gridcell_from_position = gridcell_from_position
      ),
      name = "station_skipped"
    )
  }
  if ("sample" %in% extra_tables) {
    pax_import(pcon, pax_mar_sample(mar, species = species))
  }
  if ("logbook_old" %in% extra_tables) {
    pax_import(
      pcon,
      pax_mar_logbook_old(
        mar,
        species = logbook_species,
        year_start = logbook_year_start,
        year_end = year_end
      )
    )
  }

  # Foreign data
  for (t in foreign_tables) {
    pax_import(pcon, pax_mar_foreign(mar, t))
  }
  for (type in names(foreign_files)) {
    pax_import(pcon, pax_foreign_file(foreign_files[[type]], type))
  }
  return(pcon)
}

#' @return \subsection{pax_from_mar_extra_tables}{The names of the optional
#'   tables of [pax_from_mar()] (argument ``extra_tables``)}
#' @rdname pax_from_mar
pax_from_mar_extra_tables <- function() {
  c(
    "landings_vessel",
    "vessel",
    "logbook_release",
    "research_landings",
    "catch_disposition",
    "landings_old",
    "station_skipped",
    "sample",
    "logbook_old"
  )
}
