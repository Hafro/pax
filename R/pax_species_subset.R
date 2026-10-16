#' One species of a pax database with several species
#'
#' Most functions of pax and hafroreports take all rows of the ``ldist``,
#' ``aldist`` and ``measurement`` tables, as if the database held one
#' species. [pax_species_subset()] gives a connection that shows one
#' species (or several) of a database built for many, e.g. by
#' [pax_from_mar()] with all the species of a survey: a new in-memory pax
#' connection with the database file attached read-only and a view of every
#' table, the tables with a ``species`` column filtered to ``species``.
#' Nothing is copied.
#'
#' @param pcon A pax connection to a DuckDB database file (not
#'   ``":memory:"``), e.g. the ``pax_db`` target read with
#'   [pax_tar_format_duckdb()].
#' @param species Integer vector of species codes to keep.
#' @param tables Tables to filter by ``species`` (those that have the
#'   column). Default the per-species tables of [pax_from_mar()]; the
#'   ``survey_catch``, ``stomach`` and ``taxonomy`` tables keep all species.
#' @return A pax DBI connection. Disconnect it with
#'   ``DBI::dbDisconnect()`` when done.
#' @examples
#' \dontrun{
#' pcon <- pax_connect("pax.duckdb", read_only = TRUE)
#' cod <- pax_species_subset(pcon, 1)
#' dplyr::tbl(cod, "ldist") |> dplyr::distinct(species)
#' }
pax_species_subset <- function(
  pcon,
  species,
  tables = c(
    "ldist",
    "aldist",
    "measurement",
    "lw_coeffs",
    "landings",
    "logbook",
    "sampling",
    "quotatransfer"
  )
) {
  dbdir <- pcon@driver@dbdir
  if (!nzchar(dbdir) || identical(dbdir, ":memory:")) {
    stop("pax_species_subset() needs a pax database in a file")
  }

  out <- pax_connect()
  DBI::dbExecute(
    out,
    dbplyr::build_sql("ATTACH ", dbdir, " AS pax_src (READ_ONLY)", con = out)
  )
  src_tables <- DBI::dbGetQuery(
    out,
    "SELECT table_name, column_name FROM information_schema.columns
      WHERE table_catalog = 'pax_src' AND table_schema = 'main'"
  )
  sp <- paste(as.numeric(species), collapse = ", ")
  # pax_connect() makes its own settings tables (h3_resolution)
  own <- DBI::dbGetQuery(
    out,
    "SELECT table_name FROM information_schema.tables
      WHERE table_catalog = current_database()"
  )$table_name
  for (t in setdiff(unique(src_tables$table_name), own)) {
    cols <- src_tables$column_name[src_tables$table_name == t]
    qt <- DBI::dbQuoteIdentifier(out, t)
    where <- if (t %in% tables && "species" %in% cols) {
      paste0(" WHERE species IN (", sp, ")")
    } else {
      ""
    }
    DBI::dbExecute(
      out,
      paste0("CREATE VIEW ", qt, " AS SELECT * FROM pax_src.main.", qt, where)
    )
  }
  out
}
