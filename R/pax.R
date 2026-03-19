# https://h3geo.org/docs/api/indexing/
# https://github.com/isaacbrodsky/h3-duckdb
# https://duckdb.org/docs/stable/core_extensions/spatial/functions

#' Connect to a pax DuckDB database
#'
#' Create or open a DuckDB database for use with pax, installing and loading
#' the required ``spatial`` and ``h3`` extensions as needed.
#'
#' @param dbdir Path to a DuckDB database file, or ``":memory:"`` for an in-memory database
#' @param read_only Boolean, open the database in read-only mode?
#' @param h3_resolution H3 cell resolution to use for spatial indexing,
#'   see \url{https://h3geo.org/docs/core-library/restable}
#' @return A DBI database connection object
pax_connect <- function(
  dbdir = ":memory:",
  read_only = FALSE,
  h3_resolution = 8
) {
  pcon <- DBI::dbConnect(duckdb::duckdb(), dbdir, read_only = read_only)

  # Install required extensions
  extensions <- DBI::dbGetQuery(
    pcon,
    "
    SELECT wanted.extension_name
         , wanted.source
         , duckdb_extensions.loaded
         , duckdb_extensions.installed
      FROM (
           VALUES ('spatial', ''), ('h3', 'community')
           ) AS wanted(extension_name, source)
      LEFT JOIN duckdb_extensions() ON wanted.extension_name = duckdb_extensions.extension_name;
  "
  )
  for (i in seq_len(nrow(extensions))) {
    if (!isTRUE(extensions[i, "installed"])) {
      DBI::dbExecute(
        pcon,
        dbplyr::build_sql(
          "INSTALL ",
          dplyr::ident(extensions[i, "extension_name"]),
          dplyr::sql(if (nzchar(extensions[i, "source"])) " FROM " else ""),
          dplyr::sql(extensions[i, "source"]),
          ";",
          con = pcon
        )
      )
    } else {
      # NB: DuckDB is coercing NA on the way out, thus suppresWarnings
      suppressWarnings(DBI::dbGetQuery(
        pcon,
        paste0("UPDATE EXTENSIONS (", extensions[i, "extension_name"], ");")
      ))
    }
    if (!isTRUE(extensions[i, "loaded"])) {
      DBI::dbExecute(
        pcon,
        dbplyr::build_sql(
          "LOAD ",
          extensions[i, "extension_name"],
          con = pcon
        )
      )
    }
  }

  # TODO: Pre-define common schema items? If per-package, where do they hang?

  # Default h3 resolution for actions
  # https://h3geo.org/docs/core-library/restable
  if (!DBI::dbExistsTable(pcon, "h3_resolution")) {
    DBI::dbWriteTable(
      pcon,
      "h3_resolution",
      data.frame(res = as.integer(h3_resolution)),
      overwrite = TRUE
    )
  }

  return(pcon)
}

#' List contents of a pax database
#'
#' Returns a dplyr query of all tables that have been imported into the
#' database with [pax_import()].
#'
#' @param pcon A pax DBI connection, as returned by [pax_connect()]
#' @return A dplyr query of the ``pax_citation`` table, with columns
#'   ``tbl_name`` and ``citation``
pax_contents <- function(pcon) {
  dplyr::tbl(pcon, "pax_citation")
}

#' Import a table into a pax database
#'
#' Import a data.frame, sf spatial object, or CSV file into a pax DuckDB
#' database. Geometry and H3 spatial index columns are added automatically
#' when the data contains a geometry column, ``lat``/``lon`` columns, or
#' ``begin_lat``/``begin_lon``/``end_lat``/``end_lon`` columns.
#'
#' @param pcon A pax DBI connection, as returned by [pax_connect()]
#' @param tbl Data to import: a data.frame, sf object, dplyr query, or path
#'   to a CSV file
#' @param overwrite Boolean, overwrite an existing table with the same name?
#' @param name Name to use for the imported table. Defaults to the variable
#'   name of ``tbl``, or the ``pax_name`` attribute set by [pax_decorate()]
#' @param cite Citation string for the data source. Defaults to the
#'   ``pax_cite`` attribute set by [pax_decorate()]
#' @return Invisibly returns ``NULL``
pax_import <- function(
  pcon,
  tbl,
  overwrite = FALSE,
  name = attr(tbl, "pax_name"),
  cite = attr(tbl, "pax_cite")
) {
  tbl_colnames <- colnames(tbl)

  # Try copying name from incoming table variable
  if (is.null(name)) {
    name <- substitute(tbl)
    if (is.symbol(name)) {
      name <- deparse1(name)
    } else {
      stop(
        "No table name supplied, and tbl wasn't a variable, so can't copy it's name"
      )
    }
  }

  # If handed a file name, read it in first
  if (is.character(tbl) && file.exists(tbl)) {
    tbl <- utils::read.csv(tbl)
  }

  if (DBI::dbExistsTable(pcon, name)) {
    if (!isTRUE(overwrite)) {
      stop("A table ", name, " already exists")
    }
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "DROP TABLE ",
        dbplyr::ident(name),
        con = pcon
      )
    )
  }

  field.types <- c()

  if (inherits(tbl, "sf")) {
    # Convert to data.frame, with geometry column in right place
    # https://github.com/Cidree/duckspatial/blob/b6e6bc842b1494d1cd8bfb4f73b6c8609b6ebddc/R/db_write.R#L77-L83
    # Make sure we're using the same CRS, which also needs to match H3
    tbl <- sf::st_transform(tbl, crs = pax_def_crs())
    geom_data <- sf::st_as_binary(sf::st_geometry(tbl), EWKB = TRUE)
    stopifnot(sf::st_crs(tbl, parameters = TRUE)$srid == pax_def_crs()$srid)
    geom_types <- sf::st_geometry_type(tbl)
    tbl <- as.data.frame(sf::st_drop_geometry(tbl))
    # NB: Force geometry column to be called "geom"
    tbl[["geom"]] <- geom_data
    field.types["geom"] <- "BLOB"
    tbl_colnames <- colnames(head(tbl, 0))
  } else {
    geom_types <- NULL
  }

  if (!startsWith(name, "paxdat_")) {
    if (
      is.null(geom_types) && "geom" %in% tbl_colnames && "crs" %in% tbl_colnames
    ) {
      # Force geom into known binary format, let sf complain if it can't
      tbl[["geom"]] <- sf::st_as_binary(tbl[["geom"]])
      geom_types <- sf::st_geometry_type(tbl[["geom"]])
      stopifnot(length(unique(tbl[["crs"]])) != 1)
      stopifnot(
        sf::st_crs(as.character(tbl["crs", 1]), parameters = TRUE)$srid ==
          pax_def_crs()$srid
      )
    }
    # TODO: Check schema
    # TODO: Populate lookup tables
    # TODO: unique_indexes / indexes arguments
  }

  DBI::dbWriteTable(
    pcon,
    name,
    tbl |> dplyr::collect(),
    field.types = field.types
  )

  if ("geom" %in% tbl_colnames) {
    # Give geom column the correct data type post-import
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "ALTER TABLE ",
        dbplyr::ident(name),
        " ALTER COLUMN geom",
        " SET DATA TYPE GEOMETRY USING ST_GeomFromWKB(geom);",
        con = pcon
      )
    )
  }

  if (!is.null(geom_types) && all(geom_types == "MULTIPOLYGON")) {
    # geometry columns hold multi-polygons, add h3_cells

    # NB: sub-queries not allowed in lambda expressions, so fetch first
    h3_resolution <- DBI::dbGetQuery(pcon, "SELECT res FROM h3_resolution;")[
      1,
      1
    ]
    # https://h3geo.org/docs/api/regions#polygontocells
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "ALTER TABLE ",
        dbplyr::ident(name),
        " ADD COLUMN h3_cells UBIGINT[] DEFAULT NULL",
        ";",
        con = pcon
      )
    )
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "UPDATE ",
        dbplyr::ident(name),
        # NB: h3_polygon_wkt_to_cells doesn't support MULTIPOLYGON, so we have to dump as a list of POLYGONs and then re-combine
        #     https://github.com/isaacbrodsky/h3-duckdb/issues/175
        # NB: It also doesn't support wkb, thus ST_AsText()
        #     https://github.com/isaacbrodsky/h3-duckdb/issues/178
        " SET h3_cells = list_distinct(flatten(list_transform(",
        "   ST_Dump(geom),",
        "   lambda x: h3_polygon_wkt_to_cells(ST_AsText(x.geom), ",
        h3_resolution,
        ")",
        ")));",
        con = pcon
      )
    )
  } else if (all(c("lat", "lon") %in% tbl_colnames)) {
    # No geometry, but lat/lon columns. Interpret these as ST_points
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "ALTER TABLE ",
        dbplyr::ident(name),
        " ADD COLUMN geom GEOMETRY DEFAULT NULL;",
        con = pcon
      )
    )
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "ALTER TABLE ",
        dbplyr::ident(name),
        " ADD COLUMN h3_cells UBIGINT[] DEFAULT NULL",
        ";",
        con = pcon
      )
    )
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "UPDATE ",
        dbplyr::ident(name),
        # NB: h3 assumes WGS84/EPSG:4326
        " SET geom = ST_Point(lon, lat), h3_cells = [h3_latlng_to_cell(lat, lon, (SELECT res FROM h3_resolution))]",
        " WHERE lon IS NOT NULL AND lat IS NOT NULL",
        con = pcon
      )
    )
  } else if (
    all(c("begin_lat", "begin_lon", "end_lat", "end_lon") %in% tbl_colnames)
  ) {
    # No geometry, but begin/end lat/lon column. Interpret as ST_lineString
    # TODO: Why aren't we setting a CRS? Where should that get assumed? Convert them?
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "ALTER TABLE ",
        dbplyr::ident(name),
        " ADD COLUMN geom GEOMETRY DEFAULT NULL;",
        con = pcon
      )
    )
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "ALTER TABLE ",
        dbplyr::ident(name),
        " ADD COLUMN h3_cells UBIGINT[] DEFAULT NULL;",
        con = pcon
      )
    )
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "UPDATE ",
        dbplyr::ident(name),
        # Both points, map a line from one to other
        # NB: h3 assumes WGS84/EPSG:4326
        " SET geom = ST_MakeLine([ST_Point(begin_lon, begin_lat), ST_Point(end_lon, end_lat)])",
        " , h3_cells = h3_grid_path_cells(",
        "     h3_latlng_to_cell(begin_lat, begin_lon, (SELECT res FROM h3_resolution)),",
        "     h3_latlng_to_cell(end_lat, end_lon, (SELECT res FROM h3_resolution))",
        " )",
        " WHERE begin_lon IS NOT NULL AND begin_lat IS NOT NULL AND end_lon IS NOT NULL AND end_lat IS NOT NULL",
        con = pcon
      )
    )
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "UPDATE ",
        dbplyr::ident(name),
        # If end-point missing, make a point from start
        # NB: h3 assumes WGS84/EPSG:4326
        " SET geom = ST_Point(begin_lon, begin_lat)",
        " , h3_cells = [h3_latlng_to_cell(begin_lat, begin_lon, (SELECT res FROM h3_resolution))]",
        " WHERE begin_lon IS NOT NULL AND begin_lat IS NOT NULL AND end_lon IS NULL AND end_lat IS NULL",
        con = pcon
      )
    )
  }

  if (!DBI::dbExistsTable(pcon, "pax_citation")) {
    DBI::dbExecute(
      pcon,
      "CREATE TABLE pax_citation (tbl_name VARCHAR PRIMARY KEY, citation VARCHAR)"
    )
  }
  DBI::dbExecute(
    pcon,
    dbplyr::build_sql(
      "INSERT OR REPLACE INTO pax_citation VALUES (",
      name,
      ", ",
      ifelse(is.null(cite), NA, cite),
      ")",
      con = pcon
    )
  )

  invisible(NULL)
}

#' Attach metadata to a table for use with pax_import
#'
#' Add citation and/or name attributes to a data.frame or dplyr query.
#' These attributes are used as defaults by [pax_import()].
#'
#' @param tbl A data.frame or dplyr query to decorate
#' @param cite Citation string for the data source. Defaults to the calling
#'   expression
#' @param name Table name to use when importing, or ``NULL`` to leave unset
#' @return ``tbl`` with ``pax_cite`` and/or ``pax_name`` attributes attached
pax_decorate <- function(tbl, cite = deparse1(sys.call(-1)), name = NULL) {
  if (!is.null(cite)) {
    attr(tbl, "pax_cite") <- cite
  }
  if (!is.null(name)) {
    attr(tbl, "pax_name") <- name
  }
  return(tbl)
}

#' Make a table available as a DuckDB query
#'
#' Converts an R data.frame or string table reference into a dplyr SQL query
#' against the pax database. If the input is already a database query, it is
#' returned unchanged. String names beginning with ``paxdat_`` refer to
#' package-internal datasets which are loaded on demand.
#'
#' @param pcon A pax DBI connection, as returned by [pax_connect()]
#' @param tbl A data.frame, a table name string, or an existing dplyr SQL query
#' @return A dplyr SQL query referencing the table within ``pcon``
pax_temptbl <- function(pcon, tbl) {
  # If it's already a DB table, don't do anything. Let dplyr::join worry if the source matches
  if (inherits(tbl, "tbl_sql")) {
    return(tbl)
  }

  if (is.character(tbl) && length(tbl) == 1) {
    if (isTRUE(startsWith(tbl, "paxdat_"))) {
      # Reference to pax package data, load & attach in-memory copy
      if (!DBI::dbExistsTable(pcon, tbl)) {
        name <- gsub("^paxdat_", "", tbl)
        env <- new.env(parent = emptyenv())
        utils::data(list = name, package = "pax", envir = env)
        data_df <- env[[name]]

        duckdb::duckdb_register(pcon, tbl, data_df)
      }
      return(dplyr::tbl(pcon, tbl))
    }

    if (DBI::dbExistsTable(pcon, tbl)) {
      return(dplyr::tbl(pcon, tbl))
    }

    stop("Table not available in pax DB: ", tbl)
  }

  if (is.data.frame(tbl)) {
    # Register data.frame as in-memory copy of table
    tbl_name <- paste0("temptbl_", digest::digest(tbl, algo = "xxh3_64"))
    duckdb::duckdb_register(pcon, tbl_name, tbl, overwrite = TRUE)
    return(dplyr::tbl(pcon, tbl_name))
  }

  stop("Unknown table format, can't translate to DB table: ", substitute(tbl))
}
