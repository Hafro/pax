pax_connect <- function(dbdir = ":memory:") {
  pcon <- DBI::dbConnect(duckdb::duckdb(), dbdir)

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

  return(pcon)
}

pax_import <- function(
  pcon,
  tbl,
  overwrite = FALSE,
  name = attr(tbl, "pax_name"),
  cite = attr(tbl, "pax_cite")
) {
  tbl_colnames <- pax_tbl_colnames(tbl)

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
    geom_data <- sf::st_as_binary(sf::st_geometry(tbl), EWKB = TRUE)
    geom_crs <- sf::st_crs(tbl, parameters = TRUE)
    tbl <- as.data.frame(sf::st_drop_geometry(tbl))
    # NB: Force geometry column to be called "geom", since this is what duckdbfs assumes
    tbl[["geom"]] <- geom_data
    field.types["geom"] <- "BLOB"
    tbl_colnames <- colnames(head(tbl, 0))
  } else {
    geom_crs <- NULL
  }

  if (!startsWith(name, "paxdat_")) {
    if (is.null(geom_crs) && "geom" %in% tbl_colnames) {
      # Force geom into known binary format, let sf complain if it can't
      tbl[["geom"]] <- sf::st_as_binary(tbl[["geom"]])
      geom_crs <- attr(tbl, "crs") # TODO: Made up
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
    # Set geometry data types
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
    # TODO: https://github.com/Cidree/duckspatial/issues/7
    DBI::dbExecute(
      pcon,
      dbplyr::build_sql(
        "ALTER TABLE ",
        dbplyr::ident(name),
        " ADD COLUMN crs_duckspatial VARCHAR DEFAULT ",
        geom_crs$srid,
        ";",
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
        "UPDATE ",
        dbplyr::ident(name),
        " SET geom = ST_Point(lon, lat)",
        " WHERE lon IS NOT NULL AND lat IS NOT NULL",
        con = pcon
      )
    )
  }

  if (!startsWith(name, "paxdat_")) {
    # Populate ancillary tables
    # TODO: Eventually these will hang of the schema definition
    if ("gridcell" %in% tbl_colnames) {
      pax_dat_gridcell(
        pcon,
        dplyr::tbl(pcon, name) |>
          dplyr::distinct(gridcell) |>
          dplyr::pull(gridcell)
      )
    }
    if (all(c("lat", "lon") %in% tbl_colnames)) {
      # TODO: Separately to gridcell, fetch noaa depth for maps?
    }
    if ("sampling_type" %in% tbl_colnames) {
      pax_dat_sampling_type_desc(pcon)
    }
    if ("mfdb_gear_code" %in% tbl_colnames) {
      pax_dat_mfdb_gear_code_desc(pcon)
    }
  }

  if (!is.null(cite)) {
    # TODO: Copy citation to citation table
  }

  invisible(NULL)
}

pax_decorate <- function(tbl, cite = NULL, name = NULL) {
  if (!is.null(cite)) {
    attr(tbl, "pax_cite") <- cite
  }
  if (!is.null(name)) {
    attr(tbl, "pax_name") <- name
  }
  return(tbl)
}

pax_temptbl <- function(pcon, tbl) {
  # If it's already a table, don't do anything. Let dplyr::join worry if the source matches
  if (dplyr::is.tbl(tbl)) {
    return(tbl)
  }

  # If a character scalar, assume it's a table name
  if (is.character(tbl) && length(tbl) == 1) {
    return(dplyr::tbl(pcon, tbl))
  }

  # TODO: If lots of rows, store as temp tbl first and return that
  dbplyr::copy_inline(pcon, tbl)
}

pax_tbl_colnames <- function(tbl) {
  tbl |> dplyr::filter(0 == 1) |> colnames()
}
