# https://h3geo.org/docs/api/indexing/
# https://github.com/isaacbrodsky/h3-duckdb
# https://duckdb.org/docs/stable/core_extensions/spatial/functions

pax_connect <- function(
  dbdir = ":memory:",
  read_only = FALSE,
  h3_resolution = 6
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

pax_import <- function(
  pcon,
  tbl,
  overwrite = FALSE,
  name = attr(tbl, "pax_name"),
  cite = attr(tbl, "pax_cite")
) {
  tbl_colnames <- colnames(tbl)

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
        " SET h3_cells = list_distinct(flatten(list_transform(",
        "   ST_Dump(geom),",
        "   lambda x: h3_polygon_wkt_to_cells(x.geom, ",
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

pax_temptbl <- function(pcon, tbl, force_tbl = FALSE) {
  # If it's already a DB table, don't do anything. Let dplyr::join worry if the source matches
  if (inherits(tbl, "tbl_sql")) {
    return(tbl)
  }

  # If a character scalar, assume it's a table name
  if (is.character(tbl) && length(tbl) == 1) {
    return(dplyr::tbl(pcon, tbl))
  }

  if (!isTRUE(force_tbl) && is.data.frame(tbl) && nrow(tbl) < 20) {
    return(dbplyr::copy_inline(pcon, tbl))
  }

  if (is.data.frame(tbl)) {
    return(dplyr::copy_to(
      pcon,
      tbl,
      # TODO: Use digest to avoid filling session with copies of the same table
      name = basename(tempfile(pattern = "paxtemp_")),
      temporary = TRUE
    ))
  }

  stop("Unknown table format, can't translate to DB table: ", substitute(tbl))
}
