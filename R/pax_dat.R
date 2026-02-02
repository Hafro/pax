# TODO: Common internal designator?
intpax_dat <- function(pcon, name, df) {
  tbl_name <- paste0("paxdat_", name)

  if (!(tbl_name %in% DBI::dbListTables(pcon))) {
    env <- new.env(parent = emptyenv())
    data(list = name, package = "pax", envir = env)

    pax_import(pcon, env[[name]], name = tbl_name, cite = citation("pax"))
  }

  return(dplyr::tbl(pcon, tbl_name))
}

# Was: ops$bthe."reitmapping"
pax_dat_gridcell <- function(pcon) {
  dat_from_pkg(pcon, "gridcell")
}

refresh_gridcell <- function(mar) {
  mar::tbl_mar(mar, 'ops$bthe."reitmapping"') |>
    dplyr::filter(
      !is.na(gridcell),
      !is.na(division),
      !is.na(subdivision),
      !is.na(lat),
      !is.na(lon)
    ) |>
    dplyr::select(-id) |>
    write.table(file = "pax/data/gridcell.txt")
}

pax_dat_ocean_depth <- function(pcon) {
  if (DBI::dbExistsTable(pcon, "paxdat_ocean_depth")) {
    return(dplyr::tbl(pcon, "paxdat_ocean_depth"))
  }

  gridcell_bounds <-
    pax_dat_gridcell(pcon) |>
    dplyr::summarize(
      lat1 = min(lat, na.rm = TRUE),
      lon1 = min(lon, na.rm = TRUE),
      lat2 = max(lat, na.rm = TRUE),
      lon2 = max(lon, na.rm = TRUE)
    ) |>
    as.data.frame() |>
    as.list()

  # Fetch unaggregated data first, save to a temporary table
  # Do this to avoid bringing in an R-native H3 library
  raw_ocean_depth_tbl_name <-
    marmap::getNOAA.bathy(
      lon1 = gridcell_bounds$lon1,
      lon2 = gridcell_bounds$lon2,
      lat1 = gridcell_bounds$lat1,
      lat2 = gridcell_bounds$lat2,
      keep = TRUE
    ) |>
    as.data.frame.table(convert = TRUE, stringsAsFactors = FALSE) |>
    dplyr::mutate(
      lon = as.numeric(Var1),
      lat = as.numeric(Var2),
      # NB: NOAA.bathy depth figures are negative, we'll assume positive
      ocean_depth = -Freq,
    ) |>
    dplyr::select(
      lon,
      lat,
      ocean_depth,
    ) |>
    pax_temptbl(pcon, tbl = _) |>
    dbplyr::remote_name()

  # NB: Use raw SQL to stop dbplyr converting h3_cell into double
  DBI::dbExecute(pcon, "DROP TABLE IF EXISTS paxdat_ocean_depth")
  DBI::dbExecute(
    pcon,
    dbplyr::build_sql(
      "CREATE TABLE paxdat_ocean_depth AS",
      # NB: Re-generate lon/lat so we choose the center of the cell
      " SELECT h3_cell_to_lng(h3_cell) AS lon",
      "      , h3_cell_to_lat(h3_cell) AS lat",
      "      , ocean_depth AS ocean_depth",
      "      , h3_cell AS h3_cell",
      " FROM (",
      "SELECT h3_latlng_to_cell(lat, lon, (SELECT res FROM h3_resolution)) AS h3_cell",
      "     , mean(ocean_depth) AS ocean_depth",
      " FROM ",
      dplyr::ident(raw_ocean_depth_tbl_name),
      " GROUP BY h3_latlng_to_cell(lat, lon, (SELECT res FROM h3_resolution))",
      ")",
      "ORDER BY h3_cell", # NB: duckdb likes data to be ordered
      con = pcon
    )
  )
  #DBI::dbGetQuery(pcon, "DESCRIBE paxdat_ocean_depth")

  return(dplyr::tbl(pcon, "paxdat_ocean_depth"))
}

dat_from_pkg <- function(pcon, name) {
  tbl_name <- paste0("paxdat_", name)

  env <- new.env(parent = emptyenv())
  utils::data(list = name, package = "pax", envir = env)
  data_df <- env[[name]]

  if (!(tbl_name %in% DBI::dbListTables(pcon))) {
    pax_import(
      pcon,
      env[[name]],
      name = tbl_name,
      cite = list(
        # TODO: Replace name with data name
        citation("pax")
      )
    )
  }

  return(dplyr::tbl(pcon, tbl_name))
}

# NB: Saved with:
#     write.table(mar::les_synaflokk(mar) |> dplyr::select(
#       sampling_type = synaflokkur_nr,
#       sampling_type_desc_en = enskt_heiti,
#       sampling_type_desc_is = heiti
#     ), file = "pax/data/sampling_type_desc.txt")
pax_dat_sampling_type_desc <- function(pcon) {
  dat_from_pkg(pcon, "sampling_type_desc")
}

# NB: Saved with:
#     write.table(data.frame(
#       mfdb_gear_code = as.character(mfdb::gear$name),
#       mfdb_gear_code_desc = tools::toTitleCase(as.character(
#         mfdb::gear$description
#       )),
#       stringsAsFactors = FALSE
#     ), file = "pax/data/mfdb_gear_code_desc.txt")
pax_dat_mfdb_gear_code_desc <- function(pcon) {
  dat_from_pkg(pcon, "mfdb_gear_code_desc")
}
