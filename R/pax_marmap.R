pax_marmap_ocean_depth <- function(
  lat1 = NULL,
  lon1 = NULL,
  lat2 = NULL,
  lon2 = NULL
) {
  tmp_pcon <- pax_connect(":memory:")
  on.exit(DBI::dbDisconnect(tmp_pcon), add = TRUE)

  if (is.null(lat1) && is.null(lon1) && is.null(lat2) && is.null(lon2)) {
    # No bounds given, import pre-cached copy in package
    raw_ocean_depth_tbl_name <- pax_temptbl(
      tmp_pcon,
      "paxdat_raw_ocean_depth_defbounds"
    ) |>
      dbplyr::remote_name()
  } else {
    # Fetch data from marmap
    # Fetch unaggregated data first, save to a temporary table
    raw_ocean_depth_tbl_name <-
      marmap_ocean_depth(lat1, lon1, lat2, lon2) |>
      pax_temptbl(tmp_pcon, tbl = _) |>
      dbplyr::remote_name()
  }

  # Do H3 aggregation inside duckDB, to save having an R H3 dependency
  # NB: Use raw SQL to stop dbplyr converting h3_cell into double
  agg_ocean_depth_tbl_name <- basename(tempfile("h3_ocean_depth_"))
  DBI::dbExecute(
    tmp_pcon,
    dbplyr::build_sql(
      "CREATE TEMPORARY TABLE ",
      dplyr::ident(agg_ocean_depth_tbl_name),
      " AS",
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
      con = tmp_pcon
    )
  )
  tbl <- dplyr::tbl(tmp_pcon, agg_ocean_depth_tbl_name) |> dplyr::collect()

  return(pax_decorate(tbl, cite = citation("marmap"), name = "ocean_depth"))
}

# Fetch ocean_depth table via. marmap
marmap_ocean_depth <- function(lat1, lon1, lat2, lon2) {
  raw_ocean_depth_tbl_name <-
    marmap::getNOAA.bathy(
      lon1 = lon1,
      lon2 = lon2,
      lat1 = lat1,
      lat2 = lat2,
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
    )
}

update_data_ocean_depth <- function() {
  utils::data("gridcell", package = "pax")
  defbounds <- gridcell |>
    dplyr::summarize(
      lat1 = min(lat, na.rm = TRUE),
      lon1 = min(lon, na.rm = TRUE),
      lat2 = max(lat, na.rm = TRUE),
      lon2 = max(lon, na.rm = TRUE)
    ) |>
    as.data.frame() |>
    as.list()

  write.table(
    do.call(marmap_ocean_depth, defbounds),
    file = "pax/data/raw_ocean_depth_defbounds.txt"
  )
}
