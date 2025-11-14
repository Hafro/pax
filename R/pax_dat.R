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

# Was: 'ops$bthe."noaa_bathymetry" && ops$bthe."reitmapping"
# NB: Saved with:
#     mar::tbl_mar(mar,'ops$bthe."reitmapping"') |>
#       dplyr::filter(!is.na(gridcell), !is.na(division), !is.na(subdivision), !is.na(lat), !is.na(lon)) |>
#       dplyr::select(-id) |>
#       write.table(file = "pax/data/gridcell.txt")
pax_dat_gridcell <- function(pcon, gridcells) {
  name <- "gridcell"
  tbl_name <- "paxdat_gridcell"

  get_rows <- function(gridcells) {
    env <- new.env(parent = emptyenv())
    utils::data(list = name, package = "pax", envir = env)
    # TODO: Disable filtering for now, fetch for all of data file
    gridcell_df <- env[[name]] #|> dplyr::filter(gridcell %in% gridcells)

    depth_df <- marmap::getNOAA.bathy(
      lon1 = min(gridcell_df$lon, na.rm = TRUE),
      lon2 = max(gridcell_df$lon, na.rm = TRUE),
      lat1 = min(gridcell_df$lat, na.rm = TRUE),
      lat2 = max(gridcell_df$lat, na.rm = TRUE),
      keep = TRUE
    ) |>
      as.data.frame.table(convert = TRUE, stringsAsFactors = FALSE) |>
      dplyr::mutate(
        lon = as.numeric(Var1),
        lat = as.numeric(Var2),
        # NB: bathy depth figures are negative, we'll assume positive
        ocean_depth = -Freq,
        gridcell = geo::d2sr(lat, lon)
      ) |>
      dplyr::group_by(gridcell) |>
      dplyr::summarise(ocean_depth = mean(ocean_depth))

    return(dplyr::left_join(gridcell_df, depth_df, by = c("gridcell")))
  }

  if (tbl_name %in% DBI::dbListTables(pcon)) {
    # Filter gridcells by ones already in DB
    tbl_gridcells <- dplyr::tbl(pcon, tbl_name) |> dplyr::pull(gridcell)
    gridcells <- base::setdiff(gridcells, tbl_gridcells)

    if (length(gridcells) > 0) {
      DBI::dbWriteTable(
        pcon,
        tbl_name,
        get_rows(gridcells),
        append = TRUE
      )
    }
  } else {
    # TODO: We really want the citation in marmap::getNOAA.bathy, as well as citation("geo")
    pax_import(
      pcon,
      get_rows(gridcells),
      name = tbl_name,
      cite = list(
        citation("marmap"),
        citation("geo")
      )
    )
  }

  return(dplyr::tbl(pcon, tbl_name))
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
