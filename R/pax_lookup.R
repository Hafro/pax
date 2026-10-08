# Routines used to extract the lookup tables in data/ from mar. Run from the
# directory above the pax checkout, as the other data_update_* functions:
#   mar <- mar::connect_mar()
#   pax:::data_update_reitmapping(mar)
#   pax:::data_update_gear_mapping(mar)

data_update_reitmapping <- function(mar, path = "pax/data/reitmapping.txt") {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  id <- gridcell <- division <- subdivision <- lat <- lon <- size <- NULL

  mar::tbl_mar(mar, 'ops$bthe."reitmapping"') |>
    dplyr::collect() |>
    dplyr::transmute(
      id = as.integer(id),
      gridcell = as.integer(gridcell),
      division = as.integer(division),
      subdivision = as.integer(subdivision),
      lat,
      lon,
      size
    ) |>
    dplyr::arrange(id) |>
    as.data.frame() |>
    utils::write.table(file = path)
}

data_update_gear_mapping <- function(mar, path = "pax/data/gear_mapping.txt") {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  veidarfaeri <- gear <- gear_id <- mfdb_gear_code <- source <- NULL
  mfdb_gear_code_weight <- NULL

  biota <- mar::tbl_mar(mar, 'biota.gear_mapping') |>
    dplyr::collect() |>
    dplyr::transmute(
      gear_id = as.integer(veidarfaeri),
      mfdb_gear_code = gear,
      source = "biota.gear_mapping"
    )
  weight <- mar::tbl_mar(mar, 'ops$bthe."gear_mapping"') |>
    dplyr::collect() |>
    dplyr::transmute(
      gear_id = as.integer(veidarfaeri),
      mfdb_gear_code_weight = gear
    )

  out <- biota |>
    # Gear 91 (anglerfish gillnet) is missing from biota.gear_mapping
    dplyr::bind_rows(
      data.frame(gear_id = 91L, mfdb_gear_code = "GIL", source = "pax")
    ) |>
    dplyr::filter(!duplicated(gear_id)) |>
    dplyr::full_join(weight, by = "gear_id") |>
    dplyr::arrange(gear_id) |>
    as.data.frame()
  stopifnot(!anyNA(out$mfdb_gear_code))
  utils::write.table(out, file = path)
}
