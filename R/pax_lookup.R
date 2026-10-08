# Routines used to extract the lookup tables in data/ from mar. Run from the
# directory above the pax checkout, as the other data_update_* functions:
#   mar <- mar::connect_mar()
#   pax:::data_update_reitmapping(mar)
#   pax:::data_update_gear_mapping(mar)

data_update_reitmapping <- function(mar, path = "pax/data/reitmapping.rda") {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  # NSE variables
  id <- gridcell <- division <- subdivision <- lat <- lon <- size <- NULL

  out <- mar::tbl_mar(mar, 'ops$bthe."reitmapping"') |>
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
    as.data.frame()
  # NB: .rda, as write.table() keeps only 15 significant digits
  reitmapping <- out
  save(reitmapping, file = path, compress = "xz")
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

data_update_noaa_bathymetry <- function(
  mar,
  path = "pax/data/noaa_bathymetry.rda"
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  noaa_bathymetry <- mar::tbl_mar(mar, 'ops$bthe."noaa_bathymetry"') |>
    dplyr::collect() |>
    as.data.frame()
  noaa_bathymetry$reitur <- as.integer(noaa_bathymetry$reitur)
  noaa_bathymetry$smareitur <- as.integer(noaa_bathymetry$smareitur)
  save(noaa_bathymetry, file = path, compress = "xz")
}

#' Fill missing MFDB gear codes from the gear mapping
#'
#' Gives rows with a missing ``mfdb_gear_code`` the MFDB gear code of their
#' gear (``gear_id``) in the [gear_mapping] dataset, e.g. gear 91
#' (anglerfish gillnet), which ``biota.gear_mapping`` leaves unmapped, as
#' ``GIL``. Other rows are unchanged.
#'
#' @param tbl A dplyr query with ``gear_id`` and ``mfdb_gear_code`` columns,
#'   e.g. the pax ``station`` or ``landings`` table
#' @param gear_mapping_tbl The gear mapping, by default the [gear_mapping]
#'   dataset
#' @return ``tbl`` with ``mfdb_gear_code`` filled where it was missing
pax_fill_mfdb_gear_code <- function(
  tbl,
  gear_mapping_tbl = pax_temptbl(dbplyr::remote_con(tbl), "paxdat_gear_mapping")
) {
  # NSE variables
  gear_id <- mfdb_gear_code <- mfdb_gear_code_map <- NULL

  tbl_colnames <- colnames(tbl)
  tbl |>
    dplyr::left_join(
      gear_mapping_tbl |>
        dplyr::select(gear_id, mfdb_gear_code_map = mfdb_gear_code),
      by = "gear_id"
    ) |>
    dplyr::mutate(
      mfdb_gear_code = dplyr::coalesce(
        mfdb_gear_code,
        as.character(mfdb_gear_code_map)
      )
    ) |>
    dplyr::select(dplyr::all_of(tbl_colnames))
}

data_update_diel_correction_reg <- function(
  mar,
  path = "pax/data/diel_correction_reg.rda"
) {
  if (!requireNamespace("mar", quietly = TRUE)) {
    stop("mar package not available, cannot import from DB")
  }

  out <- lapply(c('ops$krik."predressmb_05"', 'ops$krik."predressmh_05"'), function(t) {
    x <- mar::tbl_mar(mar, t) |> dplyr::collect() |> as.data.frame()
    data.frame(
      fleet = x$fleet,
      year = as.integer(x$ar),
      time = x$kl.kastad,
      length_group = as.integer(x$lgnr),
      lengths = x$letxt,
      mult = x$mult,
      meanmult = x$meanmult,
      scaledmult = x$scaledmult,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  out <- out[order(out$fleet, out$time, out$length_group), ]
  rownames(out) <- NULL
  diel_correction_reg <- out
  save(diel_correction_reg, file = path, compress = "xz")
}
