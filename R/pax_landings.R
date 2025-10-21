# Was: tidypax::landings_by_gear
pax_landings.hafopax <- function(pcon) {
  mar::landadur_afli(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::select(
      species = tegund_nr,
      mfdb_gear_code = mfdb_gear_code,
      ices_area = ices_svaedi,
      year = ar,
      month = man,
      ices_division = ices_svaedi,
      country = land,
      boat_id = skip_nr,
      landings = magn_oslaegt
    )
}
