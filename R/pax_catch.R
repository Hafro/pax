# Was tidypax::catch_data
pax_catch.hafropax <- function(
  pcon,
  year_end = lubridate::year(Sys.Date())
) {
  mar::tbl_mar(pcon$dbcon, 'ops$bthe."logbooks_compiled"') |>
    pax::pax_tbl(pcon = pcon) |>
    # TODO: , species == local(species)
    dplyr::filter(year < local(year_end)) |>
    dplyr::rename(mfdb_gear_code = gear)
}
