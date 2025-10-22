# Was tidypax::catch_data
pax_catch.hafropax <- function(
  pcon,
  species,
  year_end = lubridate::year(Sys.Date())
) {
  mar::tbl_mar(pcon$dbcon, 'ops$bthe."logbooks_compiled"') |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::filter(
      species == local(species),
      year < local(year_end)
    ) |>
    dplyr::rename(mfdb_gear_code = gear)
}
