pax_add_sampling_type_desc.hafropax <- function(
  tbl,
  lang = getOption('tidypax.lang')
) {
  pcon <- pax::as_pax(tbl)
  st_tbl <- mar::les_synaflokk(pcon$dbcon)
  if (lang == 'is') {
    st_tbl <- st_tbl |>
      dplyr::select(sampling_type = synaflokkur_nr, sampling_type_desc = heiti)
  } else {
    st_tbl <- st_tbl |>
      dplyr::select(
        sampling_type = synaflokkur_nr,
        sampling_type_desc = enskt_heiti
      )
  }

  tbl |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::left_join(st_tbl, by = c('sampling_type'))
}
