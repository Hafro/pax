data_update_gridcell <- function(mar) {
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

data_update_sampling_type_desc <- function(mar) {
  write.table(
    mar::les_synaflokk(mar) |>
      dplyr::select(
        sampling_type = synaflokkur_nr,
        sampling_type_desc_en = enskt_heiti,
        sampling_type_desc_is = heiti
      ),
    file = "pax/data/sampling_type_desc.txt"
  )
}
