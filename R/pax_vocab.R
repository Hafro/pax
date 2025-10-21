pax_add_sampling_type_desc <- function(tbl, lang = getOption('tidypax.lang')) {
  UseMethod("pax_add_sampling_type_desc")
}
pax_add_mfdb_gear_code_desc <- function(tbl, lang = getOption('tidypax.lang')) {
  UseMethod("pax_add_mfdb_gear_code_desc")
}

pax_add_sampling_type_desc.pax <- function(
  tbl,
  lang = getOption('tidypax.lang')
) {
  # TODO: Dump les_synaflokk(pcon$dbcon) into attached table, turn it into a pax_temptbl
  stop("TODO: NotImplemented")
}

pax_add_mfdb_gear_code_desc.pax <- function(
  tbl,
  lang = getOption('tidypax.lang')
) {
  # TODO: Switch to new gear code table and store it locally
  # TODO: Should be a pax_temptbl, but is too big atm
  gc_tbl <- mfdb::gear |>
    dplyr::select(
      mfdb_gear_code = as.character(gear),
      mfdb_gear_code_desc = tools::toTitleCase(as.character(description)),
    )

  tbl |> dplyr::left_join(gc_tbl, by = c('mfdb_gear_code'))
}
