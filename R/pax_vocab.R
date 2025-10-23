pax_add_sampling_type_desc <- function(tbl, lang = getOption('tidypax.lang')) {
  UseMethod("pax_add_sampling_type_desc", as_pax(tbl))
}
pax_add_mfdb_gear_code_desc <- function(tbl, lang = getOption('tidypax.lang')) {
  UseMethod("pax_add_mfdb_gear_code_desc", as_pax(tbl))
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
  pcon <- as_pax(tbl)
  # TODO: Switch to new gear code table and store it locally
  # TODO: Far too big for a pax_temptbl, but currently doing it anyway
  gc_tbl <- data.frame(
    mfdb_gear_code = as.character(mfdb::gear$name),
    mfdb_gear_code_desc = tools::toTitleCase(as.character(
      mfdb::gear$description
    )),
    stringsAsFactors = FALSE
  )

  tbl |> dplyr::left_join(pax_temptbl(pcon, gc_tbl), by = c('mfdb_gear_code'))
}
