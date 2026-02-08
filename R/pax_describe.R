#' Add description columns for known vocabularies
#'
#' Add columns containing descriptions to a dplyr query
#'
#' @param lang Desired language for descriptions, either ``en`` or ``is``.
#' @param tbl A dplyr query to apply descriptions to
#' @name pax_describe
NULL

#' @return \subsection{pax_describe_sampling_type}{A dplyr query, with a ``sampling_type_desc`` column describing ``sampling_type`` values}
#' @rdname pax_describe
pax_describe_sampling_type <- function(
  tbl,
  lang = getOption('pax.lang')
) {
  pcon <- dbplyr::remote_con(tbl)

  st_tbl <- pax_temptbl(pcon, "paxdat_sampling_type_desc")
  if (lang == 'is') {
    st_tbl <- st_tbl |>
      dplyr::select(sampling_type, sampling_type_desc = sampling_type_desc_is)
  } else {
    st_tbl <- st_tbl |>
      dplyr::select(sampling_type, sampling_type_desc = sampling_type_desc_en)
  }

  tbl |>
    dplyr::left_join(st_tbl, by = c('sampling_type'))
}

#' @return \subsection{pax_describe_mfdb_gear_code}{A dplyr query, with a ``mfdb_gear_code_desc`` column describing ``mfdb_gear_code`` values}
#' @rdname pax_describe
pax_describe_mfdb_gear_code <- function(
  tbl,
  lang = getOption('pax.lang')
) {
  pcon <- dbplyr::remote_con(tbl)

  st_tbl <- pax_temptbl(pcon, "paxdat_mfdb_gear_code_desc")
  other_desc <- st_tbl |>
    dplyr::filter(is.na(mfdb_gear_code)) |>
    dplyr::pull(mfdb_gear_code_desc) |>
    as.character()

  tbl |>
    dplyr::left_join(st_tbl, by = c('mfdb_gear_code')) |>
    dplyr::mutate(
      mfdb_gear_code_desc = coalesce(mfdb_gear_code_desc, local(other_desc))
    )
}
data_update_mfdb_gear_code_desc <- function() {
  write.table(
    data.frame(
      mfdb_gear_code = c(as.character(mfdb::gear$name), NA),
      mfdb_gear_code_desc = c(
        tools::toTitleCase(as.character(
          mfdb::gear$description
        )),
        "Other"
      ),
      stringsAsFactors = FALSE
    ),
    file = "pax/data/mfdb_gear_code_desc.txt"
  )
}
