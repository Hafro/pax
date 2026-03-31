#' Summarise and visualise sampling data
#'
#' Functions to extract position summaries, overview plots, and data quality
#' tables from commercial sampling data.
#'
#' @param tbl A dplyr query from a sampling table, as returned by
#'   [pax_mar_sampling()]
#' @name pax_sampling
NULL

#' @param mfdb_gear_code Character vector of gear codes to include
#' @param sampling_type Integer vector of sampling type codes to include
#' @param measurement_type Character vector of measurement types to include
#' @return \subsection{pax_sampling_detail}{A data.frame wide-pivoted by gear,
#'   with columns ``year`` and per-gear columns for number of samples
#'   (``n``), total lengths (``n_lengths``), and otolith readings
#'   (``n_otol``)}
#' @rdname pax_sampling
# Was: tidypax::sampling_tables
pax_sampling_detail <- function(
  tbl, # sampling joined to measurement
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  sampling_type = c(1, 2, 3, 4, 8),
  measurement_type = c('LEN', 'LENM', 'OTOL') # NB: Was data_type
) {
  pcon <- dbplyr::remote_con(tbl)

  # NSE variables
  year <- NULL
  sample_id <- NULL
  count <- NULL
  otol <- NULL
  mfdb_gear_code_desc <- NULL
  n <- NULL
  n_lengths <- NULL
  n_otol <- NULL

  tbl |>
    dplyr::filter(
      mfdb_gear_code %in% local(mfdb_gear_code),
      sampling_type %in% local(sampling_type),
      measurement_type %in% local(measurement_type)
    ) |>
    dplyr::mutate(
      otol = ifelse(measurement_type == 'OTOL', 1, 0)
    ) |>
    dplyr::group_by(year, mfdb_gear_code) |>
    dplyr::summarise(
      n = dplyr::n_distinct(sample_id, na.rm = TRUE),
      n_lengths = sum(count, na.rm = TRUE),
      n_otol = sum(count * otol, na.rm = TRUE)
    ) |>
    pax_describe_mfdb_gear_code(pcon) |>
    dplyr::select(-mfdb_gear_code) |>
    tidyr::pivot_wider(
      names_from = mfdb_gear_code_desc,
      values_from = c(n, n_lengths, n_otol),
      values_fill = 0,
      names_glue = "{mfdb_gear_code_desc}__{.value}",
      names_sort = TRUE
    ) |>
    dplyr::select(sort(tidyselect::peek_vars())) |>
    dplyr::arrange(year)
}

#' @return \subsection{pax_sampling_age_reading_status}{A dplyr query with
#'   columns ``year``, ``species``, ``sampling_type``, ``total`` (number of
#'   otolith samples), ``read`` (number with an age assigned), and ``p``
#'   (proportion read)}
#' @rdname pax_sampling
# Was: tidypax::age_reading_status
pax_sampling_age_reading_status <- function(
  tbl, # sampling joined to measurement
  measurement_type = c('OTOL')
) {
  pcon <- dbplyr::remote_con(tbl)

  # NSE variables
  age <- NULL
  year <- NULL
  read <- NULL
  species <- NULL
  sampling_type <- NULL
  total <- NULL

  tbl |>
    dplyr::filter(measurement_type %in% local(measurement_type)) |>
    dplyr::mutate(read = nvl2(age, 1, 0)) |>
    dplyr::group_by(year, species, sampling_type) |>
    dplyr::summarise(total = dplyr::n(), read = sum(read, na.rm = TRUE)) |>
    dplyr::mutate(p = read / total)
}
