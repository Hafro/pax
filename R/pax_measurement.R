#' Summarise measurement data
#'
#' Functions to extract structured summaries from a measurement table.
#'
#' @param tbl A dplyr query from a measurement table, as returned by
#'   [pax_mar_measurement()]
#' @name pax_measurement
NULL

#' @return \subsection{pax_measurement_agelen_summary}{A dplyr query of
#'   otolith-aged individuals, with columns ``species``, ``sample_id``,
#'   ``measurement_id``, ``age``, ``maturity_stage``, ``length``, ``weight``,
#'   and ``count``}
#' @rdname pax_measurement
# Was: 02-had:maturity_key
pax_measurement_agelen_summary <- function(tbl) {
  # NSE variables
  measurement_type <- NULL
  age <- NULL
  maturity_stage <- NULL
  species <- NULL
  sample_id <- NULL
  measurement_id <- NULL
  weight <- NULL
  count <- NULL

  tbl |>
    dplyr::filter(
      measurement_type == 'OTOL',
      !is.na(age),
      !is.na(maturity_stage)
    ) |>
    dplyr::select(
      species,
      sample_id,
      measurement_id,
      age,
      maturity_stage, # NB: Not reducing down to mature/immature, since the cut-off will be species specific
      length,
      weight,
      count
    )
}

#' @return \subsection{pax_measurement_type_summary}{A dplyr query aggregated
#'   by ``sample_id`` with count columns for each measurement type:
#'   ``n_total``, ``n_LENC``, ``n_CNT``, ``n_LENM``, ``n_SAMP``,
#'   ``n_OTOL``, ``n_LEN``, ``n_CATC``, and ``n_TOTC``}
#' @rdname pax_measurement
pax_measurement_type_summary <- function(tbl) {
  # NSE variables
  sample_id <- NULL
  count <- NULL
  measurement_type <- NULL

  tbl |>
    dplyr::group_by(sample_id) |>
    dplyr::summarise(
      n_total = sum(count, na.rm = TRUE),
      n_LENC = sum(
        ifelse(measurement_type == 'LENC', 1, 0) * count,
        na.rm = TRUE
      ),
      n_CNT = sum(
        ifelse(measurement_type == 'CNT', 1, 0) * count,
        na.rm = TRUE
      ),
      n_LENM = sum(
        ifelse(measurement_type == 'LENM', 1, 0) * count,
        na.rm = TRUE
      ),
      n_SAMP = sum(
        ifelse(measurement_type == 'SAMP', 1, 0) * count,
        na.rm = TRUE
      ),
      n_OTOL = sum(
        ifelse(measurement_type == 'OTOL', 1, 0) * count,
        na.rm = TRUE
      ),
      n_LEN = sum(
        ifelse(measurement_type == 'LEN', 1, 0) * count,
        na.rm = TRUE
      ),
      n_CATC = sum(
        ifelse(measurement_type == 'CATC', 1, 0) * count,
        na.rm = TRUE
      ),
      n_TOTC = sum(
        ifelse(measurement_type == 'TOTC', 1, 0) * count,
        na.rm = TRUE
      ),
    )
}
