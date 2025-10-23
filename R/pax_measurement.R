pax_measurement <- function(pcon) UseMethod("pax_measurement")
pax_measurement_agelen_summary <- function(tbl) {
  UseMethod("pax_measurement_agelen_summary")
}
pax_measurement_type_summary <- function(tbl) {
  UseMethod("pax_measurement_typesummary")
}

# Was: 02-had:maturity_key
pax_measurement_agelen_summary.pax <- function(tbl) {
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

pax_measurement_type_summary.pax <- function(tbl) {
  tbl |>
    dplyr::group_by(sample_id) |>
    dplyr::summarise(
      n_total = sum(count),
      n_LENC = sum(ifelse(measurement_type == 'LENC', 1, 0) * count),
      n_CNT = sum(ifelse(measurement_type == 'CNT', 1, 0) * count),
      n_LENM = sum(ifelse(measurement_type == 'LENM', 1, 0) * count),
      n_SAMP = sum(ifelse(measurement_type == 'SAMP', 1, 0) * count),
      n_OTOL = sum(ifelse(measurement_type == 'OTOL', 1, 0) * count),
      n_LEN = sum(ifelse(measurement_type == 'LEN', 1, 0) * count),
      n_CATC = sum(ifelse(measurement_type == 'CATC', 1, 0) * count),
      n_TOTC = sum(ifelse(measurement_type == 'TOTC', 1, 0) * count),
    )
}
