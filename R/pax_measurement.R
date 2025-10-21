pax_measurement_agelen.hafropax <- function(pcon) {
  mar::les_maelingu(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::filter(
      maeling_teg == 'OTOL',
      !is.na(aldur),
      !is.na(kynthroski_nr)
    ) |>
    dplyr::select(
      species = tegund_nr,
      sample_id = synis_id,
      measurement_id = maeling_id,
      age = aldur,
      maturity_stage = kynthroski_nr, # NB: Not reducing down to mature/immature, since the cut-off will be species specific
      length = lengd,
      weight = thyngd,
      count = fjoldi
    )
}

pax_measurement_typesummary.hafropax <- function(pcon) {
  mar::les_maelingu(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::group_by(synis_id) |>
    dplyr::summarise(
      n_total = sum(fjoldi),
      n_LENC = sum(ifelse(maeling_teg == 'LENC', 1, 0) * fjoldi),
      n_CNT = sum(ifelse(maeling_teg == 'CNT', 1, 0) * fjoldi),
      n_LENM = sum(ifelse(maeling_teg == 'LENM', 1, 0) * fjoldi),
      n_SAMP = sum(ifelse(maeling_teg == 'SAMP', 1, 0) * fjoldi),
      n_OTOL = sum(ifelse(maeling_teg == 'OTOL', 1, 0) * fjoldi),
      n_LEN = sum(ifelse(maeling_teg == 'LEN', 1, 0) * fjoldi),
      n_CATC = sum(ifelse(maeling_teg == 'CATC', 1, 0) * fjoldi),
      n_TOTC = sum(ifelse(maeling_teg == 'TOTC', 1, 0) * fjoldi),
    )
}

pax_measurement_detail.hafropax <- function(pcon) {
  mar::les_maelingu(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::select(
      individual_id = maeling_id,
      sample_id = synis_id,
      species = tegund_nr,
      measurement_type = maeling_teg,
      length_cm = lengd,
      age = aldur,
      sex = kyn_nr,
      maturity_stage = kynthroski_nr,
      weight_g = thyngd,
      gonad_weight = kynfaeri,
      gut_weight = magi,
      liver_weight = lifur
    )
}
