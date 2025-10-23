# Was: tidypax::sampling_tables, tidypax::age_reading_status
pax_measurement.hafropax <- function(pcon) {
  kvarna_nr <- NULL # Mask NSE variable

  mar::les_maelingu(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    # TODO: From tidypax::sampling_tables - good idea, or should we expose it & let pax_sampling_detail do it, if so, what is it?
    dplyr::mutate(
      maeling_teg = ifelse(
        maeling_teg == 'OTOL' & is.na(kvarna_nr),
        'LEN',
        maeling_teg
      )
    ) |>
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
      liver_weight = lifur,
      count = fjoldi
    )
}
