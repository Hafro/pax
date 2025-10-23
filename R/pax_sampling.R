# Was: tidypax::sampling_position
pax_sampling.hafropax <- function(
  pcon,
  species,
  year_range = lubridate::year(Sys.Date()) - 1,
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  na_gear = 'BMT',
  sampling_type = c(1, 2, 3, 4, 8),
  # TODO: Optional so we can get the same results, but should it be?
  include_stomach = FALSE
) {
  mar::les_stod(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::left_join(mar::les_syni(pcon$dbcon), by = 'stod_id') |>
    dplyr::inner_join(mar::les_lengd(pcon$dbcon), by = 'synis_id') |>
    dplyr::left_join(
      mar::tbl_mar(pcon$dbcon, 'biota.gear_mapping'),
      by = 'veidarfaeri'
    ) |>
    dplyr::select(
      sample_id = synis_id,
      lat = kastad_breidd,
      lon = kastad_lengd,
      year = ar,
      month = man,
      sampling_type = synaflokkur_nr,
      species = tegund_nr,
      mfdb_gear_code = gear, # TODO: ifelse(is.na(gear), local(na_gear), gear),
      trip = leidangur
    ) |>
    dplyr::filter(
      year %in% local(year_range),
      sampling_type %in% local(sampling_type),
      species %in% local(species),
      mfdb_gear_code %in% local(mfdb_gear_code)
    ) -> out
  if (!include_stomach) {
    ## skip MAGEI and MOGUN, these are stomach samples and should be a seperate sampling type
    out <- dplyr::filter(out, !(trip %like% 'MAG%'), !(trip %like% 'MO%'))
  }
  return(out)
}
