pax_samplingpos.hafropax <- function(
  pcon,
  species,
  year_end = lubridate::year(Sys.Date()) - 1,
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  na_gear = 'BMT',
  sampling_type = c(1, 2, 3, 4, 8),
  # TODO: Can we always apply a data_type filter? wasn't one before
  data_type = c('LEN', 'LENM', 'OTOL')
) {
  mar::les_stod(pcon$dbcon) |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::left_join(mar::les_syni(pcon$dbcon), by = 'stod_id') |>
    # TODO: joining les_lengd with filter(tegund_nr == local(species)), but should be a join condition?
    dplyr::inner_join(mar::les_lengd(pcon$dbcon), by = 'synis_id') |>
    dplyr::left_join(
      mar::tbl_mar(pcon$dbcon, 'biota.gear_mapping'),
      by = 'veidarfaeri'
    ) |>
    dplyr::select(
      lat = kastad_breidd,
      lon = kastad_lengd,
      year = ar,
      sampling_type = synaflokkur_nr,
      data_type = maeling_teg,
      species_code = tegund_nr, # TODO: Should this have a vocab and/or consistent name?
      mfdb_gear_code = ifelse(is.na(gear), local(na_gear), mfdb_gear_code),
      trip = leidangur
    ) |>
    ## skip MAGEI and MOGUN, these are stomach samples and should be a seperate sampling type
    dplyr::filter(
      !(trip %like% 'MAG%'),
      !(trip %like% 'MO%')
    ) |>
    dplyr::filter(
      # TODO: This was %in%, but year_range was the final year?
      year < local(year_end),
      sampling_type %in% local(sampling_type),
      species_nr == local(species),
      data_type %in% local(data_type),
      mfdb_gear_code %in% local(mfdb_gear_code)
    ) |>
    dplyr::select(
      lat = kastad_breidd,
      lon = kastad_lengd,
      year = ar,
      mfdb_gear_code
    ) |>
    dplyr::distinct() # TODO: Suspicious
}
