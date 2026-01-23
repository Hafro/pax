# Was: tidypax::survey_locations
pax_station_location_summary <- function(
  tbl,
  ldist = dplyr::tbl(dbplyr::remote_con(tbl), "ldist") |>
    pax_ldist_scale_abund() |> # TODO: i.e. mar::skala_med_taldir, synis_id tegund_nr kvarnadir maeldir taldir vigt_synis vigt_afla
    pax_ldist_add_weight()
) {
  species_dummies <-
    tibble::tibble(species = 1:100, dummy = 1)

  tbl |>
    dplyr::left_join(ldist, by = c("sample_id", "species")) |>
    dplyr::mutate(dummy = 1) |>
    dplyr::left_join(
      pax_temptbl(pcon, species_dummies),
      by = c('species', 'dummy')
    ) |>
    dplyr::group_by(
      sample_id,
      begin_lat,
      begin_lon,
      year,
      sampling_type,
      species
    ) |>
    dplyr::summarise(
      bio = sum(
        abs(
          coalesce(count, 0) *
            coalesce(a, 0.01) *
            abs(coalesce(length, 0))^coalesce(b, 3)
        ) /
          abs(coalesce(pmax(tow_length, 0.1), 4)),
        na.rm = TRUE
      ) /
        1e3
    ) |>
    dplyr::mutate(zero_station = ifelse(bio == 0, 'Zero catch', 'Non zero')) |>
    dplyr::ungroup()
}
