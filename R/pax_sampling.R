pax_samplingpos <- function(
  pcon,
  species,
  year_end = lubridate::year(Sys.Date()) - 1,
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  na_gear = 'BMT',
  sampling_type = c(1, 2, 3, 4, 8),
  # TODO: Can we always apply a data_type filter? wasn't one before
  data_type = c('LEN', 'LENM', 'OTOL')
) {
  UseMethod("pax_samplingpos", pcon)
}

pax_sampling_position <- function(
  pcon,
  species,
  year_range = lubridate::year(Sys.Date()) - 1,
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  sampling_type = c(1, 2, 3, 4, 8)
) {
  pax_si(pcon) |> # TODO: Not the intended interface
    # TODO: Is this just making sure we only include rows with length data?
    dplyr::inner_join(
      mar::les_lengd(pcon$dbcon),
      by = c("sample_id", "synis_id")
    ) |>
    dplyr::filter(
      year %in% local(year_range),
      sampling_type %in% local(sampling_type),
      species == local(species), # TODO: not in pax_si query?
      mfdb_gear_code %in% local(mfdb_gear_code)
    ) |>
    dplyr::distinct() # TODO: Suspicious
}

# TODO: Consistent naming for parameters
# TODO: Generic?
pax_sampling_plot_overview <- function(
  pcon,
  species,
  year_end = lubridate::year(Sys.Date()),
  sampling_type = c(1, 2, 3, 4, 8),
  num_years = 5,
  # NB: translate option now "option(tidypax.lang)"
  na_gear = 'BMT'
) {
  pax_si(pcon) |> # TODO: Not the intended interface
    dplyr::filter(
      year < local(year_end),
      year > local(year_end - num_years),
      sampling_type %in% local(sampling_type)
    ) |>
    dplyr::inner_join(
      mar::les_lengd(pcon$dbcon),
      by = c("sample_id", "synis_id")
    ) |>
    dplyr::group_by(year, month, mfdb_gear_code, sampling_type) |>
    dplyr::summarise(n = dplyr::n_distinct(sample_id)) |>
    dplyr::group_by(mfdb_gear_code, ar) |>
    dplyr::arrange(month) |>
    dplyr::mutate(p = (n) / sum(n)) |>
    dplyr::group_by(mfdb_gear_code, year, month) |>
    dplyr::mutate(n = sum(n), pp = sum(p)) |>
    dplyr::ungroup() |>
    dplyr::full_join(
      # TODO: Was using mar::lods_oslaegt directly rather than mar::landadur_afli. Okay?
      pax_landings(as_pax(pcon)) |>
        dplyr::filter(species = species) |>
        # Landings by gear
        dplyr::group_by(species, year, month, mfdb_gear_code) |>
        dplyr::summarise(lnd = sum(landings)) |>
        # Window working out proportion of landings per month
        dplyr::group_by(species, year, mfdb_gear_code) |>
        dplyr::mutate(p.lnd = ifelse(sum(lnd) == 0, 0, (lnd) / sum(lnd))) |>
        dplyr::arrange(species, year, month),
      by = c("year", "month", "mfdb_gear_code")
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(pax_add_sampling_type_desc(pcon), by = "sampling_type") |>
    dplyr::left_join(
      pax_add_mfdb_gear_code_desc(pcon),
      by = "mfdb_gear_code"
    ) |>
    dplyr::collect(n = Inf) |>

    ggplot2::ggplot(ggplot2::aes(man, p.lnd)) +
    ggplot2::geom_bar(
      ggplot2::aes(y = p, fill = sampling_type_desc),
      stat = 'identity'
    ) +
    ggplot2::geom_text(ggplot2::aes(y = pp + 0.05, label = n)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(ar ~ mfdb_gear_code_desc) +
    ggplot2::scale_fill_crayola() +
    ggplot2::scale_x_continuous(breaks = c(seq(2, 12, by = 2))) +
    ggplot2::labs(x = 'Month', y = 'Percent samples/landings', fill = '') +
    ggplot2::theme(
      strip.background = element_blank(),
      legend.position = 'top'
    ) +
    ggplot2::theme(
      panel.grid.major = element_line(colour = "grey80", size = 0.25),
      panel.grid.minor = element_line(colour = "grey80", size = 0.25),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

pax_sampling_tables <- function(
  pcon,
  species,
  end_year = lubridate::year(Sys.Date()),
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  sampling_type = c(1, 2, 3, 4, 8),
  data_type = c('LEN', 'LENM', 'OTOL')
) {
  pax_si(pcon) |> # TODO: Not the intended interface
    pax_tbl(pcon) |>
    dplyr::filter(
      year < local(end_year),
      sampling_type %in% local(sampling_type),
      species %in% local(species),
      mfdb_gear_code %in% local(mfdb_gear_code),
      # TODO: Not available yet data_type %in% local(data_type)
    )
  # TODO: WHy not pax_add?
  dplyr::left_join(pax_measurement_typesummary(as_pax(pcon))) |>
    # TODO: Already rotated, won't make sense
    dplyr::filter(
      data_type %in% local(data_type)
    ) |>
    dplyr::mutate(
      otol = ifelse(maeling_teg == 'OTOL', 1, 0)
    ) |>
    dplyr::group_by(ar, mfdb_gear_code) |>
    dplyr::summarise(
      n = dplyr::n_distinct(synis_id),
      n_lengths = sum(fjoldi),
      n_otol = sum(fjoldi * otol)
    ) |>
    dplyr::left_join(
      pax_add_mfdb_gear_code_desc(pcon),
      by = 'mfdb_gear_code'
    ) |>
    dplyr::select(-mfdb_gear_code) |>
    tidyr::pivot_wider(
      names_from = description,
      values_from = c(n, n_lengths, n_otol),
      values_fill = 0,
      names_glue = "{description}__{.value}",
      names_sort = TRUE
    ) |>
    dplyr::select(sort(tidyselect::peek_vars())) |>
    dplyr::arrange(year) # |>
  #purrr::set_names(gsub('_n.+|_n','',names(.)))
}

pax_sampling_age_reading_status <- function(tbl) {
  pcon <- as_pax(tbl)

  pax_si(pcon) |>
    pax_tbl(pcon) |>
    # TODO: data_type filter moved, so not needed here
    # TODO: bah, this isn't working
    dplyr::mutate(lesid = nvl2(aldur, 1, 0)) |>
    dplyr::group_by(ar, tegund_nr, synaflokkur_nr) |>
    dplyr::summarise(n = n(), lesid = sum(lesid)) |>
    dplyr::mutate(p = lesid / n)
}
