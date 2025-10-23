pax_sampling <- function(
  pcon,
  species,
  # TODO: Saying 0:2025 is very inefficient, should we have start/end instead?
  year_range = lubridate::year(Sys.Date()) - 1,
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  na_gear = 'BMT',
  sampling_type = c(1, 2, 3, 4, 8),
  include_stomach = FALSE
) {
  UseMethod("pax_sampling", pcon)
}

pax_sampling_position_summary <- function(tbl) {
  tbl |>
    dplyr::select(lat, lon, year, mfdb_gear_code) |>
    dplyr::distinct()
}

# TODO: Generic?
# NB: translate option now "option(tidypax.lang)"
# Was: tidypax::sampling_overview_plot
pax_sampling_overview_plot <- function(tbl) {
  pcon <- as_pax(tbl)
  tbl |>
    dplyr::group_by(year, month, mfdb_gear_code, sampling_type) |>
    dplyr::summarise(n = dplyr::n_distinct(sample_id)) |>
    dplyr::group_by(mfdb_gear_code, year) |>
    dplyr::arrange(month) |>
    dplyr::mutate(p = (n) / sum(n)) |>
    dplyr::group_by(mfdb_gear_code, year, month) |>
    dplyr::mutate(n = sum(n), pp = sum(p)) |>
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
    pax_add_sampling_type_desc(pcon) |>
    pax_add_mfdb_gear_code_desc(pcon) |>
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

pax_sampling_detail <- function(
  tbl,
  measurement_type = c('LEN', 'LENM', 'OTOL') # NB: Was data_type
) {
  pcon <- as_pax(tbl)

  tbl |>
    # TODO: Why not pax_add_measurement_type?
    dplyr::left_join(pax_measurement(pcon), by = "sample_id") |>
    dplyr::filter(measurement_type %in% local(measurement_type)) |>
    dplyr::mutate(
      otol = ifelse(measurement_type == 'OTOL', 1, 0)
    ) |>
    dplyr::group_by(year, mfdb_gear_code) |>
    dplyr::summarise(
      n = dplyr::n_distinct(sample_id),
      n_lengths = sum(count),
      n_otol = sum(count * otol)
    ) |>
    # TODO: Still broken
    #pax_add_mfdb_gear_code_desc(pcon) |>
    #dplyr::select(-mfdb_gear_code) |>
    tidyr::pivot_wider(
      names_from = mfdb_gear_code,
      values_from = c(n, n_lengths, n_otol),
      values_fill = 0,
      names_glue = "{mfdb_gear_code}__{.value}",
      names_sort = TRUE
    ) |>
    dplyr::select(sort(tidyselect::peek_vars())) |>
    dplyr::arrange(year) # |>
  #purrr::set_names(gsub('_n.+|_n','',names(.)))
}

pax_sampling_age_reading_status <- function(
  tbl,
  measurement_type = c('OTOL')
) {
  pcon <- as_pax(tbl)

  tbl |>
    # TODO: Why not pax_add_measurement_type?
    dplyr::left_join(pax_measurement(pcon), by = "sample_id") |>
    dplyr::filter(measurement_type %in% local(measurement_type)) |>
    dplyr::mutate(read = nvl2(age, 1, 0)) |>
    dplyr::group_by(year, species, sampling_type) |>
    dplyr::summarise(total = n(), read = sum(read)) |>
    dplyr::mutate(p = read / total)
}
