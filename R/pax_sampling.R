pax_sampling_position_summary <- function(tbl) {
  tbl |>
    dplyr::select(lat, lon, year, mfdb_gear_code) |>
    dplyr::distinct()
}

# NB: translate option now "option(tidypax.lang)"
# Was: tidypax::sampling_overview_plot
pax_sampling_overview_plot <- function(
  tbl,
  landings_tbl = dplyr::tbl(dbplyr::remote_con(tbl), "landings")
) {
  pcon <- dbplyr::remote_con(tbl)

  tbl |>
    dplyr::group_by(year, month, mfdb_gear_code, sampling_type) |>
    dplyr::summarise(n = dplyr::n_distinct(sample_id)) |>
    dplyr::group_by(mfdb_gear_code, year) |>
    dplyr::arrange(month) |>
    dplyr::mutate(p = (n) / sum(n)) |>
    dplyr::group_by(mfdb_gear_code, year, month) |>
    dplyr::mutate(n = sum(n), pp = sum(p)) |>
    dplyr::full_join(
      landings_tbl |>
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
    pax_describe_sampling_type(pcon) |>
    pax_describe_mfdb_gear_code(pcon) |>
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

# Was: tidypax::sampling_tables
pax_sampling_detail <- function(
  tbl, # sampling joined to measurement
  mfdb_gear_code = c('BMT', 'LLN', 'DSE'),
  sampling_type = c(1, 2, 3, 4, 8),
  measurement_type = c('LEN', 'LENM', 'OTOL') # NB: Was data_type
) {
  pcon <- dbplyr::remote_con(tbl)

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
      n = dplyr::n_distinct(sample_id),
      n_lengths = sum(count),
      n_otol = sum(count * otol)
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

# Was: tidypax::age_reading_status
pax_sampling_age_reading_status <- function(
  tbl, # sampling joined to measurement
  measurement_type = c('OTOL')
) {
  pcon <- dbplyr::remote_con(tbl)

  tbl |>
    dplyr::filter(measurement_type %in% local(measurement_type)) |>
    dplyr::mutate(read = nvl2(age, 1, 0)) |>
    dplyr::group_by(year, species, sampling_type) |>
    dplyr::summarise(total = n(), read = sum(read)) |>
    dplyr::mutate(p = read / total)
}
