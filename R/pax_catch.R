# Was: tidypax::cpue_plot (first half)
pax_add_cpue <- function(tbl) {
  con <- dbplyr::remote_con(tbl)

  tbl |>
    dplyr::mutate(
      effort = nvl(towtime / 60, nvl(hooks / 1000, nvl(nr_net, 1)))
    ) |>
    dplyr::mutate(effort = ifelse(mfdb_gear_code == 'DSE', 1, effort)) |>
    dplyr::filter(effort > 0) |>
    dplyr::mutate(cpue = ifelse(effort > 0, catch / effort, 0)) |>
    dplyr::filter(cpue > 0)
}

# Was: tidypax::cpue_plot
pax_catch_plot_cpue <- function(
  tbl,
  year_end = lubridate::year(Sys.Date()),
  limit = 0.5
) {
  # TODO: Expects cpue / effort columns as above
  tbl |>
    dplyr::group_by(year, mfdb_gear_code) |>
    dplyr::summarise(cpue = median(cpue)) |>
    dplyr::collect(n = Inf) |>
    dplyr::bind_rows(
      tbl |>
        dplyr::filter(catch > local(limit) * total, year < local(year_end)) |>
        dplyr::group_by(year, mfdb_gear_code) |>
        dplyr::summarise(cpue = median(cpue)) |>
        dplyr::collect(n = Inf),
      .id = 'Prop'
    ) |>
    dplyr::ungroup() |>
    ggplot2::ggplot(ggplot2::aes(year, cpue, lty = Prop)) +
    ggplot2::geom_line() +
    ggplot2::scale_linetype_manual(
      labels = c(
        'All records',
        sprintf('> %s %% of catch', round(100 * limit))
      ),
      values = 1:2
    ) +
    ggplot2::facet_wrap(~mfdb_gear_code, scale = 'free_y') +
    ggplot2::theme_bw() +
    ggplot2::labs(y = 'Catch per unit effort', x = 'Year', lty = '') +
    ggplot2::theme(
      legend.position = c(0.1, 0.8),
      legend.background = element_blank(),
      strip.background = element_blank()
    )
}
