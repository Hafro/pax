# Was: tidypax::cpue_plot (first half)
pax_add_cpue <- function(tbl) {
  pax_checkcols(
    tbl,
    "catch",
    "mfdb_gear_code",
    "tow_hooks",
    "tow_time",
    "tow_num_nets"
  )
  con <- dbplyr::remote_con(tbl)

  tbl |>
    dplyr::mutate(
      effort = coalesce(
        ifelse(mfdb_gear_code == 'DSE', 1, NA),
        tow_time / 60,
        tow_hooks / 1000,
        tow_num_nets,
        1
      )
    ) |>
    dplyr::filter(catch > 0, effort > 0) |>
    dplyr::mutate(cpue = catch / effort)
}

# Was: tidypax::cpue_plot
pax_logbook_cpue_plot <- function(
  tbl,
  year_end = lubridate::year(Sys.Date()),
  limit = 0.5
) {
  pax_checkcols(tbl, "year", "mfdb_gear_code", "cpue", "catch", "catch_total")

  tbl |>
    dplyr::group_by(year, mfdb_gear_code) |>
    dplyr::summarise(cpue = median(cpue, na.rm = TRUE)) |>
    dplyr::collect(n = Inf) |>
    dplyr::bind_rows(
      tbl |>
        dplyr::filter(
          catch > local(limit) * catch_total,
          year < local(year_end)
        ) |>
        dplyr::group_by(year, mfdb_gear_code) |>
        dplyr::summarise(cpue = median(cpue, na.rm = TRUE)) |>
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
      legend.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )
}
