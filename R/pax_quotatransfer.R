# Was: tidypax::quota_transfer_table
pax_quotatransfer_summary <- function(tbl) {
  con <- dbplyr::remote_con(tbl)

  tbl |>
    dplyr::mutate(
      diff = varanlegt - afli,
      diffp = diff / varanlegt,
      diff = sprintf("%s (%.1f %%)", diff, 100 * diffp),
      stada = sprintf("%s (%.1f %%)", stada, 100 * stada / kvoti)
    ) |>
    dplyr::select(
      Period = timabil,
      TAC = varanlegt,
      Catch = afli,
      Diff = diff,
      TACtrans = kvoti,
      Diff_trans = stada
    )
}

# Was: quota_transfer_plot
pax_quotatransfer_plot <- function(tbl) {
  con <- dbplyr::remote_con(tbl)

  tbl |>
    head(-1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      #m_ara = n_ar,
      m_p = 100 * (m_ara - n_ar - onotad) / varanlegt,
      til_p = 100 * tilf / varanlegt,
      m_ara = (m_ara - n_ar - onotad) / 1e3,
      tilf = tilf / 1e3
    ) |>
    dplyr::select(timabil, tilf, m_ara, m_p, til_p) |>
    tidyr::gather(col, value, -timabil) |>
    dplyr::mutate(
      col = ifelse(
        col == 'm_ara',
        'Between years',
        ifelse(
          col == 'm_p',
          'Between years (%)',
          ifelse(col == 'tilf', 'Between species', 'Between species (%)')
        )
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(timabil, value)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::facet_wrap(~col, ncol = 2, scale = 'free_y') +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      strip.background = element_blank()
    ) +
    ggplot2::labs(x = 'Quota period', y = 'Transfers (in thousand t)')
}
