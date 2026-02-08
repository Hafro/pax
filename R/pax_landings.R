# Was: tidypax::landings_by_gear
pax_landings_by_gear <- function(
  tbl,
  species = (tbl |> head(1) |> dplyr::pull(species)),
  year_end = lubridate::year(Sys.Date()),
  gear_group = list(
    Other = 'Var',
    BMT = c('BMT', 'NPT', 'SHT', 'PGT'),
    LLN = 'LLN',
    DSE = c('PSE', 'DSE')
  )
) {
  # TODO: Check required columns
  tbl |>
    dplyr::filter(
      species %in% local(species),
      year < local(tyr)
    ) |>
    pax_add_gear_group(gear_group) |>
    dplyr::group_by(year, species, gear_name, country, ices_region) |>
    dplyr::summarise(
      landings = sum(landings),
      num_boats = n_distinct(boat_id)
    )
}

# Was tidypax::landings_plot
pax_landings_plot <- function(
  tbl,
  ylab = 'Landings (in kt)',
  xlab = 'Year',
  breaks = seq(0, 1e5, by = 10)
) {
  # TODO: Check required columns
  tbl |>
    dplyr::group_by(year, country) |>
    dplyr::summarise(c = sum(landings)) |>
    dplyr::arrange(desc(country)) |>
    ggplot2::ggplot(ggplot2::aes(year, c / 1e3, fill = country)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::theme_bw() +
    ggplot2::labs(y = ylab, x = xlab, fill = '') +
    ggplot2::theme(
      legend.background = element_blank(),
      legend.position = c(0.15, 0.75)
    ) +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_fill_crayola()
}

# Was: tidypax::boat_summary_table
pax_landings_summary_boatlandings <- function(tbl) {
  `Total catch` <- NULL # Mask NSE variable

  # TODO: Check required columns - needs pax_landings_by_gear()?
  tbl |>
    pax_describe_mfdb_gear_code() |>
    dplyr::mutate(
      mfdb_gear_code_desc = ifelse(
        is.na(mfdb_gear_code_desc),
        'Other',
        mfdb_gear_code_desc
      )
    ) |>
    tidyr::pivot_wider(
      names_from = mfdb_gear_code_desc,
      values_from = c(landings, num_boats),
      values_fill = 0
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      `Total catch` = sum(dplyr::c_across(dplyr::contains('landings_')))
    ) |>
    dplyr::select(
      Year = year,
      dplyr::starts_with('num_boats_'),
      dplyr::starts_with('landings_'),
      `Total catch`
    ) |>
    #dplyr::select(-num_boats_Other) |>
    dplyr::arrange(Year) |>
    purrr::set_names(., gsub('num_boats_', 'Nr. ', names(.))) |>
    purrr::set_names(., gsub('landings_', '', names(.)))
}

# Was: tidypax::num_boats_table
pax_landings_summary_significantboats <- function(
  tbl,
  species,
  ices_division = c("5c"),
  start_year = 1991,
  year_end = lubridate::year(Sys.Date())
) {
  # TODO: Check required columns
  tbl |>
    dplyr::filter(
      species = species,
      ices_division %in% local(ices_division),
      year %in% local(start_year):local(year_end - 1)
    ) |>
    dplyr::group_by(year, boat_id) |>
    dplyr::summarise(c = sum(landings, na.rm = TRUE)) |>
    dplyr::filter(c > 0) |>
    dbplyr::window_order(ar, c) |>
    dplyr::group_by(ar) |>
    dplyr::mutate(cc = cumsum(c), ct = sum(c, na.rm = TRUE)) |>
    dplyr::filter(cc > 0.05 * ct) |>
    dplyr::summarise(n = n(), catch = sum(c, na.rm = TRUE) / 1e3) |>
    dplyr::arrange(ar)
}

# Was: num_boats_plot
# TODO: naming/structure, plot methods plot summary, or do both?
pax_landings_summary_significantboats_plot <- function(tbl) {
  # TODO: Check columns, expects pax_landings_summary_significantboats
  years <- tbl |> dplyr::distinct(years)
  breaks <- unique(years - years %% 5) # Round all years to nearest 5 years
  breaks <- c(breaks, max(breaks) + 5) # Add back on topmost year

  p1 <-
    ggplot2::ggplot(tbl, ggplot2::aes(catch, n, label = ar)) +
    ggplot2::geom_path(colour = 4, linetype = 1, alpha = 0.5) +
    ggplot2::geom_text(
      hjust = 0,
      nudge_x = 0.05,
      check_overlap = TRUE,
      size = 3
    ) +
    ggplot2::theme_light() +
    ggplot2::expand_limits(y = 0, x = 0) +
    #  ylim(c(0,250))+
    #  xlim(4000,12500)+
    ggplot2::labs(y = "", x = "Catch (tonnes)", color = "Year") +
    ggplot2::theme(
      legend.position = c(0.9, 0.3),
      legend.title = ggplot2::element_text(size = 5),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.5))
    )

  p2 <- ggplot2::ggplot(tbl, ggplot2::aes(ar, n)) +
    ggplot2::geom_line(col = 4) +
    ggplot2::scale_x_continuous(limits = years, breaks = breaks) +
    #  ylim(0,250)+
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
      x = "Year",
      y = "Number of vessles accounting for 95% of catch"
    ) +
    ggplot2::theme_light()

  p2 + p1
}
