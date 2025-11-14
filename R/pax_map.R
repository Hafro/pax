pax_map_base <- function(
  con,
  bathymetry = pax_dat_noaa_bathymetry(con),
  # TODO: Could we have a map_region & auto-chosem xlim/ylim?
  plot_greenland = FALSE,
  plot_faroes = FALSE,
  xlim = c(-26.5, -12),
  ylim = c(63, 67.25),
  low_res = FALSE,
  depth_lines = c(-100, -500, -1000)
) {
  if (low_res) {
    world <- 'world'
  } else {
    world <- 'worldHires'
  }
  iceland <- map_data(world, 'Iceland')
  greenland <- map_data(world, 'greenland')
  faroes <- map_data('world', 'Faroe Islands')

  dd <-
    bathymetry |>
    dplyr::filter(
      longitude < local(max(xlim)),
      longitude > local(min(xlim)),
      latitude < local(max(ylim)),
      latitude > local(min(ylim))
    ) |>
    dplyr::collect(n = Inf)

  base <-
    ggplot2::ggplot() +
    ggplot2::geom_contour(
      data = dd,
      ggplot2::aes(longitude, latitude, z = noaa_depth),
      breaks = depth_lines,
      colour = "black",
      size = 0.1
    ) +
    ggplot2::geom_polygon(
      data = iceland,
      ggplot2::aes(long, lat),
      fill = 'gray70',
      colour = "black",
      size = 0.2
    ) +
    ggplot2::theme_bw() +
    ggplot2::coord_quickmap(xlim = xlim, ylim = ylim)

  if (plot_faroes) {
    base <- base +
      ggplot2::geom_polygon(
        data = faroes,
        ggplot2::aes(long, lat, group = group),
        fill = 'gray70',
        colour = "black",
        size = 0.2
      )
  }
  if (plot_greenland) {
    base <- base +
      ggplot2::geom_polygon(
        data = greenland,
        ggplot2::aes(long, lat, group = group),
        fill = 'gray70',
        colour = "black",
        size = 0.2
      )
  }

  base <-
    base +
    ggplot2::theme(
      plot.title = element_text(size = 14),
      legend.background = element_rect(fill = "white"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.5, 'cm'),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.spacing = unit(0, 'cm'),
      plot.margin = unit(c(0, 0, 0, 0), 'cm'),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  return(base)
}

# TODO: again, naming.
pax_map_add_catch <- function(
  base,
  data,
  breaks = c(seq(0, 20, by = 3), 40, 60),
  fill_lab = 'Catch (t/nm2)',
  annotation = 'year',
  alpha = 0.5,
  na.fill = -1,
  label_x = -18.5,
  label_y = 65
) {
  base <-
    base +
    metR::geom_contour_fill(
      ggplot2::aes(x, y, z = catch, fill = stat(level)),
      na.fill = na.fill,
      alpha = alpha,
      breaks = breaks,
      data = data
    ) +

    ggplot2::labs(fill = fill_lab) +
    ggplot2::scale_fill_crayola()

  if (annotation == 'year') {
    base +
      ggplot2::geom_label(
        x = label_x,
        y = label_y,
        ggplot2::aes(label = year),
        data = data |> dplyr::select(year) |> dplyr::distinct()
      ) +
      ggplot2::facet_wrap(~year, ncol = 3, dir = 'v')
  } else if (annotation == 'gear') {
    base +
      ggplot2::geom_label(
        x = label_x,
        y = label_y,
        ggplot2::aes(label = description),
        data = data |>
          dplyr::select(mfdb_gear_code) |>
          dplyr::distinct() |>
          pax_add_mfdb_gear_code_desc()
      ) +
      ggplot2::facet_wrap(~mfdb_gear_code, dir = 'v', ncol = 2)
  } else {
    base
  }
}
