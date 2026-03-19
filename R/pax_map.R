#' Map plotting functions
#'
#' Functions to create base maps and add spatial layers for survey and
#' catch data around Iceland.
#'
#' @name pax_map
NULL

#' @param plot_greenland Boolean, include Greenland coastline?
#' @param plot_faroes Boolean, include Faroe Islands coastline?
#' @param xlim Numeric vector of length 2, longitude limits
#' @param ylim Numeric vector of length 2, latitude limits
#' @param low_res Boolean, use low-resolution world map?
#' @return \subsection{pax_map_base}{A ggplot2 object with Iceland coastline
#'   and a clean theme, ready for additional layers}
#' @rdname pax_map
# Was tidypax::base_plot
pax_map_base <- function(
  plot_greenland = FALSE,
  plot_faroes = FALSE,
  xlim = c(-26.5, -12),
  ylim = c(63, 67.25),
  low_res = FALSE
) {
  require(mapdata)
  if (low_res) {
    world <- 'world'
  } else {
    world <- 'worldHires'
  }

  # NSE variables
  long <- NULL
  lat <- NULL
  group <- NULL

  base <-
    ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::coord_quickmap(xlim = xlim, ylim = ylim)

  if (TRUE) {
    base <- base +
      ggplot2::geom_polygon(
        data = ggplot2::map_data(world, 'Iceland'),
        ggplot2::aes(long, lat),
        fill = 'gray70',
        colour = "black",
        linewidth = 0.2
      )
  }

  if (plot_faroes) {
    base <- base +
      ggplot2::geom_polygon(
        data = ggplot2::map_data('world', 'Faroe Islands'),
        ggplot2::aes(long, lat, group = group),
        fill = 'gray70',
        colour = "black",
        linewidth = 0.2
      )
  }
  if (plot_greenland) {
    base <- base +
      ggplot2::geom_polygon(
        data = ggplot2::map_data(world, 'greenland'),
        ggplot2::aes(long, lat, group = group),
        fill = 'gray70',
        colour = "black",
        linewidth = 0.2
      )
  }

  base <-
    base +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14),
      legend.background = ggplot2::element_rect(fill = "white"),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = grid::unit(0.5, 'cm'),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.spacing = grid::unit(0, 'cm'),
      plot.margin = grid::unit(c(0, 0, 0, 0), 'cm'),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
  return(base)
}

#' @param base A ggplot2 map object, as returned by [pax_map_base()]
#' @param ocean_depth_tbl A dplyr query with columns ``lon``, ``lat``, and
#'   ``ocean_depth``
#' @param depth_lines Numeric vector of depth contour values to draw (negative,
#'   in metres)
#' @return \subsection{pax_map_layer_depth}{The ``base`` ggplot2 object with
#'   depth contour lines added}
#' @rdname pax_map
# Was included in tidypax::base_plot
pax_map_layer_depth <- function(
  base,
  ocean_depth_tbl,
  depth_lines = c(-100, -500, -1000)
) {
  stopifnot(inherits(base, "ggplot2::ggplot"))

  # NSE variables
  lon <- NULL
  lat <- NULL
  ocean_depth <- NULL

  dd <-
    ocean_depth_tbl |>
    dplyr::filter(
      lon < local(max(base$coordinates$limits$x)),
      lon > local(min(base$coordinates$limits$x)),
      lat < local(max(base$coordinates$limits$y)),
      lat > local(min(base$coordinates$limits$y))
    ) |>
    dplyr::mutate(lon = round(lon, 1), lat = round(lat, 1)) |>
    dplyr::group_by(lon, lat) |>
    dplyr::summarize(ocean_depth = mean(ocean_depth, na.rm = TRUE)) |>
    dplyr::collect(n = Inf)

  base +
    ggplot2::geom_contour(
      data = dd,
      ggplot2::aes(lon, lat, z = -ocean_depth),
      breaks = depth_lines,
      colour = "black",
      linewidth = 0.1
    )
}

#' @param data A dplyr query with columns ``year``, ``lon``, ``lat``, and
#'   ``catch``, and optionally ``mfdb_gear_code``
#' @param breaks Numeric vector of contour fill break points
#' @param fill_lab Legend label for the fill scale
#' @param annotation Character, type of facet annotation: ``"year"`` or
#'   ``"gear"``
#' @param alpha Numeric, fill transparency (0--1)
#' @param na.fill Fill value used for missing catch data in contour
#'   interpolation
#' @param label_x,label_y Coordinates for the annotation label
#' @return \subsection{pax_map_layer_catch}{The ``base`` ggplot2 object with a
#'   filled contour catch layer and facets added}
#' @rdname pax_map
# Was tidypax::add_catch_layer
pax_map_layer_catch <- function(
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
  stopifnot(inherits(base, "ggplot2::ggplot"))
  pax_checkcols(
    data,
    "year",
    "lon",
    "lat",
    "catch",
    switch(annotation, gear = "mfdb_gear_code", NULL),
    expected = "logbook"
  )

  # NSE variables
  lon <- NULL
  lat <- NULL
  catch <- NULL
  level <- NULL
  year <- NULL
  description <- NULL
  mfdb_gear_code <- NULL

  base <-
    base +
    metR::geom_contour_fill(
      ggplot2::aes(lon, lat, z = catch, fill = ggplot2::after_stat(level)),
      na.fill = na.fill,
      alpha = alpha,
      breaks = breaks,
      data = data |> dplyr::collect()
    ) +
    ggplot2::labs(fill = fill_lab) +
    pax_scale_fill_crayola()

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
          pax_describe_mfdb_gear_code()
      ) +
      ggplot2::facet_wrap(~mfdb_gear_code, dir = 'v', ncol = 2)
  } else {
    base
  }
}
