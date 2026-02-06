# TODO: N/A

#' crayola fills
#'
#' @param defines Length of color entries in the vector of palettes is calculated as n*12 (this length should be longer than levels filled).
#' @param ... Additional arguments passed to ggplot2::scale_fill_manual.
#'
#' @return Functions the same as ggplot2::scale_fill_manual but with color palette vector defined according to the 'crayola' spectrum
#' @export

pax_scale_fill_crayola <- function(n = 100, ...) {
  # taken from RColorBrewer::brewer.pal(12, "Paired")
  pal <- c(
    "#A6CEE3",
    "#1F78B4",
    "#B2DF8A",
    "#33A02C",
    "#FB9A99",
    "#E31A1C",
    "#FDBF6F",
    "#FF7F00",
    "#CAB2D6",
    "#6A3D9A",
    "#FFFF99",
    "#B15928"
  )
  pal <- rep(pal, n)
  ggplot2::scale_fill_manual(values = pal, ...)
}

#' crayola colors
#'
#' @param defines Length of color entries in the vector of palettes is calculated as n*12 (this length should be longer than levels filled).
#' @param ... Additional arguments passed to ggplot2::scale_fill_manual.
#'
#' @return Functions the same as ggplot2::scale_fill_manual but with color palette vector defined according to the 'crayola' spectrum
#' @export

pax_scale_col_crayola <- function(n = 100, ...) {
  # taken from RColorBrewer::brewer.pal(12, "Paired")
  pal <- c(
    "#A6CEE3",
    "#1F78B4",
    "#B2DF8A",
    "#33A02C",
    "#FB9A99",
    "#E31A1C",
    "#FDBF6F",
    "#FF7F00",
    "#CAB2D6",
    "#6A3D9A",
    "#FFFF99",
    "#B15928"
  )
  pal <- rep(pal, n)
  ggplot2::scale_color_manual(values = pal, ...)
}
