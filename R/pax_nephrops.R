#' Nephrops grounds (UWTV survey strata)
#'
#' The 13 Norway lobster (*Nephrops*) grounds off south Iceland (FU1) over
#' which the UWTV burrow density is kriged, from the package data
#' [nephrops_grounds].
#'
#' @param crs Coordinate reference system of the result: the default 3057
#'   (ISN93, as stored, metres), or e.g. 4326 for longitude and latitude
#' @return An sf data frame with columns ``name`` (ground number, ``"1"`` to
#'   ``"13"``, text), ``area_name`` (the area the ground belongs to in the
#'   tables, e.g. ``"Breiðamerkurdjúp"``; several grounds can share an
#'   area), ``area`` (m², as the source file; the polygon areas from
#'   ``sf::st_area()`` differ slightly) and the polygons
#' @export
pax_nephrops_grounds <- function(crs = 3057) {
  env <- new.env(parent = emptyenv())
  utils::data(list = "nephrops_grounds", package = "pax", envir = env)
  out <- env$nephrops_grounds
  if (!identical(as.numeric(crs), 3057)) {
    out <- sf::st_transform(out, crs)
  }
  out
}
