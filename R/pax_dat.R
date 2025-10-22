# TODO: Just 'depth'?
# Was tidypax::depth_layer()
pax_dat_noaa_bathymetry.hafropax <- function(pcon) {
  x <- NULL # Mask NSE variable
  y <- NULL # Mask NSE variable
  z <- NULL # Mask NSE variable

  mar::tbl_mar(pcon$dbcon, 'ops$bthe."noaa_bathymetry"') |>
    pax::pax_tbl(pcon = pcon) |>
    dplyr::select(
      # TODO: Guessed, but seems right based on limits
      longitude = x,
      latitude = y,
      noaa_depth = z,
      # TODO: Should be gridcell = 10*reitur + smareitur?
      gridcell = smareitur,
    )
}

# TODO: Generation code in https://gitlab.hafogvatn.is/dag/00-setup/-/blob/master/support_tables/noaa_bathymetry.R
# TODO: Takes ~5s, okay to do if not already available.
if (FALSE) {
  depths <- marmap::getNOAA.bathy(
    lon1 = -80,
    lon2 = 50,
    lat1 = 30,
    lat2 = 80,
    keep = TRUE
  )
  dd <-
    as.data.frame.table(depths, convert = TRUE, stringsAsFactors = FALSE) |>
    dplyr::mutate(
      x = as.numeric(Var1),
      y = as.numeric(Var2),
      z = Freq
    ) |>
    dplyr::mutate(
      reitur = geo::d2r(y, x),
      smareitur = geo::d2sr(y, x)
    )
}

pax_dat_lw_coeffs.hafropax <- function(pcon) {
  # TODO: Generation, but where is lwcoeff.csv? https://gitlab.hafogvatn.is/dag/00-setup/-/blob/master/SI_db_setup/01b-dbsetup.R#L477
  mar::lw_coeffs(pcon$dbcon)
}

# Was: tidypax::add_regions (data)
pax_dat_reitmapping.hafropax <- function(pcon) {
  # TODO: reitmapping *data* is ~iceland-specific, but not hafropax-specific
  # TODO: Generation: https://gitlab.hafogvatn.is/dag/00-setup/-/blob/master/SI_db_setup/01b-dbsetup.R#L440
  mar::tbl_mar(pcon$dbcon, 'ops$bthe."reitmapping"') |> pax_tbl(pcon = pcon)
}

# TODO: Generation script https://gitlab.hafogvatn.is/dag/00-setup/-/blob/master/SI_db_setup/01b-dbsetup.R
pax_dat_strata_stations.hafropax <- function(pcon) {
  mar::tbl_mar(pcon$dbcon, 'biota.strata_stations') |> pax::pax_tbl(pcon = pcon)
}

pax_dat_strata_attributes.hafropax <- function(pcon) {
  mar::tbl_mar(pcon$dbcon, 'biota.strata_attributes') |>
    pax::pax_tbl(pcon = pcon)
}

pax_dat_strata_areas.hafropax <- function(pcon) {
  mar::tbl_mar(pcon$dbcon, 'ops$bthe."strata_areas"') |>
    pax::pax_tbl(pcon = pcon)
}
