pax_dat_noaa_bathymetry <- function(pcon) UseMethod("pax_dat_noaa_bathymetry")
pax_dat_lw_coeffs <- function(pcon) UseMethod("pax_dat_lw_coeffs")
pax_dat_reitmapping <- function(pcon) UseMethod("pax_dat_reitmapping")

# Was: tidypax::add_regions (data)
pax_dat_reitmapping.pax <- function(pcon) {
  # TODO: reitmapping *data* is ~iceland-specific, but not hafropax-specific
  # TODO: Generation: https://gitlab.hafogvatn.is/dag/00-setup/-/blob/master/SI_db_setup/01b-dbsetup.R#L440
  mar::tbl_mar(pcon$dbcon, 'ops$bthe."reitmapping"') |> pax_tbl(pcon = pcon)
}
