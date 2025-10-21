# TODO: Generation script https://gitlab.hafogvatn.is/dag/00-setup/-/blob/master/SI_db_setup/01b-dbsetup.R
pax_si_strata_stations.hafropax <- function(pcon) {
  mar::tbl_mar(pcon$dbcon, 'biota.strata_stations') |> pax::pax_tbl(pcon = pcon)
}

pax_si_strata_attributes.hafropax <- function(pcon) {
  mar::tbl_mar(pcon$dbcon, 'biota.strata_attributes') |>
    pax::pax_tbl(pcon = pcon)
}

pax_si_strata_areas.hafropax <- function(pcon) {
  mar::tbl_mar(pcon$dbcon, 'ops$bthe."strata_areas"') |>
    pax::pax_tbl(pcon = pcon)
}
