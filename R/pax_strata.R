# Routine used to extract strata from mar
export_mar_strata <- function(strata_name) {
  shp_path <- paste0("pax/inst/extdata/strata_", strata_name, ".shp")
  mar::tbl_mar(mar, 'ops$bthe."strata_areas"') |>
    dplyr::filter(stratification == local(strata_name)) |>
    dplyr::select(-stratification) |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    dplyr::arrange(stratum, order) |>
    dplyr::collect() |>
    dplyr::mutate(stratum = as.integer(stratum)) |>
    dplyr::group_by(stratum) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = "+proj=eqc") |>
    dplyr::group_by(stratum) |>
    dplyr::summarise(geometry = sf::st_combine(geometry)) |>
    sf::st_cast("POLYGON") |>
    dplyr::left_join(
      mar::tbl_mar(mar, 'biota.strata_attributes') |>
        dplyr::filter(stratification == local(strata_name)) |>
        dplyr::select(-stratification) |>
        dplyr::collect() |>
        dplyr::mutate(stratum = as.integer(stratum)),
      by = c("stratum")
    ) |>
    sf::st_make_valid() |>
    sf::st_write(shp_path)
  print(sf::st_is_valid(sf::st_read(shp_path), reason = TRUE))
}

pax_def_crs <- function() {
  return(sf::st_crs(4326))
}

pax_def_strata <- function(strata_name) {
  shp_path <- system.file(
    "extdata",
    paste0("strata_", strata_name, ".shp"),
    package = "pax"
  )
  pax_decorate(
    sf::st_read(shp_path),
    name = strata_name,
    # TODO: Should strata have individual citiations?
    cite = citation("pax")
  )
}
