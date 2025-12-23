# Routine used to extract strata from mar
export_mar_strata <- function(strata_name) {
  shp_path <- paste0("pax/inst/extdata/strata_", strata_name, ".shp")
  if (file.exists(shp_path)) {
    unlink(gsub(".shp$", ".*", shp_path))
  }
  # NB: area is (in theory) the area of the stratum. "rall_area" ~ "surveyable_area",
  #     i.e. the area in the stratum deep enough for the survey to reach.
  mar::tbl_mar(mar, 'ops$bthe."strata_areas"') |>
    dplyr::filter(stratification == local(strata_name)) |>
    dplyr::select(-stratification) |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    dplyr::arrange(stratum, order) |>
    dplyr::collect() |>
    dplyr::mutate(stratum = as.integer(stratum), order = as.integer(order)) |>
    dplyr::group_by(stratum) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    dplyr::group_by(stratum) |>
    dplyr::arrange(stratum, order) |>
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
# for (s in c("old_strata", "new_strata", "ghl_strata")) export_mar_strata(s)

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

# Was: tidypax::si_add_strata
pax_add_strata <- function(
  tbl,
  strata_tbl = "new_strata",
  area_col = "rall_area"
) {
  pcon <- dbplyr::remote_con(tbl)
  if (is.character(strata_tbl)) {
    strata_tbl <- dplyr::tbl(pcon, strata_tbl)
  }
  tbl_colnames <- pax_tbl_colnames(tbl)
  strata_tbl_colnames <- pax_tbl_colnames(strata_tbl)

  if ("h3_cells" %in% tbl_colnames && "h3_cells" %in% strata_tbl_colnames) {
    out <- tbl |>
      # First join to a de-duplicated map of h3_cell -> stratum ID
      dplyr::mutate(h3_cell = list_first(h3_cells)) |>
      dplyr::left_join(
        strata_tbl |>
          dplyr::group_by(h3_cell = sql("UNNEST(h3_cells)")) |>
          dplyr::summarize(stratum = min(stratum)),
        by = c("h3_cell")
      )
  } else {
    stop(
      "Cannot join tables, expected columns missing (tbl: ",
      paste(tbl_colnames, collapse = ","),
      ", strata_tbl: ",
      paste(tbl_colnames, collapse = ","),
      ")"
    )
  }

  # ..then join again to pull in metadata from strata_tbl
  # TODO: Instead of area_col ideally we...
  #     * Calculate area from stratum shape
  #     * Use depth cut-off instead of rall_area
  out |>
    dplyr::left_join(
      strata_tbl |>
        dplyr::mutate(
          area = coalesce(!!as.symbol(area_col), 0) / 1.852^2
        ) |>
        dplyr::select(-geom, -h3_cells, -rall_area),
      by = c("stratum")
    ) |>
    dplyr::mutate(
      unadjusted_N = N,
      unadjusted_B = B,
      N = area * N / dplyr::n_distinct(sample_id),
      B = area * B / dplyr::n_distinct(sample_id)
    )
}
