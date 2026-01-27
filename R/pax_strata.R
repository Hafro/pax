# Routine used to extract strata from mar
export_mar_strata <- function(mar, strata_name, substratification = "default") {
  shp_path <- paste0(
    "pax/inst/extdata/strata_",
    strata_name,
    (if (substratification == "default") {
      ""
    } else {
      paste0("_", substratification)
    }),
    ".shp"
  )
  if (file.exists(shp_path)) {
    unlink(gsub(".shp$", ".*", shp_path))
  }

  # Pregenerate substring to match substratification
  substratification_match <- paste0(":", substratification, ":")

  # NB: area is (in theory) the area of the stratum. "rall_area" ~ "surveyable_area",
  #     i.e. the area in the stratum deep enough for the survey to reach.
  mar::tbl_mar(mar, 'ops$bthe."strata_areas"') |>
    dplyr::filter(stratification == local(strata_name)) |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    dplyr::collect() |>
    dplyr::mutate(stratum = as.integer(stratum), order = as.integer(order)) |> # NB: This won't work in dbplyr

    # Assign substratification, either throwing away unused stratum or breaking up into separate non-overlapping sets
    dplyr::mutate(
      substratification = dplyr::case_when(
        # Remove overlapping strata not mentioned in strata_stations
        # Will likely be sets of equivalent stratum here too, but not searched for
        stratification == "old_strata" &
          stratum %in% c(24:25, 30:32, 37L, 52L, 59L, 69L, 70:78, 86L, 94L) ~
          ":unused:",
        # Sets of equivalent stratum in new_strata, any substratification needs to choose one:
        # 45 <--> 22, 23, 28
        # 42, 43, 44 <--> 4, 7, 9, 12
        # 13 <--> 14, 15 <--> 15, 36, 37 <--> 36, 38
        # 21, 28 <--> 45
        # 18 <--> 29, 30
        stratification == "new_strata" & stratum %in% c(42, 43, 44, 15, 37) ~
          ":spring:",
        stratification == "new_strata" & stratum %in% c(4, 7, 9, 12, 38) ~
          ":autumn:",
        # No substratification that uses these
        stratification == "new_strata" &
          stratum %in% c(22, 23, 28, 13, 14, 21, 28, 29, 30) ~
          ":unused:",
        # Assume every other stratum is common
        stratification == "new_strata" ~ ":spring:autumn:",
        TRUE ~ ":default:",
      )
    ) |>

    # Assign substratum, either breaking up multiple polygons or binning nonsense points
    dplyr::mutate(
      substratum = dplyr::case_when(
        # Remove nonsense points
        stratification == "old_strata" & stratum == 67 & order > 11775 ~ NA,
        # Break up multipolygons
        stratification == "old_strata" & stratum == 54 & order < 9329 ~ 1,
        stratification == "old_strata" & stratum == 54 & order < 9368 ~ 2,
        stratification == "old_strata" & stratum == 54 ~ 3,
        # Break up multipolygons
        stratification == "old_strata" & stratum == 58 & order < 9810 ~ 1,
        stratification == "old_strata" & stratum == 58 & order < 9881 ~ 2,
        stratification == "old_strata" & stratum == 58 ~ 3,
        # Break up multipolygons
        stratification == "old_strata" & stratum == 89 & order < 14242 ~ 1,
        stratification == "old_strata" & stratum == 89 & order < 14257 ~ 2,
        stratification == "old_strata" & stratum == 89 & order < 14285 ~ 3,
        stratification == "old_strata" & stratum == 89 & order < 14291 ~ 4,
        stratification == "old_strata" & stratum == 89 & order < 14303 ~ 5,
        stratification == "old_strata" & stratum == 89 & order < 14337 ~ 6,
        stratification == "old_strata" & stratum == 89 ~ 7,
        # Break up multipolygons
        stratification == "new_strata" &
          stratum == 41 &
          order %in% 65697:65736 ~
          1,
        stratification == "new_strata" &
          stratum == 41 &
          order %in% 65737:65762 ~
          2,
        stratification == "new_strata" &
          stratum == 41 &
          order %in% 65764:65811 ~
          3,
        stratification == "new_strata" &
          stratum == 41 &
          order %in% 65813:65837 ~
          4,
        stratification == "new_strata" &
          stratum == 41 &
          order %in% 65838:65856 ~
          5,
        # Remove nodes attempting to join the polygons
        stratification == "new_strata" & stratum == 41 ~ NA,
        # Remove lump
        stratification == "new_strata" & stratum == 9 & (order %in% 7594:7623) ~
          NA,
        # Remove part of stratum 9
        stratification == "new_strata" &
          stratum == 12 &
          (order %in% 13379:13400) ~
          NA,
        # Remove duplicated corner
        stratification == "new_strata" &
          stratum == 19 &
          (order %in% 29957:29962) ~
          NA,
        stratification == "new_strata" & stratum == 19 & order == 29092 ~ NA,
        # Remove overlap corner (should be replacing it with a point further northwest)
        stratification == "new_strata" & stratum == 45 & (order %in% 71842) ~
          NA,
        TRUE ~ 1,
      )
    ) |>

    # Remove rows filtered by the above
    dplyr::filter(is.finite(substratum)) |>
    dplyr::filter(grepl(
      local(substratification_match),
      substratification,
      fixed = TRUE
    )) |>
    dplyr::select(-stratification, -substratification) |>

    # Convert data.frame into geometry of points
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    # Combine points into substratum polygon
    dplyr::arrange(stratum, substratum, order) |>
    dplyr::group_by(stratum, substratum) |>
    dplyr::summarise(geometry = sf::st_combine(geometry)) |>
    sf::st_cast("POLYGON") |>
    # Combine substratum polygon to stratum multipolygon
    dplyr::group_by(stratum) |>
    dplyr::summarise(geometry = sf::st_combine(geometry)) |>
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
  out <- sf::st_read(shp_path)
  print(sf::st_is_valid(out, reason = TRUE))
  invisible(out)
}
if (FALSE) {
  pax:::export_mar_strata(mar, "old_strata")
  pax:::export_mar_strata(mar, "new_strata", "spring")
  pax:::export_mar_strata(mar, "new_strata", "autumn")
  pax:::export_mar_strata(mar, "ghl_strata")
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
  tbl_colnames <- colnames(tbl)
  strata_tbl_colnames <- colnames(strata_tbl)

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
      si_abund = area * si_abund / dplyr::n_distinct(sample_id),
      si_biomass = area * si_biomass / dplyr::n_distinct(sample_id)
    )
}
