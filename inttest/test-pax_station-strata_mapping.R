#!/usr/bin/env Rscript
# Test converted versions of queries from 02-had
library(unittest)

library(pax)

if (interactive()) {
  options(width = 10000)
}

if (!exists("mar")) {
  mar <- mar::connect_mar()
}

import_defs <- list(
  species = 2,
  year_start = 1990,
  year_end = 1994
)

test_db_path <- "/tmp/test-pax_station-strata_mapping.duckdb"
if (!file.exists(test_db_path)) {
  pcon <- pax::pax_connect(test_db_path)
  pax_import(pcon, pax_marmap_ocean_depth())
  for (s in pax_def_strata_list()) {
    pax_import(pcon, pax_def_strata(s))
  }
  pax_import(pcon, pax_mar_ldist(mar, species = import_defs$species))
  pax_import(
    pcon,
    pax_mar_station(
      mar,
      species = import_defs$species,
      year_start = import_defs$year_start,
      year_end = import_defs$year_end,
      sampling_type = c(1, 2, 8, 10, 11, 30, 35)
    )
  )
  pax_import(
    pcon,
    pax_mar_lw_coeffs(mar, species = import_defs$species)
  )
} else {
  pcon <- pax::pax_connect(test_db_path)
}

ok_group("R/R/06-surveyplots.R:survey index by area", {
  # Do the si by_strata query, and extract the station/stratum mapping from it
  df_newpax_strata <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(
      sampling_type == 30 & coalesce(tow_number, 0) %in% 0:35
    ) |>
    pax_si_by_length() |>
    pax_si_scale_by_strata(strata_tbl = "new_strata_spring") |>
    dplyr::group_by(station, stratum) |>
    #dplyr::summarise(
    #  h3_cell = to_hex(min(list_first(h3_cells))),
    #  begin_lat = min(begin_lat),
    #  begin_lon = min(begin_lon)
    #) |>
    #dplyr::filter(!is.na(h3_cell)) |>
    as.data.frame()

  # Get station mapping from tidypax
  df_tidypax_strata <- tidypax::si_strata_stations(mar) |>
    dplyr::filter(stratification == "new_strata", synaflokkur == 30) |>
    dplyr::select(station, stratum)

  # TODO: In an ideal world, these match, but there's a mismatch between what we assign & strata_stations:
  # https://github.com/Hafro/haftaf/issues/12
  #mapview::mapview(
  #  layer.name = "strata",
  #  pax::pax_def_strata('new_strata_spring') |>
  #    dplyr::mutate(stratum = as.character(stratum)),
  #  zcol = "stratum",
  #  legend = FALSE,
  #  col.regions = rainbow(60)
  #) + mapview::mapview(
  #  df_strata_comparision |> dplyr::filter(!match),
  #  xcol = "begin_lon", ycol = "begin_lat", zcol = "stratum.newpax", crs = 4326
  #) + mapview::mapview(
  #  h3jsr::cell_to_polygon(df_strata_comparision[!df_strata_comparision$match, "h3_cell", drop = FALSE]),
  #  layer.name = "h3_cell"
  #)
  df_strata_comparision <- df_newpax_strata |>
    dplyr::left_join(
      df_tidypax_strata,
      by = c("station"),
      suffix = c(".newpax", ".tidypax"),
      copy = TRUE
    ) |>
    dplyr::filter(!is.na(stratum.newpax)) |>
    dplyr::mutate(match = stratum.newpax == stratum.tidypax) |>
    dplyr::mutate(match = ifelse(is.na(match), FALSE, match)) |>
    dplyr::distinct(stratum.newpax, stratum.tidypax, .keep_all = TRUE) |>
    dplyr::arrange(stratum.newpax, stratum.tidypax)
  ok(
    sum(df_strata_comparision$match) >= 24,
    "At least 24 stations have the same stratum assigned"
  )
})
