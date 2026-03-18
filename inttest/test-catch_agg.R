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

test_db_path <- "/tmp/test-catch_agg.duckdb"
if (!file.exists(test_db_path)) {
  pcon <- pax::pax_connect(test_db_path)
  pax_import(pcon, pax_marmap_ocean_depth())
  pax_import(
    pcon,
    pax_mar_logbook(
      mar,
      species = import_defs$species,
      year_start = import_defs$year_start,
      year_end = import_defs$year_end
    )
  )
} else {
  pcon <- pax::pax_connect(test_db_path)
}

ok_group("R/01-plots_and_tables.R:catch_agg", {
  df_tidypax <- suppressWarnings(
    tidypax::catch_data(mar, import_defs$species) |>
      dplyr::filter(
        year >= local(import_defs$year_start),
        year <= local(import_defs$year_end)
      ) |>
      tidypax::add_depth_labels(breaks = c(0, 100, 200, 300)) |>
      tidypax::add_regions(
        regions = list(
          W = 101,
          NW = 102,
          NE = c(103, 104, 105),
          SE = c(107, 106),
          SW = 108
        )
      ) |>
      dplyr::mutate(region = coalesce(region, 'Other')) |>
      dplyr::group_by(year, mfdb_gear_code, region, depth_class) |>
      dplyr::summarise(c = sum(catch, na.rm = TRUE) / 1e6) |>
      dplyr::ungroup() |>
      dplyr::arrange(year, mfdb_gear_code, region, depth_class) |>
      dplyr::rename(ocean_depth_class = depth_class) |>
      dplyr::collect() |>
      as.data.frame()
  )
  df_newpax <- dplyr::tbl(pcon, "logbook") |>
    pax_add_ocean_depth_class(breaks = c(0, 100, 200, 300)) |>
    pax_add_regions(
      regions = list(
        W = 101,
        NW = 102,
        NE = c(103, 104, 105),
        SE = c(107, 106),
        SW = 108,
        Other = NULL
      )
    ) |>
    dplyr::group_by(year, mfdb_gear_code, region, ocean_depth_class) |>
    dplyr::summarise(c = sum(catch, na.rm = TRUE) / 1e6) |>
    dplyr::ungroup() |>
    dplyr::arrange(year, mfdb_gear_code, region, ocean_depth_class) |>
    dplyr::collect() |>
    as.data.frame()
  # NB: tidypax's noaa_bathymetry is wrong: https://github.com/Hafro/pax/issues/9 so queries don't match
  # > df_newpax |> dplyr::filter(year == 1990, mfdb_gear_code == "DSE", region == "NE")
  # > df_tidypax |> dplyr::filter(year == 1990, mfdb_gear_code == "DSE", region == "NE")
  ok(
    ut_cmp_equal(
      df_tidypax |>
        dplyr::group_by(year, mfdb_gear_code, region) |>
        dplyr::filter(year %in% 1990:1994) |>
        dplyr::summarise(c = sum(c, na.rm = TRUE)),
      df_newpax |>
        dplyr::group_by(year, mfdb_gear_code, region) |>
        dplyr::filter(year %in% 1990:1994) |>
        dplyr::summarise(c = sum(c, na.rm = TRUE))
    ),
    "data frames match, ignoring ocean_depth_class"
  )
})
