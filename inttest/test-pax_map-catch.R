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

test_db_path <- "/tmp/test-pax_map-catch.duckdb"
if (!file.exists(test_db_path)) {
  pcon <- pax::pax_from_mar(
    species = import_defs$species,
    year_start = import_defs$year_start,
    year_end = import_defs$year_end,
    dbdir = test_db_path
  )
} else {
  pcon <- pax::pax_connect(test_db_path)
}

catch_by_location <-
  dplyr::tbl(pcon, "logbook") |>
  dplyr::filter(year > 1989) |>
  dplyr::group_by(year, lat, lon) |>
  dplyr::summarise(
    catch = sum(1e-3 * catch / tow_area, na.rm = TRUE),
    tow_time = sum(tow_time / tow_area, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::collect(n = Inf)

pax::pax_map_base() |>
  pax::pax_map_layer_depth(dplyr::tbl(pcon, "ocean_depth")) |>
  pax::pax_map_layer_catch(
    catch_by_location |> dplyr::filter(year > (import_defs$year_end - 22)),
    alpha = 1,
    na.fill = -50,
    breaks = c(0, 1, 2, seq(3, 20, by = 3), 40, 60)
  )
