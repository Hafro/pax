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

species_code <- 2
import_defs <- list(
  mar,
  species = species_code,
  year_start = 1990,
  year_end = 1994
)

if (!file.exists("/tmp/camel.duckdb")) {
  pcon <- pax::pax_connect("/tmp/camel.duckdb")
  pax_import(pcon, pax_def_strata("old_strata"))
  pax_import(pcon, pax_def_strata("new_strata_spring"))

  writeLines("=== pax_marmap_ocean_depth")
  pax_import(pcon, pax::pax_marmap_ocean_depth(), overwrite = TRUE)
  writeLines("=== pax_mar_station")
  pax_import(pcon, do.call(pax_mar_station, import_defs))
  writeLines("=== pax_mar_measurement")
  pax_import(pcon, do.call(pax_mar_measurement, import_defs))
  writeLines("=== pax_mar_logbook")
  pax_import(pcon, do.call(pax_mar_logbook, import_defs))
  writeLines("=== pax_mar_landings")
  pax_import(
    pcon,
    pax_mar_landings(
      mar,
      species = import_defs$species,
      ices_area_like = "5a%",
      year_start = import_defs$year_start,
      year_end = import_defs$year_end
    )
  )
  writeLines("=== pax_mar_sampling")
  pax_import(pcon, do.call(pax_mar_sampling, import_defs))
  writeLines("=== pax_mar_aldist")
  pax_import(pcon, pax_mar_aldist(mar, species = import_defs$species))
  writeLines("=== pax_mar_ldist")
  pax_import(pcon, pax_mar_ldist(mar, species = import_defs$species))
  writeLines("=== pax_mar_lw_coeffs")
  pax_import(pcon, pax_mar_lw_coeffs(mar, species = import_defs$species))
} else {
  pcon <- pax::pax_connect("/tmp/camel.duckdb")
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
