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

test_db_path <- "/tmp/test-pax_sampling.duckdb"
if (!file.exists(test_db_path)) {
  pcon <- pax::pax_connect(test_db_path)
  pax_import(pcon, pax_marmap_ocean_depth())
  pax_import(
    pcon,
    pax_mar_sampling(
      mar,
      species = import_defs$species,
      year_start = import_defs$year_start,
      year_end = import_defs$year_end
    )
  )
  pax_import(
    pcon,
    pax_mar_measurement(
      mar,
      species = import_defs$species,
      year_start = import_defs$year_start,
      year_end = import_defs$year_end
    )
  )
} else {
  pcon <- pax::pax_connect(test_db_path)
}

ok_group("R/01-plots_and_tables.R:sampling_position", {
  df_tidypax <- tidypax:::sampling_position(
    mar,
    species_nr = import_defs$species,
    year_range = import_defs$year_start:import_defs$year_end
  ) |>
    dplyr::arrange(lat, lon, year, mfdb_gear_code) |>
    as.data.frame()
  df_newpax <- dplyr::tbl(pcon, "sampling") |>
    dplyr::left_join(dplyr::tbl(pcon, "measurement"), by = "sample_id") |>
    pax_sampling_position_summary() |>
    dplyr::arrange(lat, lon, year, mfdb_gear_code) |>
    as.data.frame()
  ok(
    ut_cmp_equal(
      df_tidypax |> dplyr::filter(year %in% 1990:1994),
      df_newpax |> dplyr::filter(year %in% 1990:1994)
    ),
    "data frames match"
  )
})

ok_group("R/01-plots_and_tables.R:sampling_tables", {
  import_defs$species <- 2
  df_tidypax <- tidypax:::sampling_tables(
    mar,
    species_nr = import_defs$species
  ) |>
    dplyr::filter(Year %in% 1990:1994) |>
    dplyr::collect() |>
    dplyr::rename(year = Year)
  df_newpax <- dplyr::tbl(pcon, "sampling") |>
    dplyr::left_join(dplyr::tbl(pcon, "measurement"), by = "sample_id") |>
    pax_sampling_detail() |>
    dplyr::relocate("year") |>
    dplyr::collect()
  ok(
    ut_cmp_equal(
      df_tidypax |> dplyr::filter(year %in% 1990:1994),
      df_newpax |> dplyr::filter(year %in% 1990:1994)
    ),
    "data frames match"
  )
})
