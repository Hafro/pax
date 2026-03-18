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

test_db_path <- "/tmp/test-maturity_key.duckdb"
if (!file.exists(test_db_path)) {
  pcon <- pax::pax_connect(test_db_path)
  pax_import(pcon, pax_marmap_ocean_depth())
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

ok_group("assessment_model/00-setup/input_data.R:maturity_key", {
  df_tidypax <- suppressWarnings(
    tidypax::si_stations(mar) |>
      dplyr::filter(sampling_type == 30) |>
      dplyr::inner_join(
        mar::les_maelingu(mar) |>
          dplyr::filter(
            maeling_teg == 'OTOL',
            tegund_nr == local(import_defs$species),
            !is.na(aldur),
            !is.na(kynthroski_nr)
          ) |>
          dplyr::mutate(mat = ifelse(kynthroski_nr == 1, 0, 1)) |>
          dplyr::rename(sample_id = synis_id, length = lengd, age = aldur)
      ) |>
      tidypax::add_lgroups(lgroups = seq(0, 200, 5)) |>
      tidypax:::add_regions(
        regions = list(
          S = c(101, 107, 106, 108, 109, 114),
          N = c(102, 103, 104, 105, 111, 113)
        )
      ) |>
      dplyr::mutate(region = coalesce(region, 'S')) |>
      dplyr::filter(
        year >= local(import_defs$year_start),
        year <= local(import_defs$year_end)
      ) |>
      dplyr::group_by(year, lgroup, age, region) |>
      dplyr::summarise(p = mean(mat, na.rm = TRUE)) |>
      dplyr::arrange(year, region, age, lgroup) |>
      dplyr::collect()
  )
  df_newpax <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type == 30) |>
    dplyr::inner_join(
      dplyr::tbl(pcon, "measurement") |>
        dplyr::filter(
          measurement_type == "OTOL",
          !is.na(age),
          !is.na(maturity_stage)
        ) |>
        dplyr::mutate(mat = ifelse(maturity_stage == 1, 0, 1))
    ) |>
    pax_add_lgroups(lgroups = seq(0, 200, 5)) |>
    pax_add_regions(
      regions = list(
        S = c(101, 107, 106, 108, 109, 114),
        N = c(102, 103, 104, 105, 111, 113),
        S = NULL
      )
    ) |>
    dplyr::group_by(year, lgroup, age, region) |>
    dplyr::summarise(p = mean(mat, na.rm = TRUE)) |>
    dplyr::arrange(year, region, age, lgroup) |>
    dplyr::collect()
  ok(
    ut_cmp_equal(
      df_tidypax |> dplyr::filter(year %in% 1990:1994),
      df_newpax |> dplyr::filter(year %in% 1990:1994)
    ),
    "data frames match"
  )
})
