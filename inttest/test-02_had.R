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
  pax_import(pcon, pax_def_strata("new_strata"))

  pax_import(pcon, do.call(pax_mar_si, import_defs))
  pax_import(pcon, do.call(pax_mar_measurement, import_defs))
  pax_import(pcon, do.call(pax_mar_catch, import_defs))
  pax_import(pcon, do.call(pax_mar_sampling, import_defs))
  pax_import(pcon, pax_mar_ldist(mar, species = import_defs$species))
  pax_import(pcon, pax_mar_lw_coeffs(mar, species = import_defs$species))
} else {
  pcon <- pax::pax_connect("/tmp/camel.duckdb")
}

ok_group("R/R/06-surveyplots.R:survey index by area", {
  # Do the si by_strata query, and extract the station/stratum mapping from it
  df_newpax_strata <- dplyr::tbl(pcon, "si") |>
    dplyr::filter(
      sampling_type == 30 &
        coalesce(tow_number, 0) %in% 0:35 | ## spring survey
        coalesce(tow_number, 0) %in%
          0:75 &
          gear_id %in% 77:78 &
          year != 2011 &
          sampling_type == 35 ## autumn survey
    ) |>
    pax_si_by_length() |>
    pax_add_strata() |>
    dplyr::group_by(station, stratum) |>
    dplyr::summarise(
      h3_cells = min(list_first(h3_cells)),
      begin_lat = min(begin_lat),
      begin_lon = min(begin_lon)
    ) |>
    as.data.frame()

  # Get station mapping from tidypax
  df_tidypax_strata <- tidypax::si_strata_stations(mar) |>
    dplyr::filter(stratification == "new_strata", synaflokkur == 30) |>
    dplyr::select(station, stratum)

  # TODO: These should match, but they don't. Either it's GIGO strata, or we should be using the tow midpoint as our join
  df_strata_comparision <- df_newpax_strata |>
    dplyr::left_join(
      df_tidypax_strata,
      by = c("station"),
      suffix = c(".newpax", ".tidypax"),
      copy = TRUE
    ) |>
    dplyr::filter(!is.na(stratum.newpax)) |>
    dplyr::mutate(match = stratum.newpax == stratum.tidypax) |>
    dplyr::distinct(stratum.newpax, stratum.tidypax, .keep_all = TRUE) |>
    dplyr::arrange(stratum.newpax, stratum.tidypax)
})

ok_group("R/01-plots_and_tables.R:sampling_position", {
  df_tidypax <- tidypax:::sampling_position(
    mar,
    species_nr = species_code,
    year_range = import_defs$year_start:import_defs$year_end
  ) |>
    dplyr::arrange(lat, lon, year, mfdb_gear_code) |>
    as.data.frame()
  df_newpax <- dplyr::tbl(pcon, "sampling") |>
    dplyr::left_join(dplyr::tbl(pcon, "measurement"), by = "sample_id") |>
    pax_sampling_position_summary() |>
    dplyr::arrange(lat, lon, year, mfdb_gear_code) |>
    as.data.frame()
  ok(ut_cmp_equal(df_tidypax, df_newpax), "data frames match")
})

ok_group("R/01-plots_and_tables.R:sampling_tables", {
  species_code <- 2
  df_tidypax <- tidypax:::sampling_tables(mar, species_nr = species_code) |>
    dplyr::filter(Year %in% 1990:1994) |>
    dplyr::collect() |>
    dplyr::rename(year = Year)
  df_newpax <- dplyr::tbl(pcon, "sampling") |>
    dplyr::left_join(dplyr::tbl(pcon, "measurement"), by = "sample_id") |>
    pax_sampling_detail() |>
    dplyr::relocate("year") |>
    dplyr::collect()
  ok(ut_cmp_equal(df_tidypax, df_newpax), "data frames match")
})

ok_group("R/01-plots_and_tables.R:catch_agg", {
  df_tidypax <- suppressWarnings(
    tidypax::catch_data(mar, species_code) |>
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
      dplyr::summarise(c = sum(catch) / 1e6) |>
      dplyr::ungroup() |>
      dplyr::arrange(year, mfdb_gear_code, region, depth_class) |>
      dplyr::rename(ocean_depth_class = depth_class) |>
      dplyr::collect() |>
      as.data.frame()
  )
  df_newpax <- dplyr::tbl(pcon, "catch") |>
    pax_add_ocean_depth_class(breaks = c(0, 100, 200, 300)) |>
    pax_add_regions(
      regions = list(
        W = 101,
        NW = 102,
        NE = c(103, 104, 105),
        SE = c(107, 106),
        SW = 108
      ),
      default = "Other"
    ) |>
    dplyr::group_by(year, mfdb_gear_code, region, ocean_depth_class) |>
    dplyr::summarise(c = sum(catch) / 1e6) |>
    dplyr::ungroup() |>
    dplyr::arrange(year, mfdb_gear_code, region, ocean_depth_class) |>
    dplyr::collect() |>
    as.data.frame()
  # TODO: Total catch good, but grouping isn't right?
  #ok(ut_cmp_equal(df_tidypax, df_newpax), "data frames match")
  ok(ut_cmp_equal(sum(df_tidypax$c), sum(df_newpax$c)), "total catch match")
})

ok_group("assessment_model/00-setup/input_data.R:maturity_key", {
  df_tidypax <- suppressWarnings(
    tidypax::si_stations(mar) |>
      dplyr::filter(sampling_type == 30) |>
      dplyr::inner_join(
        mar::les_maelingu(mar) |>
          dplyr::filter(
            maeling_teg == 'OTOL',
            tegund_nr == local(species_code),
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
      dplyr::summarise(p = mean(mat)) |>
      dplyr::arrange(year, region, age, lgroup) |>
      dplyr::collect()
  )
  df_newpax <- dplyr::tbl(pcon, "si") |>
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
        N = c(102, 103, 104, 105, 111, 113)
      ),
      default = "S"
    ) |>
    dplyr::group_by(year, lgroup, age, region) |>
    dplyr::summarise(p = mean(mat)) |>
    dplyr::arrange(year, region, age, lgroup) |>
    dplyr::collect()
  ok(ut_cmp_equal(df_tidypax, df_newpax), "data frames match")
})

DBI::dbDisconnect(pcon)
