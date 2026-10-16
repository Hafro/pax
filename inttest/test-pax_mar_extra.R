#!/usr/bin/env Rscript
# The optional tables of pax_from_mar() (needs a connection to mar). The
# comparisons with the queries of the stock repositories are in the no-mar
# migration check scripts; here the shape of the tables
library(unittest)

library(pax)

if (!exists("mar")) {
  mar <- mar::connect_mar()
}

has_cols <- function(tbl, cols) {
  all(cols %in% colnames(tbl))
}

ok_group("pax_from_mar_extra_tables", {
  ok(
    ut_cmp_equal(
      sort(pax_from_mar_extra_tables()),
      sort(c(
        "landings_vessel", "vessel", "logbook_release", "research_landings",
        "catch_disposition", "landings_old", "station_skipped", "sample",
        "logbook_old", "station_env", "temperature_logger", "stomach",
        "stomach_prey", "stomach_prey_length", "survey_catch", "taxonomy"
      ))
    ),
    "All optional tables listed"
  )
})

ok_group("pax_mar_landings_vessel", {
  lv <- pax_mar_landings_vessel(mar, 21, year_start = 1990, year_end = 1994)
  ok(ut_cmp_identical(attr(lv, "pax_name"), "landings_vessel"), "Table name")
  ok(
    has_cols(lv, c("source", "year", "month", "species", "vessel_id", "gear_id",
      "fishing_area", "period", "fishing_year", "catch")),
    "Columns"
  )
  ok(all(lv$species == 21), "Only the species")
  ok(all(lv$year >= 1990 & lv$year <= 1994), "Only the years")
  ok(ut_cmp_equal(sort(unique(lv$source)), c("fiskifelag", "lods")), "Both sources")
  ok(all(lv$fishing_year[lv$year < 1991] == as.character(lv$year[lv$year < 1991])), "Calendar years before 1991")
})

ok_group("pax_mar_vessel", {
  v <- pax_mar_vessel(mar)
  ok(!anyDuplicated(v$vessel_id), "One row per vessel")
  ok(!any(c("identity_no", "address", "fishery_name") %in% colnames(v)), "No owner details")
})

ok_group("pax_mar_station: only_trips", {
  st <- pax_mar_station(mar, sampling_type = c(1, 2, 8), year_start = 2022, year_end = 2022,
    skip_trips = NULL, only_trips = c("MAG%", "MO%")) |> dplyr::collect()
  ok(nrow(st) > 0, "Stations of the stomach-sampling trips")
  ok(all(grepl("^(MAG|MO)", st$trip)), "Only those trips")
})

ok_group("pax_mar_sample", {
  s <- pax_mar_sample(mar, 21) |> dplyr::filter(year == 1980) |> dplyr::collect()
  ok(nrow(s) > 0, "Samples before 1985")
  ok(has_cols(s, c("sample_id", "year", "sampling_type", "trip", "reitur", "smareitur", "gridcell")), "Columns")
})

ok_group("pax_mar_logbook_release", {
  r <- pax_mar_logbook_release(mar, 21, medafli_species = 2021)
  ok(ut_cmp_equal(sort(unique(r$source)), c("adb", "medafli")), "Both sources")
  ok(any(r$condition %in% "RELE"), "Released fish")
})

ok_group("pax_mar_survey tables", {
  env <- pax_mar_station_env(mar, sampling_type = 30, year_start = 2024, year_end = 2024)
  ok(ut_cmp_identical(attr(env, "pax_name"), "station_env"), "station_env: name")
  env <- dplyr::collect(env)
  ok(nrow(env) > 500 && !anyDuplicated(env$sample_id), "station_env: one row per spring survey sample")
  ok(mean(!is.na(env$bottom_temp)) > 0.9, "station_env: bottom temperature recorded")
  ok(all(!is.na(env$rectangle)), "station_env: rectangle")

  tl <- pax_mar_temperature_logger(mar, sampling_type = 30, year_start = 2024, year_end = 2024) |> dplyr::collect()
  ok(nrow(tl) > 0 && has_cols(tl, c("sample_id", "time", "temperature", "depth")), "temperature_logger: readings")
  ok(is.character(tl$sample_id) && all(tl$sample_id %in% env$sample_id), "temperature_logger: sample_id as the station table")

  sto <- pax_mar_stomach(mar, species = 1, sampling_type = 30, year_start = 2024, year_end = 2024) |> dplyr::collect()
  ok(nrow(sto) > 0 && all(sto$species == 1), "stomach: cod predators only")
  prey <- pax_mar_stomach_prey(mar, species = 1, sampling_type = 30, year_start = 2024, year_end = 2024) |> dplyr::collect()
  ok(all(prey$stomach_id %in% sto$stomach_id), "stomach_prey: prey of those stomachs")

  sc <- pax_mar_survey_catch(mar, sampling_type = 30, year_start = 2024, year_end = 2024) |> dplyr::collect()
  ok(length(unique(sc$species)) > 50, "survey_catch: all species")
  ok(!anyDuplicated(sc[c("sample_id", "species")]), "survey_catch: one row per sample and species")

  tx <- pax_mar_taxonomy(mar)
  ok(!anyDuplicated(tx$species) && 1 %in% tx$species, "taxonomy: one row per species")
})

ok_group("pax_mar_fingerprint", {
  f <- pax_mar_fingerprint(mar, sampling_type = c(30, 35), year_start = 2020, year_end = 2022)
  ok(ut_cmp_equal(nrow(f), 6L), "One row per survey and year")
  ok(all(f$n_stations > 100) && all(f$n_aged > 0), "Stations and aged fish counted")
})
