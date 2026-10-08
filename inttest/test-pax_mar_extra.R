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
        "logbook_old"
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
