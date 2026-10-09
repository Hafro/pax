if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

# A small pax database: two surveys, two strata, three years
pcon <- pax_connect(":memory:")
station <- expand.grid(
  sampling_type = c(30, 35),
  year = 2020:2022,
  station = 1:4
)
station$sample_id <- seq_len(nrow(station))
station$month <- ifelse(station$sampling_type == 30, 3, 10)
station$gridcell <- 1000 + station$station
station$tow_depth <- 100
station$tow_length <- 4
station$tow_number <- station$station
station$gear_id <- ifelse(station$station == 4, 99, 73)
station$fixed <- ifelse(station$station == 3, 0, 1)
pax_import(pcon, station, name = "station")

# One fish of each length per station; station 2 has no fish over 40 cm in
# 2021
lengths <- c(20, 35, 50)
ldist <- merge(station[, c("sample_id", "year", "station")], data.frame(length = lengths))
ldist <- ldist[!(ldist$year == 2021 & ldist$station == 2 & ldist$length > 40), ]
ldist$species <- 1
ldist$sex <- NA_real_
ldist$count <- 1
pax_import(pcon, ldist[, c("sample_id", "species", "length", "sex", "count")], name = "ldist")
pax_import(pcon, data.frame(species = 1, a = 0.01, b = 3), name = "lw_coeffs")

pax_import(
  pcon,
  data.frame(
    stratification = c(rep("new_strata", 8), "old_strata"),
    sampling_type = c(rep(30, 4), rep(35, 4), 30),
    station = c(1:4, 1:4, 1),
    stratum = c(1, 1, 2, 2, 1, 1, 2, 2, 9)
  ),
  name = "strata_stations"
)
pax_import(pcon, data.frame(stratum = 1:2, rall_area = c(1.852^2 * 100, 1.852^2 * 50)), name = "new_strata_spring")
pax_import(pcon, data.frame(stratum = 1:2, rall_area = c(1.852^2 * 30, NA)), name = "new_strata_autumn")

ok_group("pax_si_strata_stations", {
  ss <- pax_si_strata_stations(pcon)
  ok(ut_cmp_equal(nrow(ss), 8L), "Only the stations of the stratification")
  ok(
    ut_cmp_equal(
      ss$area[ss$sampling_type == 30 & ss$stratum == 2][[1]],
      50
    ),
    "Spring survey areas from new_strata_spring, in square nautical miles"
  )
  ok(
    ut_cmp_equal(ss$area[ss$sampling_type == 35 & ss$stratum == 1][[1]], 30),
    "Autumn survey areas from new_strata_autumn"
  )
  ok(
    ut_cmp_equal(ss$area[ss$sampling_type == 35 & ss$stratum == 2][[1]], 0),
    "No area: 0"
  )
})

ss <- pax_si_strata_stations(pcon)

# The steps of the old stock wrappers, by hand
by_hand <- function(st, sampling_type, lr) {
  st |>
    pax_si_by_length() |>
    pax_si_scale_by_strata_stations(
      ss[ss$sampling_type == sampling_type, c("station", "stratum", "area")]
    ) |>
    pax_si_strata_summary(length_range = lr) |>
    pax_si_year_summary() |>
    dplyr::collect() |>
    dplyr::ungroup() |>
    dplyr::arrange(year)
}

ok_group("pax_si_length_index: same as the steps by hand", {
  out <- pax_si_length_index(
    pcon,
    ss,
    length_ranges = list(total = c(1, 500), big = c(40, 500)),
    sampling_type = 30,
    tow_number = 1:3
  )
  ok(ut_cmp_identical(unique(out$length_range), c("big", "total")), "Ordered by length range")
  ok(
    ut_cmp_identical(
      colnames(out),
      c("length_range", "year", "srN", "n", "n_cv", "b", "b_cv")
    ),
    "Columns"
  )
  st <- dplyr::tbl(pcon, "station") |>
    dplyr::filter(sampling_type == 30, station %in% 1:3)
  ref <- by_hand(st, 30, c(1, 500))
  tot <- out[out$length_range == "total", ]
  ok(ut_cmp_equal(tot$b, ref$si_biomass), "Biomass")
  ok(ut_cmp_equal(tot$n_cv, ref$si_abund_cv), "Abundance CV")
  # Stratum 1: two stations, 100 nm2; stratum 2: one station (tow 3), 50 nm2.
  # Each station 3 fish in 4 nm x 17 m: 3 / (4 * 17 / 1852) per nm2
  per_nm2 <- 3 / (4 * 17 / 1852) / 1e3
  ok(ut_cmp_equal(tot$n[1], per_nm2 * (100 + 50)), "Abundance (thousands) of a full year")
})

ok_group("pax_si_length_index: station filters", {
  idx <- function(...) {
    pax_si_length_index(
      dplyr::tbl(pcon, "station"),
      ss,
      length_ranges = list(total = c(1, 500)),
      ...
    )
  }
  all_st <- idx(sampling_type = 35)
  ok(ut_cmp_equal(all_st$year, 2020:2022), "All years")
  ok(
    ut_cmp_equal(idx(sampling_type = 35, skip_years = 2021)$year, c(2020, 2022)),
    "skip_years"
  )
  st <- dplyr::tbl(pcon, "station") |> dplyr::filter(sampling_type == 35)
  ok(
    ut_cmp_equal(
      idx(sampling_type = 35, gear_id = 73)$b,
      by_hand(st |> dplyr::filter(gear_id == 73), 35, c(1, 500))$si_biomass
    ),
    "gear_id"
  )
  ok(
    ut_cmp_equal(
      idx(sampling_type = 35, fixed_only = TRUE)$b,
      by_hand(st |> dplyr::filter(fixed == 1), 35, c(1, 500))$si_biomass
    ),
    "fixed_only"
  )
  ok(
    ut_cmp_equal(
      idx(sampling_type = 35, tow_number = c(1, 2))$b,
      by_hand(st |> dplyr::filter(tow_number %in% c(1, 2)), 35, c(1, 500))$si_biomass
    ),
    "tow_number"
  )
  ok(
    !isTRUE(all.equal(idx(sampling_type = 35)$b, idx(sampling_type = 30)$b)),
    "Each survey has its own stratum areas"
  )
})

ok_group("pax_si_length_index: years without fish in the range", {
  # Only station 2 in 2021 has no fish over 40 cm: with stations 1-2 only,
  # 2021 has none
  args <- list(
    pcon,
    ss,
    length_ranges = list(big = c(40, 500)),
    sampling_type = 30,
    tow_number = 2
  )
  out <- do.call(pax_si_length_index, args)
  ok(ut_cmp_equal(out$year, c(2020, 2022)), "No row for 2021 by default")
  out <- do.call(pax_si_length_index, c(args, complete_years = TRUE))
  ok(ut_cmp_equal(out$year, 2020:2022), "complete_years: a row for 2021")
  ok(ut_cmp_equal(out$b[out$year == 2021], 0), "with an index of 0")
  ok(ut_cmp_equal(out$b_cv[out$year == 2021], NA_real_), "and CV NA")
})

ok_group("pax_si_length_index: arguments", {
  ok(
    ut_cmp_error(
      pax_si_length_index(pcon, ss, list(c(1, 500)), sampling_type = 30),
      "named list"
    ),
    "length_ranges must be named"
  )
  ok(
    ut_cmp_error(
      pax_si_by_strata(pcon, ss[, c("station", "stratum")], sampling_type = 30),
      "area"
    ),
    "strata_stations needs areas"
  )
})
