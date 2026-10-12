if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

pcon <- pax_connect(":memory:")

# A toy inshore shrimp survey: two years, two sub-areas (plus one station in
# a sub-area without an area), three stations per sub-area
station <- expand.grid(year = 2020:2021, survey_subarea = c(1, 2), k = 1:3)
station$sample_id <- seq_len(nrow(station))
station$survey_area <- 52
station$sampling_type <- 37
station$gear_id <- 14
station$tow_length <- c(1, 2, 1.5, 0.005, 2, 1, 1, 2, 1, 1, 2, 1)
station <- rbind(
  station,
  data.frame(
    year = 2020, survey_subarea = 7, k = 1, sample_id = 13, survey_area = 52,
    sampling_type = 37, gear_id = 14, tow_length = 1
  )
)
pax_import(pcon, station, name = "station")

# Lengths in cm (0.1 mm), sex and maturity stage; station 5 has no shrimp,
# station 6 has shrimp but no sample weight, station 9 an unsexed animal
set.seed(1)
ldist <- do.call(rbind, lapply(setdiff(station$sample_id, 5), function(s) {
  data.frame(
    sample_id = s,
    species = 41,
    length = c(1.21, 1.33, 1.42, 1.5, 1.61, 1.87, 2.24),
    sex = c(1, 1, 1, 2, 2, 2, 2),
    maturity_stage = c(10, NA, 10, 1, 3, 21, 30),
    count = sample(1:20, 7)
  )
}))
ldist$sex[ldist$sample_id == 9][1] <- NA
pax_import(pcon, ldist, name = "ldist_maturity")
sw <- data.frame(
  sample_id = setdiff(station$sample_id, 5),
  species = 41,
  n_measured = NA, n_counted = 0, n_aged = 0,
  sample_weight = runif(12, 0.5, 2),
  catch_weight = c(runif(10, 2, 10), 0, NA)
)
sw$sample_weight[sw$sample_id == 6] <- NA
sw <- sw[sw$sample_id != 7, ] # no sample weight row at all
pax_import(pcon, sw, name = "sample_weight")

strata <- pax_shrimp_strata("arnarfjordur")
lw <- data.frame(
  species = 41,
  sex = c(1, 2, 2, 2, 2, 2, 2, 2, 2, 2),
  maturity_stage = c(NA, 1, 2, 11, 3, 12, 21, 22, 23, 30),
  a = c(rep(0.0005358522, 4), rep(0.0006249431, 6)),
  b = c(rep(3.01352, 4), rep(2.99411, 6))
)
ramp <- data.frame(length = seq(13, 15.5, 0.5), p = c(0.143, 0.286, 0.429, 0.571, 0.714, 0.857))

ok_group("pax_si_station_strata", {
  st <- pax_si_station_strata(dplyr::tbl(pcon, "station"), strata) |>
    dplyr::collect() |>
    dplyr::arrange(sample_id)
  ok(ut_cmp_equal(st$area[1:12], strata$area[match(station$survey_subarea[1:12], strata$survey_subarea)]), "Areas by sub-area")
  ok(ut_cmp_equal(st$area[13], NA_real_), "Sub-area outside the strata: no area")

  yr <- data.frame(survey_area = 52, survey_subarea = c(1, 1, 2), year_start = c(NA, 2021, NA), year_end = c(2020, NA, NA), area = c(10, 20, 30), stratum = c(1, 1, 2))
  st2 <- pax_si_station_strata(dplyr::tbl(pcon, "station"), yr) |> dplyr::collect()
  ok(ut_cmp_equal(sort(unique(st2$area[st2$survey_subarea == 1])), c(10, 20)), "Areas by year")
  ok(ut_cmp_error(
    pax_si_station_strata(dplyr::tbl(pcon, "station"), rbind(yr, yr)),
    "more than one row"
  ), "Overlapping strata rows are an error")
})

ok_group("pax_ldist_add_weight: by, wildcards and missing", {
  ld <- pax:::ut_tbl(pcon, data.frame(
    species = 41, length = c(14, 14, 14, NA), sex = c(1, 2, NA, NA), maturity_stage = c(NA, 21, 1, NA)
  ))
  out <- pax_ldist_add_weight(ld, lw, by = c("species", "sex", "maturity_stage"), missing = "zero") |>
    dplyr::collect()
  ok(ut_cmp_equal(out$weight, c(0.0005358522 * 14^3.01352, 0.0006249431 * 14^2.99411, 0, 0)), "Male of any stage, female by stage, unsexed 0")
  ok(ut_cmp_error(
    pax_ldist_add_weight(ld, lw, by = c("species", "sex", "maturity_stage"), missing = "stop") |> dplyr::collect(),
    "no length-weight coefficients"
  ), "missing = 'stop' stops on the unsexed animal")
  ld2 <- pax:::ut_tbl(pcon, data.frame(species = 2, length = 14, sex = 1))
  ok(ut_cmp_error(pax_ldist_add_weight(ld2, lw, by = c("species", "sex"), missing = "zero"), "species 2"), "No coefficients for the species: error")
  ok(ut_cmp_equal(
    pax_ldist_add_weight(ld2, lw) |> dplyr::pull(weight),
    0.01 * 14^3
  ), "Default unchanged: a = 0.01, b = 3")
})

ok_group("pax_ldist_scale_catch_weight, pax_ldist_scale_counted", {
  ld <- pax:::ut_tbl(pcon, data.frame(sample_id = c(1, 1, 6, 7), species = 41, length = 1, count = c(2, 3, 4, 5)))
  out <- pax_ldist_scale_catch_weight(ld) |> dplyr::collect() |> dplyr::arrange(sample_id, count_unscaled)
  s1 <- sw[sw$sample_id == 1, ]
  ok(ut_cmp_equal(out$count[1:2], c(2, 3) * s1$catch_weight / s1$sample_weight), "Raised to the catch weight")
  ok(ut_cmp_equal(out$mean_weight[1], s1$sample_weight / 5), "Mean weight from the measured sample")
  s6 <- sw[sw$sample_id == 6, ]
  ok(ut_cmp_equal(out$mean_weight[3], 1 / 4), "Missing sample weight taken as 1 kg (mar::skala_med_afla())")
  ok(ut_cmp_equal(out$count[3], 4 * s6$catch_weight), "...and the ratio is the catch weight")
  ok(ut_cmp_equal(c(out$count[4], out$mean_weight[4]), c(NA, 0)), "No sample weight row: count NA, mean weight 0")

  swc <- data.frame(sample_id = 1:2, species = 41, n_measured = c(10, NA), n_counted = c(5, 0))
  ld <- pax:::ut_tbl(pcon, data.frame(sample_id = 1:2, species = 41, length = 1, count = 10))
  out <- pax_ldist_scale_counted(ld, swc) |> dplyr::arrange(sample_id) |> dplyr::pull(count)
  ok(ut_cmp_equal(out, c(15, 10)), "Raised to the counted animals")
})

ok_group("pax_ldist_round_length, pax_ldist_add_selection", {
  ld <- pax:::ut_tbl(pcon, data.frame(length = c(1.21, 1.225, 1.425, 1.33, NA)))
  ok(ut_cmp_equal(
    pax_ldist_round_length(ld) |> dplyr::pull(length),
    c(12, 12.5, 14.5, 13.5, NA)
  ), "cm to mm in 0.5 mm classes, halves up")
  ld <- pax:::ut_tbl(pcon, data.frame(length = c(10, 13, 14.5, 15.5, 16, NA)))
  ok(ut_cmp_equal(
    pax_ldist_add_selection(ld, ramp) |> dplyr::arrange(length) |> dplyr::pull(p_selected),
    c(0, 0.143, 0.571, 0.857, 1, 1)
  ), "Ramp: 0 below, the table within, 1 above and for NA")
  ld <- pax:::ut_tbl(pcon, data.frame(length = 13.25))
  ok(ut_cmp_error(pax_ldist_add_selection(ld, ramp), "not in it"), "A length off the grid is an error")
})

ok_group("pax_si_catch_weight_biomass and pax_si_stratified_index reproduce the old algorithm", {
  st <- pax_si_station_strata(dplyr::tbl(pcon, "station"), strata)
  bio <- pax_si_catch_weight_biomass(st, species = 41, lw_coeffs = lw, selection = ramp) |>
    dplyr::collect()
  idx <- pax_si_stratified_index(bio, c("b", "b_fishable", "b_juvenile", "b_female"))

  # The old algorithm in plain R
  d <- merge(station, ldist, by = "sample_id", all.x = TRUE)
  d$count[is.na(d$count)] <- 0
  d$species <- 41
  d <- merge(d, sw[c("sample_id", "sample_weight", "catch_weight")], by = "sample_id", all.x = TRUE)
  has_row <- d$sample_id %in% sw$sample_id
  sw1 <- ifelse(is.na(d$sample_weight), 1, d$sample_weight)
  r <- ifelse(is.na(d$catch_weight) | d$catch_weight == 0, 1, d$catch_weight / ifelse(sw1 > 0, sw1, 1))
  r[!has_row] <- NA
  nn <- ave(d$count, d$sample_id, FUN = sum)
  mw <- ifelse(has_row, sw1, 0)
  mw <- ifelse(nn > 0, mw / nn, mw)
  cnt <- d$count * r
  l10 <- floor(d$length * 10 / 0.5 + 0.5) * 0.5
  tl <- pmin(pmax(d$tow_length, 0.01), 8)
  area <- strata$area[match(d$survey_subarea, strata$survey_subarea)]
  N <- ifelse(is.na(cnt), 0, cnt / tl) * area / (0.0149 * 1.852)
  B <- N * mw / 1000
  w <- ifelse(d$sex %in% 1, 0.0005358522 * l10^3.01352,
    ifelse(d$sex %in% 2 & d$maturity_stage %in% c(1, 2, 11), 0.0005358522 * l10^3.01352,
      ifelse(d$sex %in% 2 & d$maturity_stage %in% c(3, 12, 21, 22, 23, 30), 0.0006249431 * l10^2.99411, 0)))
  Ba <- N * w / 1e6
  sB <- ave(B, d$sample_id, FUN = function(x) sum(x, na.rm = TRUE))
  sBa <- ave(Ba, d$sample_id, FUN = function(x) sum(x, na.rm = TRUE))
  Ba <- ifelse(sBa == 0, 0, Ba * sB / sBa)
  p <- ifelse(is.na(l10), 1, ifelse(l10 < 13, 0, ifelse(l10 > 15.5, 1, ramp$p[match(l10, ramp$length)])))
  old <- data.frame(year = d$year, stratum = d$survey_subarea, sample_id = d$sample_id, area = area, b = Ba, b_fishable = Ba * p)
  old <- old[!is.na(old$area), ]
  per_st <- aggregate(cbind(b, b_fishable) ~ year + stratum + sample_id, old, sum)
  per_h <- do.call(rbind, lapply(split(per_st, list(per_st$year, per_st$stratum), drop = TRUE), function(x) {
    data.frame(year = x$year[1], m = mean(x$b_fishable), v = stats::var(x$b_fishable) / nrow(x))
  }))
  per_y <- aggregate(cbind(m, v) ~ year, per_h, sum)

  f <- idx[idx$index == "b_fishable", ]
  ok(ut_cmp_equal(f$value, per_y$m), "Fishable index: sum of sub-area means")
  ok(ut_cmp_equal(f$cv, sqrt(per_y$v) / per_y$m), "CV from the sub-area variances of the mean")
  ok(ut_cmp_equal(f$lower, f$value * exp(-1.96 * f$cv)), "Log-normal interval")
  ok(ut_cmp_equal(f$n_stations, c(6L, 6L)), "The station outside the strata is left out")
  b <- idx[idx$index == "b", ]
  j <- idx[idx$index == "b_juvenile", ]
  ok(ut_cmp_equal(f$value + j$value, b$value), "Fishable + juvenile = total")
  ok(all(bio$b[bio$sample_id == 5] == 0), "Station without shrimp: biomass 0 (counted in the mean)")
  ok(all(bio$b[bio$sample_id == 7] == 0), "Station without a sample weight row: biomass 0")
  s3 <- bio[bio$sample_id == 3, ]
  ok(ut_cmp_equal(sum(s3$b), sum(s3$b_sample)), "Station biomass from the sample weight, split by length-weight")
})

ok_group("pax_station_columns, pax_shrimp_strata, pax_shrimp_towdims", {
  ok(ut_cmp_equal(
    pax:::station_columns_map(c("survey_area", "sample_name")),
    c(survey_area = "skiki", sample_name = "syni_heiti")
  ), "Station columns map to biota columns")
  ok(ut_cmp_error(pax:::station_columns_map("nope"), "Unknown station columns"), "Unknown column is an error")
  ok(ut_cmp_equal(sum(pax_shrimp_strata("arnarfjordur")$area), 176.7), "Arnarfjordur sub-areas")
  ok(ut_cmp_equal(nrow(pax_shrimp_strata(offshore = TRUE)), 42L), "42 offshore rectangles")
  ok(ut_cmp_error(pax_shrimp_strata("nope"), "Unknown shrimp stock"), "Unknown stock is an error")
  td <- pax_shrimp_towdims("nm2")
  ok(ut_cmp_equal(td$std_width[td$sampling_type == 37], 27.595 / 1.852^2 / 1000, tolerance = 1e-4), "Shrimp trawl width as the pax type-37 default (27.595 = 14.9 x 1.852, rounded)")
  ok(ut_cmp_equal(pax_from_mar_extra_tables()[10:12], c("ldist_maturity", "sample_weight", "landings_stock")), "New extra tables")
})
