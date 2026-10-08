if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

# Made-up Thünen exports: 2 years, 5 stations, 1 species
ger_dir <- tempfile("ger_")
dir.create(ger_dir)
writeLines(
  c(
    "STATID,REISENR,JAHR,QUARTAL,MONAT,STATION,STATDATUM,NATION,TRANSECTNR,STATIONTYP,HOLTYP,AREA,SUBAREA,DIVISION,STRATUMNR",
    "1,1A,2001,4,10,1,20011001,GFR,-9,GLS,,27,1,1F,6.2",
    "2,1A,2001,4,10,2,20011001,GFR,-9,GLS,,27,1,1F,6.2",
    "3,1A,2001,4,10,3,20011002,GFR,-9,GLS,,27,1,1F,6.2",
    "4,1A,2001,4,10,4,20011002,GFR,-9,GLS,,21,1,1F,3.2",
    "5,2B,2002,4,11,1,20021101,GFR,-9,GLS,,27,1,1F,-9"
  ),
  file.path(ger_dir, "StatFi.csv")
)
writeLines(
  c(
    "NETZID,SCHIFF,JAHR,QUARTAL,MONAT,STATION,DATUM,GBFANGB,GLFANGB,GBHIEV,GLHIEV,GBSTART,GLSTART,GBENDE,GLENDE,DISTANZ,depthmin1,depthmax1,SICHTTIEFE,depthmin2,depthmax2,MITFTIEFE,MITTIEFE,NETZTYP,FANGGERAET,timebeginning,timeend,duration,SCHLRICHT,SCHLGESCHW,SCHLGGRUND",
    "11,HB,2001,4,10,1,20011001,650824N,0335630W,650754N,0340054W,,,,,2,100,110,,-9,-9,-9,0,OTB,BT140,1531,1601,30,250,4.3,-9",
    "12,HB,2001,4,10,2,20011001,640000N,0350000W,640000N,0360000W,,,,,0,200,220,,210,230,-9,0,OTB,BT140,1700,1730,30,250,4.3,-9",
    "13,HB,2001,4,10,3,20011002,623000N,0400000W,623000N,0403000W,,,,,2.5,300,-9,,-9,-9,-9,0,OTB,BT140,800,830,30,250,4.3,-9",
    "14,HB,2001,4,10,4,20011002,62000 N,0500000W,630000N,0500000W,,,,,12,150,160,,-9,-9,-9,0,OTB,BT140,900,930,30,250,4.3,-9",
    "15,HB,2002,4,11,1,20021101,630000N,0380000E,631000N,0381000E,,,,,2,250,260,,-9,-9,-9,0,OTB,BT140,1000,1030,30,250,4.3,-9"
  ),
  file.path(ger_dir, "NetzFi.csv")
)
writeLines(
  c(
    "FISHID,SCHIFF,EU_NR,REISENR,JAHR,QUARTAL,MONAT,STATION,STATDATUM,NETZ,FISH,E_STADIUM,E_STADIUM_NR,ARTCODE,ARTCODETYP,TAXCODE,FARTNAME,REIFEKEY,STOCK_ID,GESAMTKG,GESAMTSTCK",
    "1,HB,HB,1A,2001,4,10,1,20011001,1,703,,,8826010139,,,SEBASTES MARINUS,,,10,40",
    "2,HB,HB,1A,2001,4,10,3,20011002,1,703,,,8826010139,,,SEBASTES MARINUS,,,5,8",
    "3,HB,HB,1A,2001,4,10,4,20011002,1,703,,,8826010139,,,SEBASTES MARINUS,,,-9,-9",
    "4,HB,HB,2B,2002,4,11,1,20021101,1,703,,,8826010139,,,SEBASTES MARINUS,,,2,6",
    "5,HB,HB,1A,2001,4,10,1,20011001,1,704,,,8791031101,,,BROSME BROSME,,,3,1"
  ),
  file.path(ger_dir, "CatchesNorvegicus.csv")
)
writeLines(
  c(
    "LENGID,SCHIFF,EU_NR,REISENR,JAHR,QUARTAL,MONAT,STATION,STATDATUM,NETZ,FISH,E_STADIUM,E_STADIUM_NR,ARTCODE,SEX,STADIUM,REIFEKEY,LAENGE,CARAPAXBREITE,LANZAHL,HANZAHL,GEWICHT,GANZAHL",
    "1,HB,HB,1A,2001,4,10,1,20011001,1,703,,,8826010139,F,UNDEF,,20.5,,3,,-9,-9",
    "2,HB,HB,1A,2001,4,10,1,20011001,1,703,,,8826010139,M,UNDEF,,20.5,,1,,-9,-9",
    "3,HB,HB,1A,2001,4,10,1,20011001,1,703,,,8826010139,M,UNDEF,,30.5,,4,,-9,-9",
    "4,HB,HB,2B,2002,4,11,1,20021101,1,703,,,8826010139,U,UNDEF,,75.5,,2,,-9,-9",
    "5,HB,HB,1A,2001,4,10,1,20011001,1,704,,,8791031101,U,UNDEF,,50.5,,1,,-9,-9"
  ),
  file.path(ger_dir, "LengthFreqNorvegicus.csv")
)
writeLines("strata,id,lat,lon", file.path(ger_dir, "gerarea.csv"))

ger <- pax_ger_survey(ger_dir, species = "SEBASTES MARINUS", species_code = 5)

ok_group("pax_ger_survey", {
  ok(ut_cmp_identical(names(ger), c("ger_station", "ger_catch", "ger_ldist")))
  ok(ut_cmp_identical(
    sapply(ger, attr, "pax_name"),
    c(ger_station = "ger_station", ger_catch = "ger_catch", ger_ldist = "ger_ldist")
  ))

  st <- ger$ger_station
  ok(ut_cmp_equal(st$sample_id, c(2001001, 2001002, 2001003, 2001004, 2002001)))
  ok(ut_cmp_equal(st$year, c(2001L, 2001L, 2001L, 2001L, 2002L)))
  ok(ut_cmp_equal(st$station, c(1L, 2L, 3L, 4L, 1L)))
  ok(ut_cmp_equal(st$haul_id, c(11, 12, 13, 14, 15)))
  ok(ut_cmp_identical(unique(st$sampling_type), 101L))
  ok(ut_cmp_equal(st$ger_area, c(27L, 27L, 27L, 21L, 27L)))
  ok(ut_cmp_equal(st$ger_stratum, c(6.2, 6.2, 6.2, 3.2, NA)), "-9 is NA")
  # DDMMmm: degrees, minutes and hundredths of minutes
  ok(ut_cmp_equal(st$begin_lat[1], 65 + 8.24 / 60))
  ok(ut_cmp_equal(st$begin_lon[1], -(33 + 56.30 / 60)), "W is negative")
  ok(ut_cmp_equal(st$end_lon[5], 38 + 10 / 60), "E as in the file")
  ok(ut_cmp_equal(st$begin_lat[4], 6 + 20 / 60), "Missing digit kept as in the file")
  ok(ut_cmp_equal(st$lat[2], 64))
  ok(ut_cmp_equal(st$lon[2], -35.5))
  # Fishing depth before bottom depth
  ok(ut_cmp_equal(st$tow_depth, c(105, 220, 300, 155, 255)))
  ok(ut_cmp_equal(st$tow_length, c(2, 0, 2.5, 12, 2)), "Distance as in the file")
  ok(ut_cmp_identical(unique(st$mfdb_gear_code), "BMT"))

  ok(ut_cmp_equal(
    as.data.frame(ger$ger_catch),
    data.frame(
      sample_id = c(2001001, 2001003, 2001004, 2002001),
      species = 5,
      catch_count = c(40, 8, NA, 6),
      catch_weight = c(10, 5, NA, 2)
    ),
    check.attributes = FALSE
  ), "Catch of the species only, -9 is NA")

  # Station 2001001: 8 measured, 40 caught, so raised by 5
  ok(ut_cmp_equal(
    as.data.frame(ger$ger_ldist),
    data.frame(
      sample_id = c(2001001, 2001001, 2001001, 2002001),
      species = 5,
      length = c(21, 21, 31, 76),
      sex = c("F", "M", "M", "U"),
      count = c(15, 5, 20, 6),
      count_measured = c(3, 1, 4, 2)
    ),
    check.attributes = FALSE
  ), "Lengths raised to the catch, LAENGE + 0.5")

  ger_all <- pax_ger_survey(ger_dir)
  ok(ut_cmp_identical(
    sort(unique(ger_all$ger_ldist$species)),
    c("8791031101", "8826010139")
  ), "species is ARTCODE without species_code")
  ok(ut_cmp_equal(
    ger_all$ger_ldist$count[ger_all$ger_ldist$species == "8791031101"],
    1
  ))

  ok(ut_cmp_error(
    pax_ger_survey(c(ger_dir, file.path(ger_dir, "StatFi.csv"))),
    "more than one row per station"
  ), "Same table twice")
})

ok_group("pax_ger_station_fix", {
  st <- pax_ger_station_fix(ger$ger_station)
  ok(ut_cmp_equal(st$begin_lat[4], 60 + 200 / 60), "Latitude under 50 times 10")
  ok(ut_cmp_equal(st$end_lon[5], -(38 + 10 / 60)), "All longitudes West")
  ok(ut_cmp_equal(st$lon[5], -(38 + 5 / 60)), "Mid-tow longitude")
  ok(ut_cmp_equal(st$tow_length, c(2, 2.5, 2.5, 2.5, 2)))

  st2 <- pax_ger_station_fix(
    ger$ger_station,
    lon_end_fix = c("2001002" = 1)
  )
  ok(ut_cmp_equal(st2$end_lon[2], -35), "lon_end_fix")

  pcon <- pax_connect(":memory:")
  pax_import(pcon, ger$ger_station)
  st_db <- dplyr::tbl(pcon, "ger_station") |>
    pax_ger_station_fix() |>
    dplyr::arrange(sample_id) |>
    dplyr::collect()
  ok(ut_cmp_equal(st_db$lat, st$lat), "Same in DB")
  ok(ut_cmp_equal(st_db$lon, st$lon), "Same in DB")
  ok(ut_cmp_equal(st_db$tow_length, st$tow_length), "Same in DB")
  DBI::dbDisconnect(pcon, shutdown = TRUE)
})

ok_group("pax_ger_ldist_impute", {
  ldist <- data.frame(
    sample_id = c(1, 1, 2, 2),
    species = 5,
    length = c(20, 30, 20, 40),
    sex = c("F", "M", "U", "U"),
    count = c(2, 2, 6, 6),
    count_measured = c(1, 1, 3, 3)
  )
  station <- data.frame(
    sample_id = c(1, 2, 3, 4, 5),
    year = c(2001, 2001, 2001, 2001, 2002),
    ger_stratum = c(6.2, 6.2, 6.2, NA, 6.2)
  )
  catch <- data.frame(
    sample_id = c(1, 2, 3, 4, 5),
    species = 5,
    catch_count = c(4, 12, 10, 3, 7)
  )
  # Station 3: mean count_measured by length 20: 2, 30: 1, 40: 3, raised to 10
  imputed_3 <- data.frame(
    sample_id = 3,
    species = 5,
    length = c(20, 30, 40),
    sex = NA_character_,
    count = c(2, 1, 3) * 10 / 6,
    count_measured = c(2, 1, 3)
  )
  out <- pax_ger_ldist_impute(ldist, station, catch)
  ok(ut_cmp_equal(
    pax:::ut_as_sort_df(out),
    pax:::ut_as_sort_df(rbind(ldist, imputed_3))
  ), "No stratum (4) or no lengths in stratum and year (5): not imputed")

  out <- pax_ger_ldist_impute(ldist, station, catch, sample_ids = 4)
  ok(ut_cmp_equal(pax:::ut_as_sort_df(out), pax:::ut_as_sort_df(ldist)))

  out <- pax_ger_ldist_impute(
    ldist,
    station,
    catch,
    sample_ids = data.frame(sample_id = 5, year = 2001, ger_stratum = 6.2)
  )
  imputed_5 <- imputed_3
  imputed_5$sample_id <- 5
  imputed_5$count <- c(2, 1, 3) * 7 / 6
  ok(ut_cmp_equal(
    pax:::ut_as_sort_df(out),
    pax:::ut_as_sort_df(rbind(ldist, imputed_5))
  ), "Lengths from another year")

  pcon <- pax_connect(":memory:")
  out <- pax_ger_ldist_impute(
    pax:::ut_tbl(pcon, ldist),
    pax:::ut_tbl(pcon, station),
    pax:::ut_tbl(pcon, catch),
    sample_ids = data.frame(sample_id = c(3, 5), year = 2001, ger_stratum = 6.2)
  ) |>
    dplyr::collect()
  ok(ut_cmp_equal(
    pax:::ut_as_sort_df(out),
    pax:::ut_as_sort_df(rbind(ldist, imputed_3, imputed_5))
  ), "Same in DB")
  DBI::dbDisconnect(pcon, shutdown = TRUE)
})

ok_group("pax_ger_strata", {
  strata <- pax_ger_strata()
  ok(ut_cmp_identical(attr(strata, "pax_name"), "ger_strata"))
  ok(ut_cmp_equal(strata$rall_area, 24331.73))
  ok(ut_cmp_equal(
    as.numeric(sf::st_bbox(strata)),
    c(-44, 61, -26, 66)
  ))
})

ok_group("East Greenland index with pax_si_*", {
  pcon <- pax_connect(":memory:")
  for (t in ger) pax_import(pcon, t)
  pax_import(pcon, pax_ger_strata(area = 1.852^2 * 1000))

  ger_station <- dplyr::tbl(pcon, "ger_station") |>
    pax_ger_station_fix() |>
    dplyr::filter(
      lon > -44,
      lat > 61,
      lat < 66,
      year > 1983,
      is.na(ger_stratum) | ger_stratum != 3.2
    ) |>
    dplyr::mutate(tow_length = 2.2 * 41 / 1852)
  ger_ldist <- dplyr::tbl(pcon, "ger_ldist") |>
    pax_ger_ldist_impute(
      station = dplyr::tbl(pcon, "ger_station"),
      catch = dplyr::tbl(pcon, "ger_catch")
    ) |>
    dplyr::filter(length < 72) |>
    pax_ldist_add_weight(data.frame(species = 5, a = 0.0109, b = 3.07))
  by_sample <- ger_station |>
    pax_si_by_length(ldist = ger_ldist) |>
    pax_si_scale_by_strata(
      "ger_strata",
      strata_stations = dplyr::tbl(pcon, "ger_station") |>
        dplyr::distinct(station, sampling_type) |>
        dplyr::mutate(stratum = 1L)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(sample_id, year, length, si_abund, si_biomass) |>
    dplyr::arrange(sample_id, length) |>
    dplyr::collect() |>
    as.data.frame()

  # 2001: stations 1, 2 (zero), 3 (imputed from station 1: 21 cm 2, 31 cm
  # 4 measured, raised to 8); 4 is West. 2002: station 1, all >= 72 cm
  tow_area <- 2.2 * 41 / 1852
  n_2001 <- c(20, 20, 0, 8 * 2 / 6, 8 * 4 / 6) / tow_area / 1000 * 1000 / 3
  ok(ut_cmp_equal(
    by_sample,
    data.frame(
      sample_id = c(2001001, 2001001, 2001002, 2001003, 2001003, 2002001),
      year = c(2001L, 2001L, 2001L, 2001L, 2001L, 2002L),
      length = c(21, 31, 0, 21, 31, 0),
      si_abund = c(n_2001, 0),
      si_biomass = c(n_2001 * 0.0109 * c(21, 31, 0, 21, 31)^3.07 / 1000, 0)
    )
  ), "Area-weighted numbers and biomass per station and length")
  DBI::dbDisconnect(pcon, shutdown = TRUE)
})
