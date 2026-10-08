if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

# NB: Made-up data only, no foreign data in tests
dir <- tempfile("pax_foreign")
dir.create(dir)

ok_group("pax_foreign_mar_tables", {
  tables <- pax_foreign_mar_tables()
  ok(!anyDuplicated(tables$name), "Unique pax table names")
  ok(all(grepl("^[a-z0-9_]+$", tables$name)), "Plain lower-case names")
  ok(
    ut_cmp_error(pax_mar_foreign(NULL, "badger"), "Unknown foreign table"),
    "Unknown table"
  )
})

ok_group("pax_foreign_file: greenland_logbooks", {
  path <- file.path(dir, "logbooks.csv")
  writeLines(
    c(
      '"code","year","gear","time1","time2","country","area","area_detail","catch_t","trawltime_h","lon","lat","eez_grl","eez_ice"',
      '"BLI",2020,"OTB",2020-03-02,"2020-03-02 10:30","GRL","ices14b","ices14b2",1.5,3,-35.5,64.2,1,0',
      '"GHL",2021,"LL",2021-11-30 21:15,"2021-11-30 23:00","GRL","ices14b","ices14b2",0.25,NA,-36,65,1,0'
    ),
    path
  )
  out <- pax_foreign_file(path, "greenland_logbooks")
  ok(ut_cmp_identical(attr(out, "pax_name"), "greenland_logbooks"), "Table name")
  ok(ut_cmp_equal(out$code, c("BLI", "GHL")), "Columns as in the file")
  ok(
    ut_cmp_equal(
      out$time1,
      as.POSIXct(c("2020-03-02 00:00", "2021-11-30 21:15"), tz = "UTC")
    ),
    "time1 is a date-time, with or without the time"
  )
  ok(
    ut_cmp_equal(
      out$time2,
      as.POSIXct(c("2020-03-02 10:30", "2021-11-30 23:00"), tz = "UTC")
    ),
    "time2 is a date-time"
  )
  ok(ut_cmp_equal(out$catch_t, c(1.5, 0.25)), "catch_t")

  pcon <- pax_connect(":memory:")
  pax_import(pcon, out)
  ok(
    ut_cmp_equal(
      dplyr::tbl(pcon, "greenland_logbooks") |> dplyr::pull(catch_t) |> sort(),
      c(0.25, 1.5)
    ),
    "Imported into a pax database"
  )
  DBI::dbDisconnect(pcon)
})

ok_group("pax_foreign_file: greenland_catch", {
  path <- file.path(dir, "catch.csv")
  writeLines(
    c(
      "year,month,area,catch_tot",
      '1999,1,"NE, Dohrns Bank",4.2e-5',
      '1999,2,"SE, Deep",3e-4'
    ),
    path
  )
  out <- pax_foreign_file(path, "greenland_catch")
  ok(ut_cmp_identical(attr(out, "pax_name"), "greenland_catch"), "Table name")
  ok(
    ut_cmp_equal(
      out,
      data.frame(
        year = 1999L,
        month = 1:2,
        area = c("NE, Dohrns Bank", "SE, Deep"),
        catch_tot = c(4.2e-5, 3e-4)
      ),
      check.attributes = FALSE
    ),
    "Read as in the file"
  )
})

ok_group("pax_foreign_file: faroese_logbooks (csv)", {
  path <- file.path(dir, "logbook_2025.csv")
  writeLines(
    c(
      "LOGBOOKINFO_TYPE,VORN_ID,YEAR,MONTH,SPECIES_CODE,WEIGHT_KG,WEIGHT_TONNES,GEARSHOT_DATETIME,GEARSHOT_DEPTH,GEARSHOT_LATITUDE,GEARSHOT_LONGITUDE",
      "LINAGARNASNELLA,1,2025,1,COD,100,0.1,04-JAN-25 08.20.00,474,62.95,-11.45",
      "BOTNVORP,2,2025,12,COD,250,0.25,31-DEC-25 23.59.00,300,63.5,-10.25"
    ),
    path
  )
  out <- pax_foreign_file(path, "faroese_logbooks")
  ok(ut_cmp_identical(attr(out, "pax_name"), "faroese_logbooks"), "Table name")
  ok(ut_cmp_equal(out$year, c(2025L, 2025L)), "year from the date")
  ok(ut_cmp_equal(out$month, c(1L, 12L)), "month from the date")
  ok(ut_cmp_equal(out$lat, c(62.95, 63.5)), "lat")
  ok(ut_cmp_equal(out$lon, c(-11.45, -10.25)), "lon, west negative")
  ok(ut_cmp_equal(out$logbook_type, c("LongLine", "Trawl")), "logbook_type")
  ok(ut_cmp_equal(out$catch, c(100, 250)), "catch (kg)")
})

ok_group("foreign_ddmm", {
  ok(ut_cmp_equal(pax:::foreign_ddmm(6330), 63.5), "63 degrees 30 minutes")
  ok(ut_cmp_equal(pax:::foreign_ddmm(1015), 10.25), "10 degrees 15 minutes")
})

ok_group("pax_foreign_file: missing file", {
  ok(
    ut_cmp_error(
      pax_foreign_file(file.path(dir, "badger.csv"), "greenland_catch"),
      "not found"
    ),
    "Missing file is an error"
  )
  ok(
    ut_cmp_error(
      pax_foreign_file(file.path(dir, "catch.csv"), "badger"),
      "should be one of"
    ),
    "Unknown type is an error"
  )
})

if (requireNamespace("mar", quietly = TRUE)) {
  ok_group("pax_from_mar: optional table arguments are checked first", {
    ok(
      ut_cmp_error(pax_from_mar(1, extra_tables = "badger"), "Unknown extra_tables"),
      "Unknown extra table"
    )
    ok(
      ut_cmp_error(pax_from_mar(1, foreign_tables = "badger"), "Unknown foreign_tables"),
      "Unknown foreign table"
    )
    ok(
      ut_cmp_error(
        pax_from_mar(1, foreign_files = list(badger = "x.csv")),
        "Unknown foreign_files"
      ),
      "Unknown foreign file type"
    )
    ok(
      ut_cmp_error(
        pax_from_mar(1, foreign_files = list(greenland_catch = file.path(dir, "badger.csv"))),
        "not found"
      ),
      "Missing foreign file"
    )
    ok(
      ut_cmp_error(
        pax_from_mar(1, ger_survey_path = file.path(dir, "badger")),
        "not found"
      ),
      "Missing German survey path"
    )
  })
}

unlink(dir, recursive = TRUE)
