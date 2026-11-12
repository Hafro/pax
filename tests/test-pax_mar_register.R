if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

ok_group("mar_ices_area_of", {
  ok(
    ut_cmp_identical(
      pax:::mar_ices_area_of(c("I", "P", "G", "O", "H", "X", "R", "U", NA)),
      c("5a", "5a", "14b", "5b", "2", "1", "12", NA, NA)
    ),
    "Fishing areas of the register as kvoti_veidisv()"
  )
})

ok_group("mar_fishing_year_of, mar_period_of_month", {
  ok(
    ut_cmp_identical(
      pax:::mar_fishing_year_of(
        c(1990, 1991, 1991, 2024, 2024),
        c(10, 8, 9, 8, 9)
      ),
      c("1990", "1991", "1991/1992", "2023/2024", "2024/2025")
    ),
    "Calendar years before September 1991, then September-August"
  )
  ok(
    ut_cmp_identical(
      pax:::mar_period_of_month(c(2024, 2024), c(8, 9)),
      c("20232024", "20242025")
    ),
    "Quota period as lods_oslaegt() timabil"
  )
})

ok_group("mar_landings_gear_id", {
  g <- function(...) pax:::mar_landings_gear_id(...)
  ok(
    ut_cmp_identical(
      g(c("agf", "agf", "agf"), c(12, 6, 13), NA, c(1, 1, 1)),
      c(1, 6, 1)
    ),
    "AGF longline, bottom trawl and land-baited longline to the old codes"
  )
  ok(
    ut_cmp_identical(
      g("agf", c(15, 15), NA, c(43, 191)),
      c(15, 40)
    ),
    "AGF dredge: scallop dredge, sea urchin dredge by species"
  )
  ok(
    ut_cmp_identical(
      g("lods", c(11, 11, 13), c("Dragnót 135 mm", "Dragnót 120 mm", "Landbeitt lína"), 1),
      c(5, 26, 71)
    ),
    "Landings register 1992-2007 by gear name"
  )
  ok(
    ut_cmp_identical(
      g("fiskifelag", c(2, 9), c("Net", "Spærlingsvarpa"), 1),
      c(2, 8)
    ),
    "Fisheries Association, by source or by the name of the register"
  )
})

ok_group("Lookup datasets", {
  for (n in c("agf_fishing_area", "agf_stock", "landings_gear")) {
    env <- new.env()
    utils::data(list = n, package = "pax", envir = env)
    ok(is.data.frame(env[[n]]) && nrow(env[[n]]) > 0, paste(n, "is there"))
  }
  env <- new.env()
  utils::data("agf_fishing_area", "agf_stock", package = "pax", envir = env)
  ok(
    ut_cmp_identical(
      env$agf_fishing_area$fishing_area[env$agf_fishing_area$agf_area == "IS"],
      "I"
    ),
    "AGF IS is the register's I"
  )
  ok(
    ut_cmp_identical(
      env$agf_stock$stock_code[match(c(4101, 4103), env$agf_stock$stock)],
      c(51L, 53L)
    ),
    "Arnarfjordur and Isafjardardjup shrimp stocks"
  )
})
