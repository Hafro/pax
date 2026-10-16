if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

path <- tempfile(fileext = ".duckdb")
pcon <- pax_connect(path)
DBI::dbWriteTable(pcon, "ldist", data.frame(
  sample_id = c("1", "1", "2"), species = c(1, 2, 1), length = c(10, 20, 30), count = 1:3
))
DBI::dbWriteTable(pcon, "station", data.frame(sample_id = c("1", "2"), year = 2000))
DBI::dbWriteTable(pcon, "survey_catch", data.frame(sample_id = "1", species = c(1, 2), count = 1))
DBI::dbDisconnect(pcon)

ok_group("pax_species_subset", {
  pcon <- pax_connect(path, read_only = TRUE)
  cod <- pax_species_subset(pcon, 1)
  ok(ut_cmp_equal(dplyr::collect(dplyr::tbl(cod, "ldist"))$length, c(10, 30)), "ldist: only the species")
  ok(ut_cmp_equal(nrow(dplyr::collect(dplyr::tbl(cod, "station"))), 2L), "station: all rows")
  ok(ut_cmp_equal(nrow(dplyr::collect(dplyr::tbl(cod, "survey_catch"))), 2L), "survey_catch: not filtered")
  DBI::dbWriteTable(cod, "tmp", data.frame(x = 1))
  ok(DBI::dbExistsTable(cod, "tmp"), "Writable (temporary tables of hafroreports)")
  DBI::dbDisconnect(cod)
  ok(ut_cmp_error(pax_species_subset(pax_connect(), 1), "file"), "Needs a database file")
  DBI::dbDisconnect(pcon)
})
