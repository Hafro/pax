if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

ok_group("pax_annotator_id: stable pseudonym, OPS$ prefix and case ignored", {
  id <- pax_annotator_id(c("OPS$ABC", "ABC", "ops$abc", " abc", "OPS$XYZ", NA))
  ok(ut_cmp_identical(length(unique(id[1:4])), 1L), "OPS$ABC = ABC = ops$abc")
  ok(id[1] != id[5], "different users, different ids")
  ok(is.na(id[6]), "NA stays NA")
  ok(all(nchar(id[1:5]) == 10), "10 hex digits")
  ok(ut_cmp_identical(
    id[1],
    substr(digest::digest("ABC", algo = "sha256", serialize = FALSE), 1, 10)
  ), "SHA-256 of the user name without OPS$")
  ok(!any(grepl("ABC|XYZ", id, ignore.case = TRUE)), "no name in the id")
})

ok_group("video_annotation_tidy: types, year, no user names, comments or paths", {
  # As collected from HAFMYND.VIDEO_TAXON_CONCEPT_V (numbers as text)
  raw <- data.frame(
    ID = c("1", "2", "3"),
    VIDEO_ID = "480",
    FILE_PATH = "/2019/B7-2019/x.mpg",
    VIDEO_NAME = "x.mpg",
    PROJECT_ID = "21",
    PROJECT_NAME = "Humar",
    PROJECT_NUMBER = NA,
    CRUISE_ID = "402",
    CRUISE_NAME = c("B7-2019", "B7-2019", "Þ5-2025"),
    STATION_NUMBER = c("493", "493", "199"),
    REAL_TIME = as.POSIXct("2019-06-09 00:51:24", tz = "UTC"),
    VIDEO_TIMESTAMP = 136069,
    DURATION = NA,
    CONFIDENCE = "2",
    INDIVIDUALS = NA,
    COVER = NA,
    RANKING = "3",
    TAXON_ID = c(NA, "5", NA),
    TAXON_NAME = c(NA, "Nephrops norvegicus", NA),
    TAXON_RANK = NA,
    ACCEPTED_TAXON_ID = NA,
    ACCEPTED_SCIENTIFIC_NAME = NA,
    ACCEPTED_TAXON_RANK = NA,
    CONCEPT_ID = "186",
    CONCEPT_NAME = c("Hole", "Hole_Neph", "Trollfar"),
    SEABED_TYPE_ID = NA,
    SEABED_TYPE_NAME = NA,
    SCALE_MARKER_CM = NA,
    COMMENTS = "seen by Jon",
    COORDINATES = "POINT(0.7786 0.0723)",
    FINAL = c("1", "0", "1"),
    INSERT_USER = c("OPS$ABC", "ABC", "OPS$XYZ"),
    stringsAsFactors = FALSE
  )
  out <- pax:::video_annotation_tidy(raw)
  ok(ut_cmp_identical(out$station_number, c(493, 493, 199)), "station number numeric")
  ok(ut_cmp_identical(out$year, c(2019, 2019, 2025)), "year from the cruise name")
  ok(ut_cmp_identical(out$final, c(1L, 0L, 1L)), "final integer")
  ok(ut_cmp_identical(out$annotator[1], out$annotator[2]), "same user, same id")
  ok(ut_cmp_identical(out$annotator, pax_annotator_id(raw$INSERT_USER)), "pseudonymous ids")
  ok(!any(c("insert_user", "comments", "file_path", "video_name") %in% names(out)), "no user name, comments or paths")
  flat <- unlist(lapply(out, as.character))
  ok(!any(grepl("ABC|XYZ|Jon", flat)), "no names anywhere in the table")
})

ok_group("pax_nephrops_grounds", {
  g <- pax_nephrops_grounds()
  ok(ut_cmp_identical(nrow(g), 13L), "13 grounds")
  ok(ut_cmp_identical(g$name, as.character(1:13)), "ground numbers 1-13")
  ok(ut_cmp_identical(sf::st_crs(g)$epsg, 3057L), "ISN93")
  ok(ut_cmp_equal(sum(as.numeric(sf::st_area(g))) / 1e6, 6587.52, tolerance = 1e-6), "6588 km2 in total (the table rounds by area)")
  ok(ut_cmp_identical(sf::st_crs(pax_nephrops_grounds(4326))$epsg, 4326L), "transformed")
  ok(ut_cmp_identical(
    sort(unique(g$area_name)),
    sort(c("Breiðamerkurdjúp", "Eldey", "Grindavík", "Hornafjarðardjúp",
      "Jökuldjúp", "Lónsdjúp", "Selvogur", "Skeiðarárdjúp", "Vestmannaeyjasvæði"))
  ), "nine areas")
})
