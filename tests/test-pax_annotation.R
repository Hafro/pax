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

ok_group("pax_pseudonym", {
  p <- pax_pseudonym(c(1178, "1178", NA))
  ok(ut_cmp_identical(p[1], p[2]), "number and text give the same pseudonym")
  ok(is.na(p[3]), "NA stays NA")
  ok(ut_cmp_identical(p[1], substr(digest::digest("1178", algo = "sha256", serialize = FALSE), 1, 10)), "SHA-256")
  ok(ut_cmp_identical(pax_annotator_id("OPS$ABC"), pax_pseudonym("ABC")), "annotator id is the pseudonym of the normalised name")
})

ok_group("logbook_old and landings_port options", {
  ok(all(c("tow_time", "tow_length", "num_traps", "trap_hours", "n_same_catch") %in% pax_logbook_old_columns()), "effort columns")
  ok("logbook_adb" %in% pax_from_mar_extra_tables(), "logbook_adb extra table")
  ok("landings_port" %in% pax_from_mar_extra_tables(), "landings_port extra table")
  ok(ut_cmp_error(pax_mar_logbook_old(NULL, 191, columns = "nonsense"), "Unknown logbook_old columns"), "unknown column stops")
})

ok_group("pax_d2sr as geo::d2sr", {
  set.seed(2)
  lat <- c(65.10233, 65.0925, 65.25, 65.5, 64.99, runif(200, 63, 67))
  lon <- c(-21.206, -22.704, -22.5, -23, -24, runif(200, -27, -12))
  ref <- floor(geo_like <- if (requireNamespace("geo", quietly = TRUE)) geo::d2sr(lat, lon) else rep(NA, length(lat)))
  if (all(is.na(ref))) {
    ok(TRUE, "geo not installed, skipped")
  } else {
    ok(ut_cmp_identical(pax_d2sr(lat, lon), ref), "same as geo::d2sr()")
  }
  ok(ut_cmp_identical(pax_d2sr(65.1, -22.6), 5223), "a position in Breidafjordur (geo::d2sr gives 5223)")
})
