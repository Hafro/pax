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

ok_group("pax_ldist_scale_tow_area", {
  ldist <- pax:::ut_tbl(
    pcon,
    data.frame(
      sample_id = 1:6,
      species = 6,
      gear_id = 91,
      sampling_type = c(34, 34, 34, 30, 30, 30),
      tow_length = c(NA, 0, 0.4, NA, 0, 10),
      count = 100
    )
  )
  out <- pax_ldist_scale_tow_area(ldist) |>
    dplyr::arrange(sample_id) |>
    dplyr::collect()
  ok(
    ut_cmp_equal(out$count[1:3], rep(100 / (0.5 * 50), 3)),
    "Gillnet survey: every net is 0.5 nm, whatever tow_length says"
  )
  ok(
    ut_cmp_equal(out$count[4:6], 100 / (c(1, 1, 8) * 17 / 1852)),
    "Groundfish survey: missing or 0 tow length is 1, long tows clamped"
  )
})

ok_group("pax_ldist_by_year: ldist is already raised, not raised again", {
  # pax_mar_ldist() raises the counts with mar::skala_med_taldir(), so the
  # ldist table holds 2 x the measured fish when as many were counted
  pcon <- pax_connect(":memory:")
  pax_import(
    pcon,
    data.frame(
      sample_id = c("1", "2"),
      year = 2019,
      sampling_type = 30,
      mfdb_gear_code = "BMT",
      begin_lat = 64,
      begin_lon = -22,
      tow_length = 4
    ),
    name = "station"
  )
  pax_import(
    pcon,
    data.frame(
      sample_id = c("1", "1", "2"),
      species = 1,
      length = c(40.2, 50, 50),
      sex = 1,
      count = c(20, 20, 10)
    ),
    name = "ldist"
  )
  pax_import(
    pcon,
    data.frame(
      sample_id = c("1", "1", "2"),
      species = 1,
      measurement_type = c("LEN", "CNT", "LEN"),
      count = c(20, 20, 10)
    ),
    name = "measurement"
  )
  pax_import(
    pcon,
    data.frame(species = 1, a = 0.01, b = 3),
    name = "lw_coeffs"
  )
  out <- dplyr::tbl(pcon, "station") |>
    pax_ldist_by_year() |>
    dplyr::arrange(length) |>
    dplyr::collect()
  ok(
    ut_cmp_equal(out$length, c(40, 50)),
    "Lengths rounded"
  )
  ok(
    ut_cmp_equal(out$n, c(20, 30)),
    "Counts summed as in ldist, sample 1 not raised a second time"
  )

  loc <- dplyr::tbl(pcon, "station") |>
    pax_station_location_summary() |>
    dplyr::arrange(sample_id) |>
    dplyr::collect()
  ok(
    ut_cmp_equal(
      loc$bio,
      c(20 * 0.01 * 40.2^3 + 20 * 0.01 * 50^3, 10 * 0.01 * 50^3) / 4 / 1e3
    ),
    "pax_station_location_summary: kg/nm from the ldist counts and weights"
  )
  DBI::dbDisconnect(pcon)
})

ok_group("pax_ldist_plot: data.frame and database table alike", {
  pcon <- pax_connect(":memory:")
  ld <- data.frame(
    year = rep(c(2000, 2001), each = 3),
    length = rep(c(10, 20, 30), 2),
    n = c(1, 2, 1, 3, 1, 0)
  )
  p_df <- ggplot2::ggplot_build(pax_ldist_plot(ld))$data[[1]]
  p_db <- ggplot2::ggplot_build(pax_ldist_plot(pax:::ut_tbl(pcon, ld)))$data[[1]]
  ok(
    ut_cmp_equal(
      p_df[order(p_df$PANEL, p_df$x), "y"],
      c(0.25, 0.5, 0.25, 0.75, 0.25, 0)
    ),
    "data.frame: proportions by length within year"
  )
  ok(
    ut_cmp_equal(
      p_db[order(p_db$PANEL, p_db$x), "y"],
      p_df[order(p_df$PANEL, p_df$x), "y"]
    ),
    "Database table: the same"
  )
  p_df <- ggplot2::ggplot_build(pax_ldist_plot(ld, scale = 0, expand = TRUE))$data[[1]]
  ok(
    ut_cmp_equal(p_df[order(p_df$PANEL, p_df$x), "y"], ld$n),
    "scale = 0: counts, expand works on a data.frame"
  )
  DBI::dbDisconnect(pcon)
})
