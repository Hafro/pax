if (!interactive()) {
  options(warn = 2, error = function() {
    sink(stderr())
    traceback(3)
    q(status = 1)
  })
}
library(unittest)

library(pax)

pretend_pax_fn <- function(vals) {
  pax_decorate(data.frame(val = vals), name = "pretend")
}

ok_group("pax_import:cite", {
  pcon <- pax::pax_connect(":memory:")
  pax_import(pcon, pretend_pax_fn("85"))
  pax_import(pcon, pretend_pax_fn("85"), name = "name2", cite = "cite2")
  pax_import(pcon, data.frame(val = 1:100), name = "name3", cite = "cite3")
  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(pax_contents(pcon)),
      data.frame(
        tbl_name = c("name2", "name3", "pretend"),
        citation = c("cite2", "cite3", "pretend_pax_fn(\"85\")")
      )
    ),
    "pax_contents: Use function name by default, overrides worked"
  )
})

ok_group("pax_import:name", {
  pcon <- pax::pax_connect(":memory:")
  lovely_table <- data.frame(val = 1:100)
  pax_import(pcon, lovely_table)
  pax_import(pcon, lovely_table, name = "lovelier_table")

  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(pax_contents(pcon)),
      data.frame(
        tbl_name = c("lovelier_table", "lovely_table"),
        citation = NA_character_
      )
    ),
    "pax_contents: Used variable name when available"
  )
  ok(
    ut_cmp_error(
      pax_import(pcon, data.frame(val = 1:100)),
      "No table name supplied"
    ),
    "pax_import: If we can't derive a name, fall over"
  )
})

ok_group("pax_import:csvread", {
  pcon <- pax::pax_connect(":memory:")
  lovely_table <- data.frame(val = 1:100)
  lovely_csv <- tempfile(fileext = ".csv")
  write.csv(lovely_table, file = lovely_csv, row.names = FALSE)
  pax_import(pcon, lovely_csv, cite = lovely_csv)

  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(pax_contents(pcon)),
      data.frame(
        tbl_name = c("lovely_csv"),
        citation = lovely_csv
      )
    ),
    "pax_contents: Imported CSV, used provided citation"
  )

  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(dplyr::tbl(pcon, "lovely_csv")),
      lovely_table
    ),
    "lovely_csv: Table imported"
  )
})

ok_group("mar_skip_trips", {
  pcon <- pax_connect(":memory:")
  tbl <- pax:::ut_tbl(
    pcon,
    data.frame(trip = c("A1-2022", "MAG1-2022", "MOGUN22", "TB1-2022"))
  )
  ok(
    ut_cmp_equal(
      pax:::mar_skip_trips(tbl, c("MAG%", "MO%")) |> dplyr::pull(trip) |> sort(),
      c("A1-2022", "TB1-2022")
    ),
    "Stomach-sampling trips left out by default patterns"
  )
  ok(
    ut_cmp_equal(nrow(dplyr::collect(pax:::mar_skip_trips(tbl, NULL))), 4L),
    "skip_trips = NULL keeps all trips"
  )
  DBI::dbDisconnect(pcon)
})

ok_group("mar_d2sr_gridcell", {
  # geo::d2sr(), as in 07-bli's bli_d2sr()
  d2sr <- function(lat, lon) {
    lat <- lat + 1e-06
    lon <- -(lon - 1e-06)
    r <- (floor(lat) - 60) * 100 + floor(lon)
    r <- ifelse(lat - floor(lat) > 0.5, r + 50, r)
    r_lat <- (r %/% 100) + 60 + ifelse((r %% 100) >= 50, 0.75, 0.25)
    r_lon <- -((r %% 100) %% 50 + 0.5)
    dlat <- -(lat - r_lat)
    dlon <- -(-lon - r_lon)
    dl <- sign(dlat + 1e-07) + 2 * sign(dlon + 1e-07) + 4
    floor(r * 10 + c(2, 0, 4, 0, 1, 0, 3)[dl])
  }
  set.seed(1)
  pos <- data.frame(
    kastad_breidd = c(64.1, 64.6, 66.25, 63.0, runif(200, 62, 68)),
    kastad_lengd = c(-22.3, -22.8, -18.5, -14.0, runif(200, -30, -10))
  )
  pcon <- pax_connect(":memory:")
  out <- pax:::ut_tbl(pcon, pos) |>
    pax:::mar_d2sr_gridcell() |>
    dplyr::collect()
  ok(
    ut_cmp_equal(out$pos_gridcell, d2sr(out$kastad_breidd, out$kastad_lengd)),
    "Gridcell of a position as geo::d2sr()"
  )
  DBI::dbDisconnect(pcon)
})
