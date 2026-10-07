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

ok_group("pax_add_groupings", {
  out <- pax:::ut_tbl(pcon, expand.grid(length = 1:100)) |> pax_add_groupings()
  ut_cmp_equal(
    pax:::ut_as_sort_df(out),
    data.frame(
      length = 1:100,
      lgroup = rep(seq(0, 95, 5), each = 5),
      stringsAsFactors = FALSE
    ),
    "pax_add_groupings: Use default length grouping, ignored everything else not in data.frame"
  )
})

ok_group("pax_add_lgroups", {
  # TODO: Resolve https://github.com/Hafro/pax/issues/11 before thorough tests
})

ok_group("pax_add_regions", {
  # Choose some random gridcells
  #data("gridcell", package = "pax")
  #gridcell[sample(nrow(gridcell), 5), ]

  out <- pax:::ut_tbl(
    pcon,
    data.frame(
      gridcell = c(99L, 6322L, 6684L, 7133L, 7653L, 8044L),
      #division = c(NA, 110L, 103L, 113L, 111L, 115L),
      stringsAsFactors = FALSE
    )
  ) |>
    pax_add_regions()
  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(out),
      data.frame(
        gridcell = c(99L, 6322L, 6684L, 7133L, 7653L, 8044L),
        division = c(NA, 110L, 103L, 113L, 111L, 115L),
        subdivision = c(NA, 1101L, 1032L, 1133L, 1112L, 1151L),
        region = c(NA, "all", "all", "all", "all", "all"),
        stringsAsFactors = FALSE
      )
    ),
    "pax_add_regions: Default grouping is 'all'"
  )

  out <- pax:::ut_tbl(
    pcon,
    data.frame(
      gridcell = c(99L, 6322L, 6684L, 7133L, 7653L, 8044L),
      #division = c(NA, 110L, 103L, 113L, 111L, 115L),
      stringsAsFactors = FALSE
    )
  ) |>
    pax_add_regions(
      regions = list(
        Other = pax_add_other(),
        "10s" = c(NA, 103L),
        "11s" = c(110:113)
      )
    )
  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(out),
      data.frame(
        gridcell = c(99L, 6322L, 6684L, 7133L, 7653L, 8044L),
        division = c(NA, 110L, 103L, 113L, 111L, 115L),
        subdivision = c(NA, 1101L, 1032L, 1133L, 1112L, 1151L),
        region = c("Other", "11s", "10s", "11s", "11s", "Other"),
        stringsAsFactors = FALSE
      )
    ),
    "pax_add_regions: 115 -> Other, NA -> Other"
  )

  out <- pax:::ut_tbl(
    pcon,
    data.frame(
      gridcell = c(99L, 6322L, 6684L, 7133L, 7653L, 8044L),
      division = 110L,
      stringsAsFactors = FALSE
    )
  ) |>
    pax_add_regions(
      regions = list(
        Other = pax_add_other(),
        "10s" = c(NA, 103L),
        "11s" = c(110:113)
      )
    )
  ok(
    ut_cmp_equal(
      pax:::ut_as_sort_df(out),
      data.frame(
        gridcell = c(99L, 6322L, 6684L, 7133L, 7653L, 8044L),
        division = 110L,
        region = "11s",
        stringsAsFactors = FALSE
      )
    ),
    "pax_add_regions: In-table division overrode gridcell"
  )
})

ok_group("pax_add_ocean_depth_class: missing depths from the record's own cells", {
  pcon <- pax_connect(":memory:")
  pax_import(
    pcon,
    data.frame(
      lat = c(64.0, 66.5, 63.0),
      lon = c(-22.0, -18.0, -14.5),
      ocean_depth = c(50, 250, 800)
    ),
    name = "ocean_depth"
  )
  logbook <- pax:::ut_tbl(
    pcon,
    data.frame(
      id = 1:5,
      lat = c(64.0, 66.5, 63.0, 63.0, NA),
      lon = c(-22.0, -18.0, -14.5, -14.5, NA),
      ocean_depth = c(NA, NA, NA, 120, NA)
    )
  )
  out <- logbook |>
    pax_add_ocean_depth_class(breaks = c(0, 100, 200, 500)) |>
    dplyr::arrange(id) |>
    dplyr::collect()
  ok(
    ut_cmp_equal(out$ocean_depth, c(50, 250, 800, 120, NA)),
    "Missing depths filled from each record's own cell, reported depth kept"
  )
  ok(
    ut_cmp_equal(
      out$ocean_depth_class,
      c("0-100", "200-500", "500+", "100-200", "Unknown")
    ),
    "Depth classes, no position is Unknown"
  )
  DBI::dbDisconnect(pcon)
})
