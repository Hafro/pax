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
      regions = list(Other = c(), "10s" = c(NA, 103L), "11s" = c(110:113))
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
      regions = list(Other = c(), "10s" = c(NA, 103L), "11s" = c(110:113))
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
