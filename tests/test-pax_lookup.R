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

ok_group("gear_mapping", {
  gm <- pax_temptbl(pcon, "paxdat_gear_mapping") |> as.data.frame()
  ok(!anyDuplicated(gm$gear_id), "One row per gear code")
  ok(!anyNA(gm$mfdb_gear_code), "Every gear code has an MFDB gear code")
  ok(
    ut_cmp_equal(as.character(gm[gm$gear_id == 91, "mfdb_gear_code"]), "GIL"),
    "Gear 91 (anglerfish gillnet) is GIL"
  )
  ok(
    ut_cmp_equal(as.character(gm[gm$gear_id == 91, "source"]), "pax"),
    "Gear 91 is marked as added by pax"
  )
  ok(
    ut_cmp_equal(
      as.character(gm$mfdb_gear_code),
      as.character(gm$mfdb_gear_code_weight)
    ),
    "With gear 91, ops$bthe.gear_mapping (weights) is the same mapping"
  )
})

ok_group("reitmapping", {
  e <- new.env()
  utils::data("reitmapping", "gridcell", package = "pax", envir = e)
  ok(!anyDuplicated(e$reitmapping$id), "One row per id")
  sub <- e$reitmapping |>
    dplyr::filter(
      !is.na(gridcell),
      !is.na(division),
      !is.na(subdivision),
      !is.na(lat),
      !is.na(lon)
    ) |>
    dplyr::select(-id)
  num_sorted <- function(df) {
    df <- lapply(df, function(x) as.numeric(as.character(x)))
    as.data.frame(df)[order(df$gridcell), ]
  }
  ok(
    ut_cmp_equal(
      num_sorted(sub),
      num_sorted(e$gridcell),
      check.attributes = FALSE
    ),
    "gridcell dataset is the subset of reitmapping with positions"
  )
})
