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
  # The default gear groups only name codes that exist (the join is
  # case-sensitive: 'Var' matched nothing, the code is 'VAR')
  for (fn in c("pax_ldist_alk", "pax_landings_by_gear", "pax_si_scale_by_landings")) {
    codes <- unlist(eval(formals(getFromNamespace(fn, "pax"))$gear_group))
    ok(
      all(codes %in% gm$mfdb_gear_code),
      paste0(fn, "(): default gear groups only name codes in gear_mapping")
    )
  }
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

ok_group("pax_fill_mfdb_gear_code", {
  out <- pax:::ut_tbl(
    pcon,
    data.frame(
      gear_id = c(91, 91, 6, 9999, NA),
      mfdb_gear_code = c(NA, "LLN", NA, NA, "BMT"),
      catch = 1:5
    )
  ) |>
    pax_fill_mfdb_gear_code() |>
    dplyr::arrange(catch) |>
    as.data.frame()
  ok(
    ut_cmp_equal(
      out,
      data.frame(
        gear_id = c(91, 91, 6, 9999, NA),
        mfdb_gear_code = c("GIL", "LLN", "BMT", NA, "BMT"),
        catch = 1:5
      )
    ),
    "Missing codes filled, others unchanged, columns kept"
  )
})
